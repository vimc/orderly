## This reads in the yaml version of the schema and turns it into an
## object ready for use.

## This can be broken down further but it's all pretty ugly.  This is
## the sort of thing where I really miss a within-package
## namespace/module feature so that implementation details can be
## hidden away a bit further.

ORDERLY_SCHEMA_VERSION <- "0.0.1"

## These will be used in a few places and even though they're not
## super likely to change it would be good
ORDERLY_SCHEMA_TABLE <- "orderly_schema"
ORDERLY_MAIN_TABLE <- "report_version"
ORDERY_TABLE_LIST <- "orderly_schema_tables"

orderly_schema_prepare <- function(fields = NULL, dialect = "sqlite") {
  d <- yaml_read(orderly_file("database/schema.yml"))

  preprepare <- function(nm) {
    x <- d[[nm]]
    x$name <- nm
    x$columns <- unlist(x$columns, FALSE)
    ## x$values <- unlist(x$values, FALSE)
    x
  }

  d <- set_names(lapply(names(d), preprepare), names(d))

  if (!is.null(fields)) {
    f <- set_names(Map(function(t, n) list(type = t, nullable = n),
                       fields$type, !fields$required),
                   fields$name)
    d[[ORDERLY_MAIN_TABLE]]$columns <- c(d[[ORDERLY_MAIN_TABLE]]$columns, f)
  }

  prepare_table <- function(x) {
    prepare_col <- function(nm) {
      el <- x$columns[[nm]]
      el$name <- nm
      if (!is.null(el$fk)) {
        fk <- strsplit(el$fk, ".", fixed = TRUE)[[1L]]
        el$fk_table <- fk[[1L]]
        el$fk_column <- fk[[2L]]
        el$fk_sql <-
          sprintf(
            'ALTER TABLE "%s" ADD FOREIGN KEY ("%s") REFERENCES "%s" ("%s")',
            x$name, nm, el$fk_table, el$fk_column)
        el$type <- d[[el$fk_table]]$columns[[el$fk_column]]$type
        if (el$type == "SERIAL") {
          el$type <- "INTEGER"
        }
      }
      el
    }

    x$columns[] <- lapply(names(x$columns), prepare_col)

    is_nullable <- vlapply(x$columns, function(x) isTRUE(x$nullable))
    type <- vcapply(x$columns, "[[", "type")
    null <- unname(ifelse(is_nullable | type == "SERIAL", "", " NOT NULL"))
    if (dialect == "sqlite") {
      type[type == "SERIAL"] <- "INTEGER PRIMARY KEY"
    }
    ## TODO: other column types do not enforce uniqueness, which is
    ## not great; it should be enough to pop "PRIMARY KEY" after them
    ## I think.  Then we can in general drop the null generation from
    ## all first columns
    cols <- paste(sprintf('"%s" %s%s', names(x$columns), type, null),
                  collapse = ", ")

    x$sql_create <- paste0(sprintf('CREATE TABLE "%s" (', x$name), cols, ");")
    x$sql_fk <- vcapply(drop_null(lapply(x$columns, "[[", "fk_sql")),
                        identity, USE.NAMES = FALSE)
    if (!is.null(x$values)) {
      x$values <- do.call(
        rbind, lapply(x$values, as.data.frame, stringsAsFactors = FALSE))
    } else if (x$name == "artefact_format") {
      x$values <- data_frame(name = valid_formats())
    } else if (x$name == "orderly_schema") {
      x$values <- data_frame(
        schema_version = ORDERLY_SCHEMA_VERSION,
        orderly_version = as.character(packageVersion("orderly")),
        created = Sys.time())
    } else if (x$name == "orderly_schema_tables") {
      x$values <- data_frame(name = names(d))
    }
    x
  }

  tables <- lapply(d, prepare_table)

  sql <- vcapply(tables, "[[", "sql_create")
  if (dialect == "postgres") {
    sql <- c(sql, unlist(lapply(tables, "[[", "sql_fk"), FALSE, FALSE))
  }
  values <- drop_null(lapply(tables, "[[", "values"))
  list(tables = tables,
       sql = sql,
       values = values)
}


## Same pattern as existing db.R version but with
report_db2_init <- function(con, config, must_create = FALSE) {
  if (!DBI::dbExistsTable(con, ORDERLY_SCHEMA_TABLE)) {
    dialect <- "sqlite" # TODO: get from config
    dat <- orderly_schema_prepare(config$fields, dialect)

    withCallingHandlers({
      DBI::dbBegin(con)
      for (s in dat$sql) {
        DBI::dbExecute(con, s)
      }
      for (nm in names(dat$values)) {
        DBI::dbWriteTable(con, nm, dat$values[[nm]], append = TRUE)
      }
      DBI::dbCommit(con)
    }, error = function(e) DBI::dbRollback(con))
  } else if (must_create) {
    stop(sprintf("Table '%s' already exists", orderly_table))
  } else {
    sql <- sprintf("SELECT * FROM %s LIMIT 0", ORDERLY_MAIN_TABLE)
    d <- DBI::dbGetQuery(con, sql)
    custom_name <- config$fields$name
    msg <- setdiff(custom_name, names(d))
    if (length(msg) > 0L) {
      stop(sprintf("custom fields %s not present in existing database",
                   paste(squote(msg), collapse = ", ")))
    }

    ## TODO: this should be dealt with in a more sustainable way; the
    ## report_db_cols() function duplicates information already
    ## present in the yml and that should be the source of truth here
    ## but we shuold only load that once in a session.
    extra <- setdiff(setdiff(names(d), names(report_db_cols())),
                     c("report", custom_name))
    if (length(extra) > 0L) {
      stop(sprintf("custom fields %s in database not present in config",
                   paste(squote(extra), collapse = ", ")))
    }

    ## TODO: check the schema version here but probably not until we
    ## know how to deal with changes!
  }
}


report_db2_rebuild <- function(config, verbose = TRUE) {
  assert_is(config, "orderly_config")
  root <- config$path
  con <- orderly_db("destination", config)
  on.exit(DBI::dbDisconnect(con))

  if (DBI::dbExistsTable(con, ORDERY_TABLE_LIST)) {
    withCallingHandlers({
      DBI::dbBegin(con)
      report_db2_destroy(con)
      DBI::dbCommit(con)
    }, error = function(e) DBI::dbRollback(con))
  }
  report_db2_init(con, config)
  reports <- unlist(lapply(list_dirs(path_archive(root)), list_dirs))
  for (p in reports[order(basename(reports))]) {
    if (verbose) {
      message(basename(p))
    }
    report_data_import(con, p, config)
  }
}


report_data_import <- function(con, workdir, config) {
  ## Bunch of places where data we need is scattered over:
  dat_rds <- readRDS(path_orderly_run_rds(workdir))
  dat_yml <- yaml_read(path_orderly_run_yml(workdir))
  dat_in1 <- yaml_read(file.path(workdir, "orderly.yml"))
  dat_in2 <- recipe_read(workdir, config)

  if (is.null(dat_rds$meta)) {
    ## VIMC-1958
    dat_rds$meta <- dat_yml
    if (!is.null(dat_in2$depends)) {
      dat_rds$meta$depends <- dat_in2$depends
      dat_rds$meta$depends$id <- vcapply(dat_yml$depends, "[[", "id")
    }
  }

  id <- dat_rds$meta$id
  name <- dat_rds$meta$name

  sql_name <- "SELECT name FROM report WHERE name = $1"
  if (nrow(DBI::dbGetQuery(con, sql_name, name)) == 0L) {
    DBI::dbWriteTable(con, "report", data_frame(name = name), append = TRUE)
  }

  report_version <- data_frame(
    id = id,
    report = dat_rds$meta$name,
    date = dat_rds$meta$date,
    displayname = dat_rds$meta$displayname %||% NA_character_,
    description = dat_rds$meta$description %||% NA_character_)
  if (!is.null(config$fields)) {
    extra <- set_names(lapply(config$fields$name, function(x) dat_in2[[x]]),
                       config$fields$name)
    report_version <- cbind(report_version, as_data_frame(drop_null(extra)))
  }
  DBI::dbWriteTable(con, "report_version", report_version, append = TRUE)

  if (!is.null(dat_in2$views)) {
    report_version_view <- data_frame(
      report_version = id,
      name = names(dat_in2$views),
      sql = unname(dat_in2$views))
    DBI::dbWriteTable(con, "report_version_view", report_version_view,
                      append = TRUE)
  }

  ## Then see if the data is known:
  hash_data <- list_to_character(dat_rds$meta$hash_data)
  if (length(hash_data) > 0L) {
    sql_data <- sprintf("SELECT hash FROM data WHERE hash IN (%s)",
                        paste(dquote(hash_data), collapse = ", "))
    msg <- setdiff(hash_data, DBI::dbGetQuery(con, sql_data)$hash)
    if (length(msg)) {
      data <- data_frame(
        hash = msg,
        size_csv = file.size(orderly_db("csv", config)$filename(msg)),
        size_rds = file.size(orderly_db("rds", config)$filename(msg)))
      DBI::dbWriteTable(con, "data", data, append = TRUE)
    }

    report_version_data <- data_frame(
      report_version = id,
      name = names(hash_data),
      sql = unname(dat_in2$data),
      hash = unname(hash_data))
    DBI::dbWriteTable(con, "report_version_data", report_version_data,
                      append = TRUE)
  }

  if (length(dat_in2$packages) > 0L) {
    r_version <-
      paste(dat_rds$session_info$R.version[c("major", "minor")], collapse = ".")
    pkgs_base <-
      set_names(rep(r_version, length(dat_rds$session_info$basePkgs)),
                dat_rds$session_info$basePkgs)
    pkgs_other <- vcapply(unlist(
      unname(dat_rds$session_info[c("otherPkgs", "loadedOnly")]), FALSE),
      "[[", "Version")
    report_version_package <- data_frame(
      report_version = rep(id, length(dat_in2$packages)),
      package_name = dat_in2$packages,
      package_version = c(pkgs_base, pkgs_other)[dat_in2$packages])
    rownames(report_version_package) <- NULL
    DBI::dbWriteTable(con, "report_version_package", report_version_package,
                      append = TRUE)
  }

  depends <- report_data_find_dependencies(con, dat_rds$meta)
  if (!is.null(depends)) {
    DBI::dbWriteTable(con, "depends", depends, append = TRUE)
  }

  ## TODO: patch this back in for the saved rds I think
  hash_orderly_yml <- hash_files(file.path(workdir, "orderly.yml"), FALSE)

  ## A hash for sources seems to be potentially missing!
  ## dat_rds$meta$hash_sources
  file_in <- list(resource = dat_in2$resources,
                  script = dat_in2$script,
                  orderly_yml = "orderly.yml")
  file_in_name <- unlist(file_in, FALSE, FALSE)
  file_in_hash <- c(list_to_character(dat_rds$meta$hash_resources, FALSE),
                    dat_rds$meta$hash_script,
                    hash_orderly_yml)
  file_in_use <- rep(names(file_in), lengths(file_in))
  file_in_use[file_in_name %in% dat_in2$sources] <- "source"

  ## These might be missing:
  sql <- sprintf("SELECT hash from file WHERE hash IN (%s)",
                 paste(dquote(unique(file_in_hash)), collapse = ", "))
  hash_msg <- setdiff(file_in_hash, DBI::dbGetQuery(con, sql)$hash)
  i <- file_in_hash %in% hash_msg & !duplicated(file_in_hash)
  if (any(i)) {
    file <- data_frame(hash = file_in_hash[i],
                       size = file.size(file.path(workdir, file_in_name[i])))
    DBI::dbWriteTable(con, "file", file, append = TRUE)
  }

  file_input <- data_frame(
    report_version = id,
    file_hash = file_in_hash,
    filename = file_in_name,
    file_use = file_in_use)
  DBI::dbWriteTable(con, "file_input", file_input, append = TRUE)

  ## Then artefacts
  report_version_artefact <- data_frame(
    report_version = id,
    format = list_to_character(dat_in2$artefacts[, "format"], FALSE),
    description = list_to_character(dat_in2$artefacts[, "description"], FALSE),
    order = seq_len(nrow(dat_in2$artefacts)))

  DBI::dbWriteTable(con, "report_version_artefact", report_version_artefact,
                    append = TRUE)
  sql <- paste("SELECT id FROM report_version_artefact",
               "WHERE report_version = $1 ORDER BY 'order'")
  tmp <- DBI::dbGetQuery(con, sql, id)

  ## We need to do some faffage here:
  n <- lengths(dat_in2$artefacts[, "filenames"])
  i <- rep(seq_along(n), n)

  ## Not certain this will cope with shiny outputs?  Or other
  ## directory outputs (are there any?)
  artefact_files <- unlist(dat_in2$artefacts[, "filenames"], use.names = FALSE)
  artefact_hash <-
    list_to_character(dat_rds$meta$hash_artefacts[artefact_files], FALSE)
  report_data_add_files(con, artefact_hash, artefact_files, workdir)

  file_artefact <- data_frame(
    artefact = tmp$id[i],
    file_hash = artefact_hash,
    filename = artefact_files)
  DBI::dbWriteTable(con, "file_artefact", file_artefact, append = TRUE)
}


report_data_add_files <- function(con, hashes, paths, workdir) {
  sql <- sprintf("SELECT hash from file WHERE hash IN (%s)",
                 paste(dquote(unique(hashes)), collapse = ", "))
  hash_msg <- setdiff(hashes, DBI::dbGetQuery(con, sql)$hash)
  i <- hashes %in% hash_msg & !duplicated(hashes)
  if (any(i)) {
    file <- data_frame(hash = hashes[i],
                       size = file.size(file.path(workdir, paths[i])))
    DBI::dbWriteTable(con, "file", file, append = TRUE)
  }
}


## This section is far too complicated and will get rationalised soon.
## It's mostly hard because there's a problem with a previous report
## (just one: native-2015-with-herd-effects/20171101-103805-a636d323)
## that has a dependency on a file that is dragged in from a previous
## dependency.
##
## There are also issues in native-201710-diagnostics-burden with
## dependencies on resources.
report_data_find_dependencies <- function(con, meta) {
  if (is.null(meta$depends)) {
    return(NULL)
  }

  sql_depends <- paste(
    "SELECT file_artefact.id",
    "  FROM file_artefact JOIN report_version_artefact",
    "    ON file_artefact.artefact = report_version_artefact.id",
    " WHERE report_version_artefact.report_version = $1",
    "   AND file_artefact.filename = $2")
  find_depends <- function(id, filename) {
    res <- DBI::dbGetQuery(con, sql_depends, list(id, filename))$id
    if (length(res) == 0L) NA_integer_ else res
  }
  depends_use <- list_to_integer(
    Map(find_depends, meta$depends$id, meta$depends$filename,
        USE.NAMES = FALSE))

  i <- is.na(depends_use)
  if (any(i)) {
    ## try and find this in the parent.
    message(sprintf("Uncaught dependency problem: %s/%s", meta$name, meta$id))
    ## What we really want to see is the contents of "depends" for
    ## that previous report
    sql_real_depends <- paste(
      "SELECT use",
      "  FROM depends",
      "  JOIN file_artefact",
      "    ON file_artefact.id = depends.use",
      ' WHERE report_version = $1 AND "as" = $2')
    find_real_depends <- function(id, filename) {
      res <- DBI::dbGetQuery(con, sql_real_depends, list(id, filename))$use
      if (length(res) == 0L) NA_integer_ else res
    }
    depends_use[i] <- list_to_integer(
      Map(find_real_depends, meta$depends$id[i],
          meta$depends$filename[i], USE.NAMES = FALSE))
  }

  depends <- data_frame(
    report_version = meta$id,
    use = depends_use,
    as = meta$depends$as)

  i <- is.na(depends_use)
  if (any(i)) {
    message(sprintf("...discarding %d unrecoverable dependencies", sum(i)))
    if (all(i)) {
      return(NULL)
    }
    depends <- depends[!i, ]
  }

  depends
}

report_db2_destroy <- function(con) {
  if (DBI::dbExistsTable(con, ORDERY_TABLE_LIST)) {
    for (t in DBI::dbReadTable(con, ORDERY_TABLE_LIST)[[1L]]) {
      DBI::dbRemoveTable(con, t)
    }
  }
}
