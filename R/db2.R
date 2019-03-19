## This reads in the yaml version of the schema and turns it into an
## object ready for use.

## This can be broken down further but it's all pretty ugly.  This is
## the sort of thing where I really miss a within-package
## namespace/module feature so that implementation details can be
## hidden away a bit further.

ORDERLY_SCHEMA_VERSION <- "0.0.6"

## These will be used in a few places and even though they're not
## super likely to change it would be good
ORDERLY_SCHEMA_TABLE <- "orderly_schema"
ORDERLY_MAIN_TABLE <- "report_version"
ORDERLY_TABLE_LIST <- "orderly_schema_tables"

report_db2_schema_read <- function(fields = NULL, dialect = "sqlite") {
  d <- yaml_read(orderly_file("database/schema.yml"))

  preprepare <- function(nm) {
    x <- d[[nm]]
    x$name <- nm
    x$columns <- unlist(x$columns, FALSE)
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
        el$fk_sql_alter <-
          sprintf(
            'ALTER TABLE "%s" ADD FOREIGN KEY ("%s") REFERENCES "%s" ("%s");',
            x$name, nm, el$fk_table, el$fk_column)
        el$fk_sql_create <-
          sprintf('FOREIGN KEY ("%s") REFERENCES "%s" ("%s")',
                  nm, el$fk_table, el$fk_column)
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
      type[type == "SERIAL"] <- "INTEGER"
    }
    type[[1]] <- paste(type[[1]], "PRIMARY KEY")
    null[[1]] <- ""

    ## TODO: other column types do not enforce uniqueness, which is
    ## not great; it should be enough to pop "PRIMARY KEY" after them
    ## I think.  Then we can in general drop the null generation from
    ## all first columns
    cols <- sprintf('"%s" %s%s', names(x$columns), type, null)
    fks <-
      list_to_character(drop_null(lapply(x$columns, "[[", "fk_sql_create")),
                        FALSE)
    table_elements <- c(cols, if (dialect == "sqlite") fks)
    x$sql_create <- sprintf('CREATE TABLE "%s" (\n%s\n);',
                            x$name, paste(table_elements, collapse = ",\n"))
    x$sql_fk <- vcapply(drop_null(lapply(x$columns, "[[", "fk_sql_alter")),
                        identity, USE.NAMES = FALSE)
    if (!is.null(x$values)) {
      x$values <- do.call(
        rbind, lapply(x$values, as.data.frame, stringsAsFactors = FALSE))
    } else if (x$name == "artefact_format") {
      x$values <- data_frame(name = valid_formats())
    } else if (x$name == "orderly_schema") {
      x$values <- data_frame(
        schema_version = ORDERLY_SCHEMA_VERSION,
        orderly_version = as.character(utils::packageVersion("orderly")),
        created = Sys.time())
    } else if (x$name == "orderly_schema_tables") {
      x$values <- data_frame(name = names(d))
    }
    x
  }

  tables <- lapply(d, prepare_table)
  sql <- vcapply(tables, "[[", "sql_create", USE.NAMES = FALSE)
  if (dialect == "postgres") {
    sql <- c(sql, unlist(lapply(tables, "[[", "sql_fk"), FALSE, FALSE))
  }
  values <- drop_null(lapply(tables, "[[", "values"))

  list(tables = tables,
       sql = sql,
       values = values)
}


report_db2_schema <- function(fields = NULL, dialect = "sqlite") {
  key <- hash_object(list(fields, dialect))
  if (is.null(cache$schema[[key]])) {
    cache$schema[[key]] <- report_db2_schema_read(fields, dialect)
  }
  cache$schema[[key]]
}


## Same pattern as existing db.R version but with
report_db2_init <- function(con, config, must_create = FALSE, validate = TRUE) {
  sqlite_pragma_fk(con, TRUE)

  if (!DBI::dbExistsTable(con, ORDERLY_SCHEMA_TABLE)) {
    report_db2_init_create(con, config, report_db2_dialect(con))
  } else if (must_create) {
    stop(sprintf("Table '%s' already exists", ORDERLY_SCHEMA_TABLE))
  } else if (validate) {
    report_db2_open_existing(con, config)
  }
}


report_db2_init_create <- function(con, config, dialect) {
  dat <- report_db2_schema(config$fields, dialect)
  dat$values$changelog_label <- config$changelog

  DBI::dbBegin(con)
  on.exit(DBI::dbRollback(con))
  for (s in dat$sql) {
    DBI::dbExecute(con, s)
  }
  for (nm in names(dat$values)) {
    DBI::dbWriteTable(con, nm, dat$values[[nm]], append = TRUE)
  }

  DBI::dbCommit(con)
  on.exit()
}


report_db2_open_existing <- function(con, config) {
  sql <- sprintf("SELECT * FROM %s LIMIT 0", ORDERLY_MAIN_TABLE)
  d <- DBI::dbGetQuery(con, sql)
  custom_name <- config$fields$name
  msg <- setdiff(custom_name, names(d))
  if (length(msg) > 0L) {
    stop(sprintf("custom fields %s not present in existing database",
                 paste(squote(msg), collapse = ", ")))
  }

  schema <- report_db2_schema(config$fields, report_db2_dialect(con))
  cols <- names(schema$tables[[ORDERLY_MAIN_TABLE]]$columns)
  extra <- setdiff(names(d), cols)
  if (length(extra) > 0L) {
    stop(sprintf("custom fields %s in database not present in config",
                 paste(squote(extra), collapse = ", ")))
  }

  if (!DBI::dbExistsTable(con, "changelog_label")) {
    ## This DB needs rebuilding, but this at least will give a
    ## sensible error message:
    label <- data_frame(label = character(0), public = logical(0))
  } else {
    label <- DBI::dbReadTable(con, "changelog_label")
    label$public <- as.logical(label$public)
  }
  ok <- setequal(label$id, config$changelog$id) &&
    identical(label$public[match(label$id, config$changelog$id)],
              config$changelog$public %||% logical(0))
  if (!ok) {
    stop("changelog labels have changed: rebuild with orderly::orderly_rebuild",
         call. = FALSE)
  }

  d <- DBI::dbReadTable(con, ORDERLY_SCHEMA_TABLE)
  if (numeric_version(d$schema_version) <
      numeric_version(ORDERLY_SCHEMA_VERSION)) {
    ## with this approach we can't ever upgrade, but at least we throw
    stop("orderly db needs rebuilding with orderly::orderly_rebuild()",
         call. = FALSE)
  }
}


report_db2_rebuild <- function(config, verbose = TRUE) {
  assert_is(config, "orderly_config")
  root <- config$path
  con <- orderly_db("destination", config, validate = FALSE)
  on.exit(DBI::dbDisconnect(con))

  if (DBI::dbExistsTable(con, ORDERLY_TABLE_LIST)) {
    report_db2_destroy(con, config)
  }
  report_db2_init(con, config)
  reports <- unlist(lapply(list_dirs(path_archive(root)), list_dirs))
  if (length(reports) > 0L) {
    for (p in reports[order(basename(reports))]) {
      if (verbose) {
        message(sprintf("%s (%s)", basename(p), basename(dirname(p))))
      }
      report_data_import(con, p, config)
    }
  }

  if (!is.null(config$changelog)) {
    sql <- paste("SELECT distinct report_version.report",
                 "  FROM changelog",
                 "  JOIN report_version",
                 "    ON report_version.id = changelog.report_version")
    reports <- DBI::dbGetQuery(con, sql)[[1]]
    for (r in reports) {
      report_db2_update_changelog_published(con, r)
    }
  }
}


report_db2_needs_rebuild <- function(config) {
  con <- orderly_db("destination", config, FALSE, FALSE)
  on.exit(DBI::dbDisconnect(con))

  d <- DBI::dbReadTable(con, ORDERLY_SCHEMA_TABLE)
  numeric_version(d$schema_version) < numeric_version(ORDERLY_SCHEMA_VERSION)
}


report_data_import <- function(con, workdir, config) {
  dat_rds <- readRDS(path_orderly_run_rds(workdir))
  dat_in <- recipe_read(workdir, config, FALSE)
  published <- report_is_published(workdir)
  changelog <- changelog_read_json(workdir)

  ## Was not done before 0.3.3
  stopifnot(!is.null(dat_rds$meta))
  ## Was not done before 0.5.5
  stopifnot(!is.null(dat_rds$meta$connection))

  id <- dat_rds$meta$id
  name <- dat_rds$meta$name

  sql_name <- "SELECT name FROM report WHERE name = $1"
  if (nrow(DBI::dbGetQuery(con, sql_name, name)) == 0L) {
    DBI::dbWriteTable(con, "report", data_frame(name = name), append = TRUE)
  } else {
    sql <- "SELECT id FROM report_version WHERE report = $1"
    prev <- max(DBI::dbGetQuery(con, sql, name)$id)
    if (id < prev) {
      stop(sprintf(
        "Report id '%s' is behind existing id '%s'", id, prev),
        call. = FALSE)
    }
  }

  if (is.null(dat_rds$git$sha)) {
    git_clean <- NA
  } else {
    git_clean <- is.null(dat_rds$git$status)
  }

  report_version <- data_frame(
    id = id,
    report = dat_rds$meta$name,
    date = dat_rds$meta$date,
    displayname = dat_in$displayname %||% NA_character_,
    description = dat_in$description %||% NA_character_,
    published = published,
    connection = dat_rds$meta$connection,
    git_sha = dat_rds$git$sha %||% NA_character_,
    git_branch = dat_rds$git$branch %||% NA_character_,
    git_clean = git_clean)

  if (nrow(config$fields) > 0L) {
    extra <- drop_null(set_names(
      lapply(config$fields$name, function(x) dat_in[[x]]),
      config$fields$name))
    if (length(extra) > 0L) {
      report_version <- cbind(report_version, as_data_frame(extra))
    }
  }
  DBI::dbWriteTable(con, "report_version", report_version, append = TRUE)

  if (!is.null(dat_in$views)) {
    report_version_view <- data_frame(
      report_version = id,
      name = names(dat_in$views),
      sql = unname(dat_in$views))
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
      sql = unname(dat_in$data),
      hash = unname(hash_data))
    DBI::dbWriteTable(con, "report_version_data", report_version_data,
                      append = TRUE)
  }

  if (length(dat_in$packages) > 0L) {
    r_version <-
      paste(dat_rds$session_info$R.version[c("major", "minor")], collapse = ".")
    pkgs_base <-
      set_names(rep(r_version, length(dat_rds$session_info$basePkgs)),
                dat_rds$session_info$basePkgs)
    pkgs_other <- vcapply(unlist(
      unname(dat_rds$session_info[c("otherPkgs", "loadedOnly")]), FALSE),
      "[[", "Version")
    report_version_package <- data_frame(
      report_version = rep(id, length(dat_in$packages)),
      package_name = dat_in$packages,
      package_version = c(pkgs_base, pkgs_other)[dat_in$packages])
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

  ## there might be a better way to this.
  ## At the moment we are repeatedly checking if a readme has been copied
  ## maybe add a field to the run info saying that we've copied a README?
  if (file.exists(file.path(workdir, "README.md"))) {
    readme_name <- "README.md"
  } else {
    readme_name <- NULL
  }

  ## NOTE: the hash from 'sources' comes from the resources field.
  file_in <- list(resource = names(dat_rds$meta$hash_resources),
                  global = names(dat_rds$meta$hash_global),
                  script = dat_in$script,
                  readme = readme_name,
                  orderly_yml = "orderly.yml")
  file_in_name <- unlist(file_in, FALSE, FALSE)

  file_in_hash <- c(list_to_character(dat_rds$meta$hash_resources, FALSE),
                    list_to_character(dat_rds$meta$hash_global, FALSE),
                    dat_rds$meta$hash_script,
                    list_to_character(dat_rds$meta$hash_readme, FALSE),
                    hash_orderly_yml)
  file_in_purpose <- rep(names(file_in), lengths(file_in))
  file_in_purpose[file_in_name %in% dat_in$sources] <- "source"

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
    file_purpose = file_in_purpose)
  DBI::dbWriteTable(con, "file_input", file_input, append = TRUE)

  ## Then artefacts
  report_version_artefact <- data_frame(
    report_version = id,
    format = list_to_character(dat_in$artefacts[, "format"], FALSE),
    description = list_to_character(dat_in$artefacts[, "description"], FALSE),
    order = seq_len(nrow(dat_in$artefacts)))

  DBI::dbWriteTable(con, "report_version_artefact", report_version_artefact,
                    append = TRUE)
  sql <- paste("SELECT id FROM report_version_artefact",
               "WHERE report_version = $1 ORDER BY 'order'")
  tmp <- DBI::dbGetQuery(con, sql, id)

  ## We need to do some faffage here:
  n <- lengths(dat_in$artefacts[, "filenames"])
  i <- rep(seq_along(n), n)

  artefact_files <- unlist(dat_in$artefacts[, "filenames"], use.names = FALSE)
  artefact_hash <-
    list_to_character(dat_rds$meta$hash_artefacts[artefact_files], FALSE)
  report_data_add_files(con, artefact_hash, artefact_files, workdir)

  file_artefact <- data_frame(
    artefact = tmp$id[i],
    file_hash = artefact_hash,
    filename = artefact_files)
  DBI::dbWriteTable(con, "file_artefact", file_artefact, append = TRUE)

  if (!is.null(changelog)) {
    changelog <- changelog[changelog$report_version == id, , drop = FALSE]
    if (nrow(changelog) > 0L) {
      DBI::dbWriteTable(con, "changelog", changelog, append = TRUE)
    }
  }

  if (!is.null(dat_rds$meta$parameters)) {
    p <- dat_rds$meta$parameters
    parameters <- data_frame(
      report_version = id,
      name = names(p),
      type = report_db2_parameter_type(p),
      value = report_db2_parameter_serialise(p))
    DBI::dbWriteTable(con, "parameters", parameters, append = TRUE)
  }

  sql <- "UPDATE report SET latest = $1 WHERE name = $2"
  DBI::dbExecute(con, sql, list(id, name))
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


report_data_find_dependencies <- function(con, meta) {
  if (is.null(meta$depends) || nrow(meta$depends) == 0L) {
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
  ## Was not done before 0.5.0
  stopifnot(all(!is.na(depends_use)))

  data_frame(
    report_version = meta$id,
    use = depends_use,
    as = meta$depends$as,
    is_latest = meta$depends$is_latest,
    is_pinned = meta$depends$is_pinned)
}


report_db2_destroy <- function(con, config) {
  dialect <- report_db2_dialect(con)
  schema <- names(report_db2_schema(config$fields, dialect)$tables)
  existing <- DBI::dbListTables(con)
  known <- DBI::dbReadTable(con, ORDERLY_TABLE_LIST)[[1L]]
  drop <- intersect(known, existing)
  extra <- setdiff(intersect(schema, existing), drop)

  if (length(extra)) {
    msg <- c("While rebuilding the orderly database, we will delete",
             sprintf("additional tables: %s.",
                     paste(squote(extra), collapse = ", ")),
             "This is most likely an orderly bug - please request that",
             "the orderly schema version is increased")
    warning(paste(msg, collapse = " "),
            immediate. = TRUE, call. = FALSE)
    drop <- c(drop, extra)
  }

  if (length(drop) > 0L) {
    sqlite_pragma_fk(con, FALSE)
    on.exit(sqlite_pragma_fk(con, TRUE))
    for (t in drop) {
      DBI::dbRemoveTable(con, t)
    }
  }
}


report_db2_publish <- function(con, id, name, value) {
  sql <- "UPDATE report_version SET published = $1 WHERE id = $2"
  DBI::dbExecute(con, sql, list(value, id))
  report_db2_update_changelog_published(con, name)
}


report_db2_update_changelog_published <- function(con, name) {
  sql <- "SELECT id FROM report_version WHERE report = $1 and published"
  published <- DBI::dbGetQuery(con, sql, name)$id

  sql <- paste("SELECT",
               "  changelog.id, report_version, report_version_public,",
               "    published",
               "  FROM changelog",
               "  JOIN report_version",
               "    ON report_version.id = changelog.report_version",
               "  JOIN changelog_label",
               "    ON changelog_label.id = changelog.label",
               " WHERE report_version.report = $1",
               "   AND changelog_label.public",
               " ORDER BY report_version")
  dat <- DBI::dbGetQuery(con, sql, list(name))
  dat$published <- dat$published == 1

  p <- rep(NA_character_, nrow(dat))
  for (i in seq_len(nrow(dat))) {
    j <- dat$report_version[[i]] <= published
    if (!any(j)) {
      break
    }
    p[[i]] <- published[[min(which(j))]]
  }

  prev <- dat$report_version_public
  new <- p
  ## Replace NAs with empty strings for ease of the next comparison
  prev[is.na(prev)] <- ""
  new[is.na(new)] <- ""

  sql <- "UPDATE changelog SET report_version_public = $1 WHERE id = $2"
  for (k in which(new != prev)) {
    DBI::dbExecute(con, sql, list(p[[k]], dat$id[[k]]))
  }
}


sqlite_pragma_fk <- function(con, enable = TRUE) {
  if (report_db2_dialect(con) == "sqlite") {
    DBI::dbExecute(con, sprintf("PRAGMA foreign_keys = %d", enable))
  }
}


report_db2_parameter_type <- function(x) {
  vcapply(x, function(el) {
    if (is.character(el)) {
      "text"
    } else if (is.numeric(el)) {
      "number"
    } else if (is.logical(el)) {
      "boolean"
    } else {
      stop("Unsupported parameter type")
    }
  }, USE.NAMES = FALSE)
}


report_db2_parameter_serialise <- function(x) {
  vcapply(x, function(el) {
    if (is.character(el)) {
      el
    } else if (is.numeric(el)) {
      as.character(el)
    } else if (is.logical(el)) {
      tolower(as.character(el))
    } else {
      stop("Unsupported parameter type")
    }
  }, USE.NAMES = FALSE)
}


report_db2_dialect <- function(con) {
  if (inherits(con, "SQLiteConnection")) {
    "sqlite"
  } else if (inherits(con, "PqConnection")) {
    "postgres"
  } else {
    ## It's possible that RPostgreSQL might work, but it's unlikely to
    ## throw all the errors that we need.
    stop("Can't determine SQL dialect")
  }
}
