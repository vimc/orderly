## Low-level db functions

## This reads in the yaml version of the schema and turns it into an
## object ready for use.

## This can be broken down further but it's all pretty ugly.  This is
## the sort of thing where I really miss a within-package
## namespace/module feature so that implementation details can be
## hidden away a bit further.

orderly_schema_version <- "1.3.0"
orderly_schema_table <- "orderly_schema"
orderly_table_list <- "orderly_schema_tables"

report_db_schema_read <- function(fields = NULL, dialect = "sqlite") {
  d <- yaml_read(orderly_file("database/schema.yml"))

  preprepare <- function(nm) {
    x <- d[[nm]]
    x$name <- nm
    x$columns <- unlist(x$columns, FALSE)
    x
  }

  d <- set_names(lapply(names(d), preprepare), names(d))

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
        schema_version = orderly_schema_version,
        orderly_version = as.character(utils::packageVersion("orderly1")),
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
  if (!is.null(fields)) {
    values$custom_fields <- data_frame(
      id = fields$name,
      description = fields$description)
  }

  list(tables = tables,
       sql = sql,
       values = values)
}


report_db_schema <- function(fields = NULL, dialect = "sqlite") {
  key <- hash_object(list(fields, dialect))
  if (is.null(cache$schema[[key]])) {
    cache$schema[[key]] <- report_db_schema_read(fields, dialect)
  }
  cache$schema[[key]]
}


## Same pattern as existing db.R version but with
report_db_init <- function(con, config, must_create = FALSE, validate = TRUE) {
  sqlite_pragma_fk(con, TRUE)

  if (!DBI::dbExistsTable(con, orderly_schema_table)) {
    report_db_init_create(con, config, report_db_dialect(con))
  } else if (must_create) {
    stop(sprintf("Table '%s' already exists", orderly_schema_table))
  } else if (validate) {
    report_db_open_existing(con, config)
  }
}


report_db_init_create <- function(con, config, dialect) {
  dat <- report_db_schema(config$fields, dialect)
  dat$values$changelog_label <- config$changelog
  dat$values$tag <- data_frame(id = config$tags)

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


report_db_open_existing <- function(con, config) {
  version_db <- DBI::dbReadTable(con, orderly_schema_table)$schema_version
  version_package <- orderly_schema_version
  if (numeric_version(version_db) < numeric_version(version_package)) {
    stop("orderly db needs rebuilding with orderly1::orderly_rebuild()",
         call. = FALSE)
  }

  custom_db <- DBI::dbReadTable(con, "custom_fields")$id
  custom_config <- config$fields$name
  custom_msg <- setdiff(custom_config, custom_db)
  if (length(custom_msg) > 0L) {
    stop(sprintf("custom fields %s not present in existing database",
                 paste(squote(custom_msg), collapse = ", ")))
  }
  custom_extra <- setdiff(custom_db, custom_config)
  if (length(custom_extra) > 0L) {
    stop(sprintf("custom fields %s in database not present in config",
                 paste(squote(custom_extra), collapse = ", ")))
  }

  label <- DBI::dbReadTable(con, "changelog_label")
  label$public <- as.logical(label$public)
  ok <- setequal(label$id, config$changelog$id) &&
    identical(label$public[match(label$id, config$changelog$id)],
              config$changelog$public %||% logical(0))
  if (!ok) {
    stop(
      "changelog labels have changed: rebuild with orderly1::orderly_rebuild()",
      call. = FALSE)
  }

  tag <- DBI::dbReadTable(con, "tag")$id
  ok <- setequal(tag, config$tags)
  if (!ok) {
    stop(
      "tags have changed: rebuild with orderly1::orderly_rebuild()",
      call. = FALSE)
  }
}


report_db_import <- function(name, id, config, timeout = 10, metadata = FALSE) {
  if (metadata) {
    orderly_log("import", sprintf("%s:%s (metadata only)", name, id))
  } else {
    orderly_log("import", sprintf("%s:%s", name, id))
  }
  con <- orderly_db("destination", config)
  on.exit(DBI::dbDisconnect(con))

  if (!metadata) {
    prev <- DBI::dbGetQuery(con, "SELECT * from report_version WHERE id = $1",
                            id)
    if (nrow(prev) > 0) {
      ## It seems there is a chance here that something bad could
      ## happen (e.g., a migration occurs which means that the
      ## imported metadata is out of date) leaving everything a bit
      ## inconsistent. This would be detectable by comparing the
      ## contents of the two rds files
      ## (archive/:name/:id/orderly_run.rds vs
      ## metadata/:name/:id). However, with no recovery mechanism I
      ## think we're better off ignoring this for now...
      return(invisible())
    }
  }

  meta <- read_metadata(name, id, config, metadata)

  ## sqlite busy handler expects milliseconds
  RSQLite::sqliteSetBusyHandler(con, timeout * 1000)
  DBI::dbExecute(con, "BEGIN IMMEDIATE")
  report_data_import(con, meta, config)
  DBI::dbCommit(con)
}


report_db_rebuild <- function(config, verbose = TRUE) {
  assert_is(config, "orderly_config")
  root <- config$root
  con <- orderly_db("destination", config, validate = FALSE)
  on.exit(DBI::dbDisconnect(con))

  if (DBI::dbExistsTable(con, orderly_table_list)) {
    report_db_destroy(con, config)
  }
  report_db_init(con, config)

  reports_archive <- orderly_list_archive(config, FALSE)
  reports_metadata <- orderly_list_metadata(config, FALSE)
  metadata <- rep(c(FALSE, TRUE),
                  c(nrow(reports_archive), nrow(reports_metadata)))
  reports <- cbind(rbind(reports_archive, reports_metadata), metadata)
  for (i in order(reports$id)) {
    id <- reports$id[[i]]
    name <- reports$name[[i]]
    metadata <- reports$metadata[[i]]
    if (verbose) {
      fmt <- if (metadata) "%s (%s) (metadata only)" else "%s (%s)"
      message(sprintf(fmt, id, name))
    }
    meta <- read_metadata(name, id, config, metadata)
    report_data_import(con, meta, config)
  }

  legacy_report_db_rebuild_published(config)
}


report_db_needs_rebuild <- function(config) {
  con <- orderly_db("destination", config, FALSE, FALSE)
  on.exit(DBI::dbDisconnect(con))

  d <- DBI::dbReadTable(con, orderly_schema_table)
  numeric_version(d$schema_version) < numeric_version(orderly_schema_version)
}


report_data_import <- function(con, dat_rds, config) {
  name <- dat_rds$meta$name
  id <- dat_rds$meta$id

  sql_name <- "SELECT name FROM report WHERE name = $1"
  if (nrow(DBI::dbGetQuery(con, sql_name, name)) == 0L) {
    DBI::dbWriteTable(con, "report", data_frame(name = name), append = TRUE)
  } else {
    sql <- "SELECT id FROM report_version WHERE report = $1"
    prev <- max(DBI::dbGetQuery(con, sql, name)$id)
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
    displayname = dat_rds$meta$displayname,
    description = dat_rds$meta$description,
    published = FALSE, # TODO: this eventually comes out
    elapsed = dat_rds$meta$elapsed,
    connection = dat_rds$meta$connection,
    git_sha = dat_rds$git$sha %||% NA_character_,
    git_branch = dat_rds$git$branch %||% NA_character_,
    git_clean = git_clean)

  DBI::dbWriteTable(con, "report_version", report_version, append = TRUE)

  if (!is.null(dat_rds$meta$extra_fields)) {
    custom <- vcapply(dat_rds$meta$extra_fields, function(x) as.character(x))
    custom <- custom[!is.na(custom)]
    if (length(custom) > 0) {
      report_version_custom_fields <- data_frame(
        report_version = id,
        key = names(custom),
        value = unname(custom))
      DBI::dbWriteTable(con, "report_version_custom_fields",
                        report_version_custom_fields, append = TRUE)
    }
  }

  if (!is.null(dat_rds$meta$view)) {
    report_version_view <- cbind(report_version = id, dat_rds$meta$view,
                                 stringsAsFactors = FALSE)
    DBI::dbWriteTable(con, "report_version_view", report_version_view,
                      append = TRUE)
  }

  ## Then see if the data is known:
  data <- dat_rds$meta$data
  if (!is.null(data) && nrow(data) > 0L) {
    sql_data <- sprintf("SELECT hash FROM data WHERE hash IN (%s)",
                        paste(dquote(data$hash), collapse = ", "))
    msg <- setdiff(data$hash, DBI::dbGetQuery(con, sql_data)$hash)
    if (length(msg)) {
      i <- data$hash %in% msg & !duplicated(data$hash)
      cols <- c("hash", "size_csv", "size_rds")
      DBI::dbWriteTable(con, "data", data[i, cols], append = TRUE)
    }

    report_version_data <- cbind(report_version = id,
                                 data[c("name", "database", "query", "hash")],
                                 stringsAsFactors = FALSE)
    DBI::dbWriteTable(con, "report_version_data", report_version_data,
                      append = TRUE)
  }

  packages <- dat_rds$meta$packages
  if (length(packages) > 0L) {
    r_version <-
      paste(dat_rds$session_info$R.version[c("major", "minor")], collapse = ".")
    pkgs_base <-
      set_names(rep(r_version, length(dat_rds$session_info$basePkgs)),
                dat_rds$session_info$basePkgs)
    pkgs_other <- vcapply(unlist(
      unname(dat_rds$session_info[c("otherPkgs", "loadedOnly")]), FALSE),
      "[[", "Version")
    report_version_package <- data_frame(
      report_version = rep(id, length(packages)),
      package_name = packages,
      package_version = c(pkgs_base, pkgs_other)[packages])
    rownames(report_version_package) <- NULL
    DBI::dbWriteTable(con, "report_version_package", report_version_package,
                      append = TRUE)
  }

  depends <- report_data_find_dependencies(con, dat_rds$meta, config)
  if (!is.null(depends)) {
    DBI::dbWriteTable(con, "depends", depends, append = TRUE)
  }

  ## Inputs:
  report_data_add_files(con, dat_rds$meta$file_info_inputs)
  file_input <- cbind(
    report_version = id,
    dat_rds$meta$file_info_inputs[c("file_hash", "filename", "file_purpose")],
    stringsAsFactors = FALSE)
  DBI::dbWriteTable(con, "file_input", file_input, append = TRUE)

  if (!is.null(dat_rds$meta$global_resources)) {
    sql <- c("SELECT id, filename FROM file_input",
             " WHERE report_version = $1 AND file_purpose = 'global'")
    tmp <- DBI::dbGetQuery(con, paste(sql, collapse = " "), id)
    i <- match(names(dat_rds$meta$global_resources), tmp$filename)
    file_input_global <- data_frame(
      file_input = tmp$id[i],
      filename = unname(dat_rds$meta$global_resources))
    DBI::dbWriteTable(con, "file_input_global", file_input_global,
                      append = TRUE)
  }

  ## Artefacts:
  report_data_add_files(con, dat_rds$meta$file_info_artefacts)
  report_version_artefact <- cbind(
    report_version = id,
    dat_rds$meta$artefacts,
    stringsAsFactors = FALSE)
  DBI::dbWriteTable(con, "report_version_artefact", report_version_artefact,
                    append = TRUE)

  sql <- paste("SELECT id FROM report_version_artefact",
               "WHERE report_version = $1 ORDER BY 'order'")
  artefact_id <- DBI::dbGetQuery(con, sql, id)$id

  file_artefact <- cbind(
    artefact = artefact_id[dat_rds$meta$file_info_artefacts$order],
    dat_rds$meta$file_info_artefacts[c("file_hash", "filename")])
  DBI::dbWriteTable(con, "file_artefact", file_artefact, append = TRUE)

  changelog <- dat_rds$meta$changelog
  if (!is.null(changelog)) {
    changelog <- changelog[changelog$report_version == id, , drop = FALSE]
    if (nrow(changelog) > 0L) {
      prev <- DBI::dbGetQuery(con, "SELECT max(ordering) FROM changelog")[[1]]
      if (is.na(prev)) {
        prev <- 0L
      }
      changelog$ordering <- seq_len(nrow(changelog)) + prev
      DBI::dbWriteTable(con, "changelog", changelog, append = TRUE)
    }
  }

  tags <- dat_rds$meta$tags
  if (!is.null(tags)) {
    report_version_tag <- data_frame(report_version = id, tag = tags)
    DBI::dbWriteTable(con, "report_version_tag", report_version_tag,
                      append = TRUE)
  }

  if (!is.null(dat_rds$meta$parameters)) {
    p <- dat_rds$meta$parameters
    parameters <- data_frame(
      report_version = id,
      name = names(p),
      type = report_db_parameter_type(p),
      value = report_db_parameter_serialise(p))
    DBI::dbWriteTable(con, "parameters", parameters, append = TRUE)
  }

  if (!is.null(dat_rds$meta$batch_id)) {
    sql_batch <- "SELECT id FROM report_batch WHERE id = $1"
    if (nrow(DBI::dbGetQuery(con, sql_batch, dat_rds$meta$batch_id)) == 0L) {
      batch <- data_frame(
        id = dat_rds$meta$batch_id
      )
      DBI::dbWriteTable(con, "report_batch", batch, append = TRUE)
    }
    report_version_batch <- data_frame(
      report_version = id,
      report_batch = dat_rds$meta$batch_id
    )
    DBI::dbWriteTable(con, "report_version_batch", report_version_batch,
                      append = TRUE)
  }

  if (!is.null(dat_rds$meta$workflow)) {
    sql_batch <- "SELECT id FROM workflow WHERE id = $1"
    if (nrow(DBI::dbGetQuery(con, sql_batch, dat_rds$meta$workflow)) == 0L) {
      workflow <- data_frame(id = dat_rds$meta$workflow)
      DBI::dbWriteTable(con, "workflow", workflow, append = TRUE)
    }
    report_version_workflow <- data_frame(
      report_version = id,
      workflow_id = dat_rds$meta$workflow
    )
    DBI::dbWriteTable(con, "report_version_workflow", report_version_workflow,
                      append = TRUE)
  }

  ## DB instance:
  if (!is.null(dat_rds$meta$instance)) {
    ## Only add row where instance is set
    instances <- lapply(names(dat_rds$meta$instance), function(type) {
      dat_rds$meta$instance[[type]]
    })
    names(instances) <- names(dat_rds$meta$instance)
    instances <- instances[!vlapply(instances, is.null)]
    if (length(instances) > 0) {
      report_version_instance <- data_frame(
        report_version = id,
        type = names(instances),
        instance = list_to_character(instances))
      DBI::dbWriteTable(con, "report_version_instance", report_version_instance,
                        append = TRUE)
    }
  }

  sql <- "UPDATE report SET latest = $1 WHERE name = $2"
  DBI::dbExecute(con, sql, list(id, name))
}


report_data_add_files <- function(con, files) {
  sql <- sprintf("SELECT hash from file WHERE hash IN (%s)",
                 paste(dquote(unique(files$file_hash)), collapse = ", "))
  hash_msg <- setdiff(files$file_hash, DBI::dbGetQuery(con, sql)$hash)
  i <- files$file_hash %in% hash_msg & !duplicated(files$file_hash)
  if (any(i)) {
    file <- data_frame(hash = files$file_hash[i],
                       size = files$file_size[i])
    DBI::dbWriteTable(con, "file", file, append = TRUE)
  }
}


report_data_find_dependencies <- function(con, meta, config) {
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

  ## Verify that all dependencies are found and return (hopefully)
  ## helpful messages.  This is not a practical issue for our main
  ## orderly workflows.
  if (any(is.na(depends_use))) {
    err <- unique(meta$depends$id[is.na(depends_use)])
    is_draft <- vlapply(err, function(id)
      !is.null(orderly_find_name(id, config, draft = TRUE)))
    if (any(!is_draft)) {
      stop("Report uses nonexistant id:\n",
           paste(sprintf("\t- %s", err[!is_draft]), collapse = "\n"))
    }
    if (any(is_draft)) {
      stop("Report uses draft id - commit first:\n",
           paste(sprintf("\t- %s", err[is_draft]), collapse = "\n"))
    }
  }

  data_frame(
    report_version = meta$id,
    use = depends_use,
    as = meta$depends$as,
    is_latest = meta$depends$is_latest,
    is_pinned = meta$depends$is_pinned)
}


report_db_destroy <- function(con, config) {
  dialect <- report_db_dialect(con)
  schema <- names(report_db_schema(config$fields, dialect)$tables)
  existing <- DBI::dbListTables(con)
  known <- DBI::dbReadTable(con, orderly_table_list)[[1L]]
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


sqlite_pragma_fk <- function(con, enable = TRUE) {
  if (report_db_dialect(con) == "sqlite") {
    DBI::dbExecute(con, sprintf("PRAGMA foreign_keys = %d", enable))
  }
}


report_db_parameter_type <- function(x) {
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


report_db_parameter_serialise <- function(x) {
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


report_db_dialect <- function(con) {
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


read_metadata <- function(name, id, config, metadata_store) {
  if (metadata_store) {
    path_meta <- file.path(path_metadata(config$root), name, id)
  } else {
    workdir <- file.path(path_archive(config$root), name, id)
    path_meta <- path_orderly_run_rds(workdir)
  }
  readRDS(path_meta)
}
