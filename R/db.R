##' Connect to the orderly databases.  These should be treated as as
##' \emph{read only} (with the exception of \code{source}).
##'
##' @title Connect to orderly databases
##' @inheritParams orderly_list
##' @param type The type of connection to make (\code{source},
##'   \code{destination}, \code{csv} or \code{rds}).
##' @export
orderly_db <- function(type, config = NULL, locate = TRUE) {
  config <- orderly_config_get(config, locate)
  if (type == "rds") {
    file_store_rds(path_rds(config$path))
  } else if (type == "csv") {
    file_store_csv(path_csv(config$path))
  } else if (type %in% c("source", "destination")) {
    x <- orderly_db_args(type, config)
    con <- do.call(DBI::dbConnect, c(list(x$driver()), x$args))
    if (type == "destination") {
      report_db_init(con, config)
    }
    con
  } else {
    stop(sprintf("Invalid db type '%s'", type))
  }
}

orderly_db_args <- function(type, config) {
  x <- config[[type]]
  driver <- getExportedValue(x$driver[[1L]], x$driver[[2L]])

  args <- withr::with_envvar(
    orderly_envir_read(config$path),
    args <- resolve_driver_config(x$args, config))

  if (x$driver[[2]] == "SQLite") {
    dbname <- args$dbname
    if (!nzchar(dbname) || tolower(dbname) == ":memory:") {
      stop("Cannot use a transient SQLite database with orderly")
    }
    if (is_relative_path(args$dbname)) {
      args$dbname <- file.path(config$path, args$dbname)
    }
  }

  list(driver = driver, args = args)
}

## Reports database needs special initialisation:
report_db_init <- function(con, config, must_create = FALSE) {
  orderly_table <- "orderly"
  if (!DBI::dbExistsTable(con, orderly_table)) {
    ## TODO: we'll need some postgres translation here
    ## TODO: dumping json columns in as text for now
    ##
    ## TODO: the door is open here for the table name to be
    ## configurable, but defaulting to 'orderly', but I do not know
    ## that that is good thing, really.
    cols <- report_db_cols()
    col_types <- sprintf("  %s %s",
                         c(names(cols), config$fields$name),
                         c(unname(cols), config$fields$type_sql))

    sql <- sprintf("CREATE TABLE %s (\n%s\n)",
                   orderly_table,
                   paste(col_types, collapse = ",\n"))
    DBI::dbExecute(con, sql)
  } else if (must_create) {
    stop(sprintf("Table '%s' already exists", orderly_table))
  } else {
    sql <- sprintf("SELECT * FROM %s LIMIT 0", orderly_table)
    d <- DBI::dbGetQuery(con, sql)
    custom_name <- config$fields$name
    msg <- setdiff(custom_name, names(d))
    if (length(msg) > 0L) {
      stop(sprintf("custom fields %s not present in existing database",
                   paste(squote(msg), collapse = ", ")))
    }
    extra <- setdiff(setdiff(names(d), names(report_db_cols())), custom_name)
    if (length(extra) > 0L) {
      stop(sprintf("custom fields %s in database not present in config",
                   paste(squote(extra), collapse = ", ")))
    }
  }
  orderly_table
}

report_db_rebuild <- function(config, verbose = TRUE) {
  assert_is(config, "orderly_config")
  root <- config$path
  con <- orderly_db("destination", config)
  on.exit(DBI::dbDisconnect(con))
  ## TODO: this assumes name known
  tbl <- "orderly"
  DBI::dbExecute(con, "DELETE FROM orderly")
  reports <- unlist(lapply(list_dirs(path_archive(root)), list_dirs))
  if (length(reports) > 0L) {
    dat <- rbind_df(lapply(reports, report_read_data, config))
    DBI::dbWriteTable(con, tbl, dat, append = TRUE)
  }
}

report_db_cols <- function() {
  c(## INPUTS:
    id = "TEXT PRIMARY KEY NOT NULL",
    name = "TEXT",
    displayname = "TEXT",
    description = "TEXT",
    views = "TEXT",          # should be json (dict)
    data = "TEXT",           # should be json (dict)
    packages = "TEXT",       # should be json (array)
    script = "TEXT",
    artefacts = "TEXT",      # should be json (array)
    resources = "TEXT",      # should be json (array)
    hash_script = "TEXT",
    ## OUTPUTS
    parameters = "TEXT",     # should be json (dict with values)
    date = "DATETIME",
    hash_orderly = "TEXT",
    hash_input = "TEXT",
    hash_resources = "TEXT", # should be json (dict)
    hash_data = "TEXT",      # should be json (dict)
    hash_artefacts = "TEXT", # should be json (dict)
    depends = "TEXT",        # should be json (array of dicts)
    ## PUBLISHING
    published = "BOOLEAN")
}

##' Rebuild the report database
##' @title Rebuild the report database
##' @inheritParams orderly_list
##'
##' @param verbose Logical, indicating if information about the
##'   rebuild should be printed as it runs
##'
##' @export
orderly_rebuild <- function(config = NULL, locate = TRUE, verbose = TRUE) {
  config <- orderly_config_get(config, locate)
  report_db_rebuild(config, verbose)
  invisible(NULL)
}


with_connection <- function(con, f, ...) {
  withCallingHandlers(f(con, ...),
                      finally = function() DBI::dbDisconnect(con))
}
