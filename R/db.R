##' Connect to the orderly databases.  These should be treated as as
##' \emph{read only} (with the exception of \code{source}).
##'
##' @title Connect to orderly databases
##' @inheritParams orderly_list
##'
##' @param type The type of connection to make (\code{source},
##'   \code{destination}, \code{csv} or \code{rds}).
##'
##' @param validate Logical, indicating if the database schema should
##'   be validated on open (currently only applicable with \code{type
##'   = "destination"}).  This is primarily intended for internal use.
##'
##' @export
orderly_db <- function(type, config = NULL, locate = TRUE, validate = TRUE) {
  config <- orderly_config_get(config, locate)
  if (type == "rds") {
    file_store_rds(path_rds(config$path))
  } else if (type == "csv") {
    file_store_csv(path_csv(config$path))
  } else if (type %in% c("source", "destination")) {
    x <- orderly_db_args(type, config)
    con <- do.call(DBI::dbConnect, c(list(x$driver()), x$args))
    if (type == "destination") {
      withCallingHandlers(
        report_db2_init(con, config, validate = validate),
        error = function(e) DBI::dbDisconnect(con))
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


##' Rebuild the report database
##' @title Rebuild the report database
##' @inheritParams orderly_list
##'
##' @param verbose Logical, indicating if information about the
##'   rebuild should be printed as it runs
##'
##' @param if_schema_changed Logical, indicating if the rebuild should
##'   take place only if the schema has changed.  This is designed to
##'   be safe to use in (say) deployment scripts because it will be
##'   fast enough to call regularly.
##'
##' @export
orderly_rebuild <- function(config = NULL, locate = TRUE, verbose = TRUE,
                            if_schema_changed = FALSE) {
  config <- orderly_config_get(config, locate)

  if (length(migrate_plan(config$path, to = NULL)) > 0L) {
    orderly_log("migrate", "archive")
    orderly_migrate(config, locate = FALSE, verbose = verbose)
    ## This should trigger a rebuild, regardless of what anything else thinks
    if_schema_changed <- FALSE
  }

  if (!if_schema_changed || report_db2_needs_rebuild(config)) {
    orderly_log("rebuild", "db")
    report_db2_rebuild(config, verbose)
    invisible(TRUE)
  } else {
    invisible(FALSE)
  }
}
