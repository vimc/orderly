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
orderly_db <- function(type, root = NULL, locate = TRUE, validate = TRUE) {
  config <- orderly_config_get(root, locate)
  if (type == "rds") {
    con <- file_store_rds(path_rds(config$root))
  } else if (type == "csv") {
    con <- file_store_csv(path_csv(config$root))
  } else if (type == "destination") {
    con <- orderly_db_dbi_connect(config$destination, config)
    withCallingHandlers(
      report_db_init(con, config, validate = validate),
      error = function(e) DBI::dbDisconnect(con))
  } else if (type == "source") {
    con <- lapply(config$database, orderly_db_dbi_connect, config)
  } else {
    stop(sprintf("Invalid db type '%s'", type))
  }
  con
}


orderly_db_dbi_connect <- function(x, config) {
  dat <- orderly_db_args(x, config)
  do.call(DBI::dbConnect, c(list(dat$driver()), dat$args))
}


orderly_db_args <- function(x, config) {
  driver <- getExportedValue(x$driver[[1L]], x$driver[[2L]])

  args <- withr::with_envvar(
    orderly_envir_read(config$root),
    args <- resolve_driver_config(x$args, config))

  if (x$driver[[2]] == "SQLite") {
    dbname <- args$dbname
    if (!nzchar(dbname) || tolower(dbname) == ":memory:") {
      stop("Cannot use a transient SQLite database with orderly")
    }
    if (is_relative_path(args$dbname)) {
      args$dbname <- file.path(config$root, args$dbname)
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
orderly_rebuild <- function(root = NULL, locate = TRUE, verbose = TRUE,
                            if_schema_changed = FALSE) {
  ## We'll skip warnings here - they'll come out as messages rather
  ## than warnings.
  oo <- options(orderly.nowarnings = TRUE)
  on.exit(options(oo))

  config <- orderly_config_get(root, locate)

  if (length(migrate_plan(config$root, to = NULL)) > 0L) {
    orderly_log("migrate", "archive")
    orderly_migrate(config, locate = FALSE, verbose = verbose)
    ## This should trigger a rebuild, regardless of what anything else thinks
    if_schema_changed <- FALSE
  }

  if (!if_schema_changed || report_db_needs_rebuild(config)) {
    orderly_log("rebuild", "db")
    report_db_rebuild(config, verbose)
    invisible(TRUE)
  } else {
    invisible(FALSE)
  }
}


## From the SQLite docs https://www.sqlite.org/c3ref/backup_finish.html
##
## > SQLite holds a write transaction open on the destination database
## > file for the duration of the backup operation. The source
## > database is read-locked only while it is being read; it is not
## > locked continuously for the entire backup operation. Thus, the
## > backup may be performed on a live source database without
## > preventing other database connections from reading or writing to
## > the source database while the backup is underway.
orderly_backup <- function(config = NULL, locate = TRUE) {
  config <- orderly_config_get(config, locate)
  if (config$destination$driver[[1]] == "RSQLite") {
    src <- config$destination$args$dbname

    ## TODO: make the relative path configurable
    dest <- file.path("backup", basename(src), fsep = "/")
    dest_full <- file.path(config$root, dest)
    dir.create(dirname(dest_full), FALSE, TRUE)
    orderly_log("backup", sprintf("%s => %s", src, dest))

    sqlite_backup(src, dest_full)
  }
}
