##' Connect to the orderly databases.  These should be treated as as
##' \emph{read-only}.
##'
##' Orderly has several databases:
##'
##' \describe{
##'
##' \item{source}{All of the databases named in the \code{database}
##'   section of the \code{orderly_config.yml}}
##'
##' \item{destination}{The orderly index database (typically a SQLite
##' database stored at the orderly root)}
##'
##' \item{csv}{The cache of database query results, in csv format}
##'
##' \item{rds}{The cache of database query results, in rds format}
##'
##' }
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
##' @param instance Used only by \code{type = "source"}, and used to
##'   select the instance, where multiple instances are configured.
##'   Use a single \emph{unnamed} character string to indicate an
##'   instance to match.  If given, then this name must be present in
##'   all databases where instances are listed in
##'   \code{orderly_config.yml}, and will be ignored by all database
##'   where instances are not given.  See the "orderly" vignette for
##'   further information.
##'
##' @return A database connection, or list of connections in the case
##'   of \code{source}.
##'
##' @export
##' @examples
##' # Create an orderly that has a single commited report:
##' path <- orderly::orderly_example("minimal")
##' id <- orderly::orderly_run("example", root = path)
##' orderly::orderly_commit(id, root = path)
##'
##' # The source database holds the data that might be accessible via
##' # the 'data' entry in orderly.yml:
##' db <- orderly::orderly_db("source", root = path)
##' # This is a list, with one connection per database listed in the
##' # orderly_config.yml (an empty list if none are specified):
##' db
##' DBI::dbListTables(db$source)
##' head(DBI::dbReadTable(db$source, "data"))
##' DBI::dbDisconnect(db$source)
##'
##' # The destination database holds information about the archived
##' # reports:
##' db <- orderly::orderly_db("destination", root = path)
##' DBI::dbListTables(db)
##'
##' # These tables are documented online:
##' # https://vimc.github.io/orderly/schema
##' DBI::dbReadTable(db, "report_version")
orderly_db <- function(type, root = NULL, locate = TRUE, validate = TRUE,
                       instance = NULL) {
  config <- orderly_config_get(root, locate)
  if (type == "rds") {
    con <- file_store_rds(path_rds(config$root))
  } else if (type == "csv") {
    con <- file_store_csv(path_csv(config$root))
  } else if (type == "destination") {
    con <- orderly_db_dbi_connect(config$destination, config,
                                  "orderly_config.yml:destination")
    withCallingHandlers(
      report_db_init(con, config, validate = validate),
      error = function(e) DBI::dbDisconnect(con))
  } else if (type == "source") {
    config_db <- db_instance_select(instance, config$database)
    name <- sprintf("orderly_config.yml:database:%s", names(config_db))
    con <- Map(orderly_db_dbi_connect, config_db, name = name,
               MoreArgs = list(config = config))
  } else {
    stop(sprintf("Invalid db type '%s'", type))
  }
  con
}


orderly_db_dbi_connect <- function(x, config, name) {
  dat <- orderly_db_args(x, config, name)
  con <- do.call(DBI::dbConnect, c(list(dat$driver()), dat$args))
  attr(con, "instance") <- x[["instance"]]
  con
}


orderly_db_args <- function(x, config, name) {
  driver <- getExportedValue(x$driver[[1L]], x$driver[[2L]])
  args <- withr::with_envvar(
    orderly_envir_read(config$root),
    args <- resolve_driver_config(x$args, config, name))

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


##' Rebuild the report database.  This is necessary when the orderly
##' database schema changes, and you will be prompted to run this
##' function after upgrading orderly in that case.
##'
##' The report database (orderly's "destination" database) is
##' essentially an index over all the metadata associated with
##' reports.  It is used by orderly itself, and can be used by
##' applications that extend orderly (e.g.,
##' \href{https://github.com/vimc/orderly-web}{OrderlyWeb}).  All the
##' data in this database can be rebuilt from files stored with the
##' committed (archive) orderly reports, using the
##' \code{orderly_rebuild} function.
##'
##' @title Rebuild the report database
##'
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
##' @return No return value, this function is called only for its side effects
##' @examples
##' path <- orderly::orderly_example("minimal")
##' id <- orderly::orderly_run("example", root = path)
##' orderly::orderly_commit(id, root = path)
##'
##' con <- orderly::orderly_db("destination", root = path)
##' DBI::dbReadTable(con, "report_version")
##' DBI::dbDisconnect(con)
##'
##' # The database can be removed and will be rebuilt if requested
##' # (this is only a good idea if you do not extend the database with
##' # your own fields - only the fields that orderly looks after can
##' # be recovered!)
##' file.remove(file.path(path, "orderly.sqlite"))
##' orderly::orderly_rebuild(path)
##' file.exists(file.path(path, "orderly.sqlite"))
##' con <- orderly::orderly_db("destination", root = path)
##' DBI::dbReadTable(con, "report_version")
##' DBI::dbDisconnect(con)
##'
##' # It is safe to rebuild a database repeatedly, though this can be
##' # slow with larger databases.
##' orderly::orderly_rebuild(path)
orderly_rebuild <- function(root = NULL, locate = TRUE, verbose = TRUE,
                            if_schema_changed = FALSE) {
  ## We'll skip warnings here - they'll come out as messages rather
  ## than warnings.
  oo <- options(orderly.nowarnings = TRUE)
  on.exit(options(oo))

  config <- orderly_config_get(root, locate)
  orderly_backup(config, suffix = paste0(".", iso_time_str(Sys.time())))

  if (length(migrate_plan(config$archive_version, to = NULL)) > 0L) {
    orderly_log("migrate", "archive")
    orderly_migrate(config, locate = FALSE)
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
orderly_backup <- function(config = NULL, locate = TRUE, suffix = NULL) {
  config <- orderly_config_get(config, locate)
  if (config$destination$driver[[1]] == "RSQLite") {
    curr <- orderly_db_args(config$destination, config)$args$dbname

    dest <- path_db_backup(config$root, curr, suffix)
    dir.create(dirname(dest), FALSE, TRUE)

    prefix <- paste0(config$root, "/")
    orderly_log("backup", sprintf("%s => %s",
                                  sub(prefix, "", curr, fixed = TRUE),
                                  sub(prefix, "", dest, fixed = TRUE)))

    sqlite_backup(curr, dest)
  }
}


db_instance_select <- function(instance, config_db) {
  instances <- lapply(config_db, function(x) names(x$instances))
  has_instance <- lengths(instances) > 0L
  if (!any(has_instance)) {
    if (!is.null(instance)) {
      stop("Can't specify 'instance' with no databases supporting it")
    }
    return(config_db)
  }

  if (is.null(instance)) {
    instance <- vcapply(config_db[has_instance], function(x)
      names(x$instances)[[1]])
  }

  assert_character(instance)
  if (length(instance) == 1L && is.null(names(instance))) {
    err <- !vlapply(instances[has_instance], function(x) instance %in% x)
    if (any(err)) {
      stop(sprintf("Invalid instance '%s' for %s %s",
                   instance,
                   ngettext(sum(err), "database", "databases"),
                   paste(squote(names(err)[err]), collapse = ", ")),
           call. = FALSE)
    }
    instance <- set_names(rep(instance, sum(has_instance)),
                          names(instances)[has_instance])
  } else {
    assert_named(instance, TRUE)
    ## 1. Validate all database names (names(instance)) are in names(db)
    err <- setdiff(names(instance), names(config_db))
    if (length(err) > 0L) {
      stop(sprintf("Invalid database %s %s in provided instance",
                   ngettext(length(err), "name", "names"),
                   paste(squote(err), collapse = ", ")),
           call. = FALSE)
    }
    ## 2. Validate all instance names (unname(instance)) are in each db
    err <- !list_to_logical(Map(`%in%`, instance, instances[names(instance)]))
    if (any(err)) {
      msg <- sprintf("'%s' for '%s'", instance[err], names(instance)[err])
      stop(sprintf("Invalid %s: %s",
                   ngettext(length(msg), "instance", "instances"),
                   paste(msg, collapse = ", ")),
           call. = FALSE)
    }
    for (i in setdiff(names(which(has_instance)), names(instance))) {
      instance[[i]] <- names(config_db[[i]]$instances)[[1]]
    }
  }

  for (i in names(instance)) {
    config_db[[i]]$args <- config_db[[i]]$instances[[instance[[i]]]]
    config_db[[i]]$instance <- instance[[i]]
  }

  config_db
}
