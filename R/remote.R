##' Download dependent reports from an orderly remote.  This can only
##' be used if the \code{orderly_config.yml} lists a remote.  This
##' allows for a centralised workflow where a central orderly store
##' exists and holds the canonical copies of reports, from which
##' versions can be downloaded into local stores.
##'
##' The \code{orderly_pull_archive} function pulls report directly
##' (without it being a dependent report).
##'
##' After setting your username up you can run
##' \code{orderly_pull_dependencies("reportname")} to pull the
##' \emph{dependencies} of \code{"reportname"} down so that
##' \code{"reportname"} can be run, or you can run
##' \code{orderly_pull_archive("reportname")} to pull a copy of
##' \code{"reportname"} that has been run on the remote server.
##'
##' Pulling an archive report from a remote also pulls its
##' dependencies (recursively), and adds all of these to the local
##' database.  This may require migrating old orderly archives
##' (\code{\link{orderly_migrate}}).  Note that this migration will
##' likely fail for remote orderly versions older than 0.6.8 because
##' the migration needs to read data files on disk that are not
##' included in the downloaded archive in order to collect all the
##' information required for the database.  In this case, ask the
##' administrator of the remote orderly archive to migrate their
##' archive, and then re-pull.
##'
##' @title Download dependent reports
##'
##' @param name Name of the report to download dependencies for.
##'   Alternatively, the default of \code{NULL} is useful if you have
##'   already set the working directory to be the source directory.
##'
##' @param remote Description of the location.  Typically this is a
##'   character string indicating a remote specified in the
##'   \code{remotes} block of your \code{orderly_config.yml}.  It is
##'   also possible to pass in a directly created remote object (e.g.,
##'   using \code{\link{orderly_remote_path}}, or one provided by
##'   another package).  If left \code{NULL}, then the default remote
##'   for this orderly repository is used - by default that is the
##'   first listed remote.
##'
##' @inheritParams orderly_list
##' @export
##'
##' @return No return value, these functions are called only for their
##'   side effects
##'
##' @seealso \code{\link{orderly_remote_path}}, which implements the
##'   remote interface for orderly repositories at a local path.  See
##'   also \href{https://github.com/vimc/orderly-web}{OrderlyWeb} for
##'   a system for hosting orderly repositories over an HTTP API.
##'   \code{vignette("remote", package = "orderly")} describes the
##'   remote system in more detail.
##'
##' @example man-roxygen/example-remote.R
orderly_pull_dependencies <- function(name = NULL, root = NULL, locate = TRUE,
                                      remote = NULL) {
  loc <- orderly_develop_location(name, root, locate)
  name <- loc$name
  config <- loc$config
  remote <- get_remote(remote, config)
  depends <- orderly_recipe$new(name, config)$depends

  msg <- sprintf("%s has %d %s", name, NROW(depends),
                 ngettext(NROW(depends), "dependency", "dependencies"))
  orderly_log("depends", msg)

  if (!is.null(depends)) {
    for (i in seq_len(nrow(depends))) {
      if (!isTRUE(depends$draft[[i]])) {
        orderly_pull_archive(depends$name[[i]], depends$id[[i]], config,
                             FALSE, remote)
      }
    }
  }
}


##' @export
##' @rdname orderly_pull_dependencies
##'
##' @param id The identifier (for \code{orderly_pull_archive}).  The default is
##'   to use the latest report.
orderly_pull_archive <- function(name, id = "latest", root = NULL,
                                 locate = TRUE, remote = NULL) {
  config <- orderly_config_get(root, locate)
  config <- check_orderly_archive_version(config)
  remote <- get_remote(remote, config)

  v <- remote_report_versions(name, config, FALSE, remote)
  if (length(v) == 0L) {
    stop("Unknown report")
  }

  if (id == "latest") {
    id <- latest_id(v)
  }

  if (!(id %in% v)) {
    ## Confirm that the report does actually exist, working around
    ## VIMC-1281:
    stop(sprintf(
      "Version '%s' not found at '%s': valid versions are:\n%s",
      id, remote_name(remote), paste(sprintf("  - %s", v), collapse = "\n")),
      call. = FALSE)
  }

  dest <- file.path(path_archive(config$root), name, id)
  label <- sprintf("%s:%s", name, id)
  if (file.exists(dest)) {
    orderly_log("pull", sprintf("%s already exists, skipping", label))
  } else {
    orderly_log("pull", label)
    path <- remote$pull(name, id)

    ## There's an assumption here that the depenency resolution here
    ## will not be badly affected by migrations.  If a migration
    ## changes how d$meta$depends is structured (if d$meta$depends
    ## stops being a data.frame that includes name and id as
    ## columns) then we'll need to deal with that when checking
    ## dependencies too.
    orderly_pull_resolve_dependencies(path, remote, config)

    ## Only migrate after dependencies have been resolved because
    ## some migrations do check dependencies.
    migrate_single(path, config)

    withCallingHandlers({
      copy_directory(path, dest)
      report_db_import(name, id, config)
    }, error = function(e) unlink(dest, recursive = TRUE))
  }
}


##' Run a report on a remote server.  Note that this is only supported
##' for remotes using OrderlyWeb at present.
##'
##' @title Run a report on a remote server
##'
##' @param name Name of the report
##'
##' @param parameters Parameters for the report
##'
##' @param timeout Time to tell the server to wait before killing the
##'   report.
##'
##' @param wait Time to wait for the report to be run; if the report
##'   takes longer than this time to run but \code{timeout} is longer
##'   it will remain running on the server but we will stop waiting
##'   for it and instead throw an error.
##'
##' @param poll Period to poll the server for results (in seconds)
##'
##' @param open Logical, indicating if the report should be opened in
##'   a browser on completion (if supported by the remote)
##'
##' @param stop_on_error Logical, indicating if we should throw an
##'   error if the report fails.  If you set this to \code{FALSE} it
##'   will be much easier to debug, but more annoying in scripts.  If
##'   the report times out on the server (i.e., takes longer than
##'   \code{timeout}) that counts as an error.
##'
##' @param stop_on_timeout Logical, indicating if we should throw an
##'   error if the report takes longer than \code{wait} seconds to
##'   complete.
##'
##' @param progress Logical, indicating if a progress spinner should
##'   be included.
##'
##' @param ref Optional reference, indicating which branch should be
##'   used.  This cannot be used if the remote has \code{master_only}
##'   set.
##'
##' @inheritParams orderly_pull_dependencies
##'
##' @export
##' @return No return value, this function is called only for its side effects
##' @examples
##' path_remote <- orderly::orderly_example("demo")
##' path_local <- orderly::orderly_example("demo")
##' remote <- orderly::orderly_remote_path(path_remote)
##' # Currently, path remotes don't support run
##' try(orderly::orderly_run_remote(
##'   "minimal", remote = remote, root = path_local))
orderly_run_remote <- function(name, parameters = NULL, ref = NULL,
                               timeout = NULL, wait = 3600, poll = 1,
                               open = TRUE, stop_on_error = TRUE,
                               stop_on_timeout = TRUE,
                               progress = TRUE,
                               root = NULL, locate = TRUE, remote = NULL) {
  remote <- get_remote(remote, orderly_config_get(root, locate))
  invisible(remote$run(
    name, parameters = parameters, ref = ref,
    timeout = timeout, wait = wait, poll = poll,
    progress = progress, open = open,
    stop_on_error = stop_on_error,
    stop_on_timeout = stop_on_timeout))
}


##' Set and get default remote locations.  Default locations are
##' specific to an orderly repository (based on the path of the
##' repository) so there is no interaction between different orderly
##' projects.
##'
##' @title Set default remote location
##'
##' @param value A string describing a remote, a remote object, or
##'   \code{NULL} to clear
##'
##' @inheritParams orderly_list
##' @export
##'
##' @return The default remote (for
##'   \code{orderly_default_remote_get}). The function
##'   \code{orderly_default_remote_set} is called for its side effects
##'   only.
##'
##' @rdname orderly_default_remote
##' @examples
##' # Same setup as in orderly_remote_path, with a remote orderly:
##' path_remote <- orderly::orderly_example("demo")
##' id <- orderly::orderly_run("other", list(nmin = 0),
##'                            root = path_remote, echo = FALSE)
##' orderly::orderly_commit(id, root = path_remote)
##' id <- orderly::orderly_run("use_dependency",
##'                            root = path_remote, echo = FALSE)
##' orderly::orderly_commit(id, root = path_remote)
##'
##' # And a local orderly
##' path_local <- orderly::orderly_example("demo")
##'
##' # We'll create an object to interact with this remote using
##' # orderly_remote_path.
##' remote <- orderly::orderly_remote_path(path_remote)
##'
##' # There is no remote set by default:
##' try(orderly::orderly_default_remote_get(root = path_local))
##'
##' # We can set one:
##' orderly::orderly_default_remote_set(remote, root = path_local)
##'
##' # and now we can retrieve it:
##' orderly::orderly_default_remote_get(root = path_local)
##'
##' # Note that this has not affected the other orderly:
##' try(orderly::orderly_default_remote_get(root = path_remote))
orderly_default_remote_set <- function(value, root = NULL, locate = TRUE) {
  config <- orderly_config_get(root, locate)

  if (is.null(value)) {
    remote <- NULL
  } else {
    remote <- get_remote(value, config)
  }

  cache$default_remote[[config$root]] <- remote
  invisible(remote)
}


##' @rdname orderly_default_remote
##' @export
orderly_default_remote_get <- function(root = NULL, locate = TRUE) {
  config <- orderly_config_get(root, locate)
  if (!is.null(cache$default_remote[[config$root]])) {
    return(cache$default_remote[[config$root]])
  }
  if (length(config$remote) > 0L) {
    return(get_remote(names(config$remote)[[1L]], config))
  }
  msg <- paste("default remote has not been set yet:",
               "use 'orderly::orderly_default_remote_set'")
  stop(msg)
}


##' Get a remote, based on the configuration in
##' \code{orderly_config.yml} - different remote drivers have
##' different methods, and this function gives you access to these
##' lower-level objects.
##'
##' @title Get a remote
##'
##' @inheritParams orderly_pull_dependencies
##'
##' @return The orderly remote, as described in
##'   \code{orderly_config.yml} - if no remotes are configured, or if
##'   the requested remote does not exist, an error will be thrown.
##'
##' @seealso \code{\link{orderly_pull_dependencies}} which provides a
##'   higher-level interface to pulling from a remote (including
##'   adding the downloaded archive into your orderly repository), and
##'   see the documentation underlying the orderly remote driver that
##'   your \code{orderly_config.yml} declares for information about
##'   using that remote.
##'
##' @export
##' @example man-roxygen/example-orderly-remote.R
orderly_remote <- function(remote = NULL, root = NULL, locate = TRUE) {
  config <- orderly_config_get(root, locate)
  get_remote(remote, config)
}


get_remote <- function(remote, config) {
  if (is.null(remote)) {
    return(orderly_default_remote_get(config))
  }
  if (implements_remote(remote)) {
    return(remote)
  }
  if (!is.character(remote)) {
    stop("Unknown remote type ",
         paste(squote(class(remote)), collapse = " / "),
         call. = FALSE)
  }

  assert_scalar(remote)
  if (remote %in% names(config$remote)) {
    load_remote(remote, config)
  } else if (file.exists(remote)) {
    orderly_remote_path(remote)
  } else {
    stop(sprintf("Unknown remote '%s'", remote), call. = FALSE)
  }
}


load_remote <- function(name, config) {
  remote <- config$remote[[name]]
  hash <- hash_object(remote)

  cached <- cache$remotes[[hash]]
  if (!is.null(cached)) {
    return(cached)
  }

  driver <- getExportedValue(remote$driver[[1L]], remote$driver[[2L]])
  base <- "orderly_config.yml:remote"

  where <- sprintf("%s:%s:args", base, name)
  args <- resolve_secrets(resolve_env(remote$args, where), config)
  value <- do.call(driver, args)

  ## TODO(VIMC-3544): put this in an 'data' or equivalent argument
  ## to the constructor, but that required fixing both drivers.
  attr(value, "slack_url") <-
    resolve_env(remote["slack_url"], error = FALSE)$slack_url
  attr(value, "teams_url") <-
    resolve_env(remote["teams_url"], error = FALSE)$teams_url
  attr(value, "primary") <- isTRUE(remote$primary)

  cache$remotes[[hash]] <- value

  value
}


## For debugging
clear_remote_cache <- function() {
  cache$remotes <- list()
}


## Most of these functions can really shrink now?
remote_report_names <- function(root = NULL, locate = TRUE, remote = NULL) {
  remote <- get_remote(remote, orderly_config_get(root, locate))
  remote$list_reports()
}


remote_report_versions <- function(name, config = NULL, locate = TRUE,
                                   remote = NULL) {
  remote <- get_remote(remote, orderly_config_get(config, locate))
  remote$list_versions(name)
}


remote_name <- function(remote) {
  remote$name
}


## Test to see if something (might) implement our remote interface
implements_remote <- function(x) {
  is.recursive(x) &&
    is.function(x$list_reports) &&
    is.function(x$list_versions) &&
    is.function(x$pull) &&
    is.function(x$run) &&
    is.function(x$url_report)
}


orderly_pull_resolve_dependencies <- function(path, remote, config) {
  d <- readRDS(path_orderly_run_rds(path))
  depends <- d$meta$depends
  if (NROW(depends) > 0L) { # NROW(x) is 0 for x = NULL
    depends <- depends[!duplicated(depends$id), c("name", "id"), drop = FALSE]
    orderly_log("depends",
                paste(sprintf("%s/%s", depends$name, depends$id),
                      collapse = ", "))
    for (i in seq_len(nrow(depends))) {
      orderly_pull_archive(depends$name[[i]], depends$id[[i]], root = config,
                           locate = FALSE, remote = remote)
    }
  }
}
