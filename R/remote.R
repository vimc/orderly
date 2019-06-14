##' Download dependent reports.
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
##' @title Download dependent reports
##'
##' @param name Name of the report to download dependencies for
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
##' @seealso \code{\link{orderly_remote_path}}, which implements the
##'   remote interface for orderly repositories at a local path.  See
##'   also \href{https://github.com/vimc/orderly-web}{OrderlyWeb} for
##'   a system for hosting orderly repositories over an HTTP API.
##'
##' @example man-roxygen/example-remote.R
orderly_pull_dependencies <- function(name, root = NULL, locate = TRUE,
                                      remote = NULL) {
  config <- orderly_config_get(root, locate)
  remote <- get_remote(remote, config)

  path <- file.path(path_src(config$root), name)
  depends <- recipe_read(path, config, FALSE)$depends

  for (i in seq_len(nrow(depends))) {
    if (!isTRUE(depends$draft[[i]])) {
      orderly_pull_archive(depends$name[[i]], depends$id[[i]], config,
                           FALSE, remote)
    }
  }
}


##' @export
##' @rdname orderly_pull_dependencies
##'
##' @param id The identifier (for \code{orderly_pull_archive}.  The default is
##'   to use the latest report.
orderly_pull_archive <- function(name, id = "latest", root = NULL,
                                 locate = TRUE, remote = NULL) {
  config <- orderly_config_get(root, locate)
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
  if (file.exists(dest)) {
    orderly_log("pull", sprintf("%s:%s already exists, skipping", name, id))
  } else {
    orderly_log("pull", sprintf("%s:%s", name, id))
    remote$pull(name, id, config$root)
  }
}


##' Push an archive report to a remote location.
##'
##' This is experimental and only supported using
##' \code{\link{orderly_remote_path}} remotes.  It might be useful for
##' doing something like sharing preliminary artefacts peer-to-peer
##' before running centrally.
##'
##' @title Push an archive report to a remote location
##' @inheritParams orderly_pull_dependencies
##' @export
push_archive <- function(name, id = "latest", root = NULL, locate = TRUE,
                         remote = NULL) {
  config <- orderly_config_get(root, locate)
  remote <- get_remote(remote, config)

  if (id == "latest") {
    id <- orderly_latest(name, config, FALSE)
  }

  v <- remote_report_versions(name, config, FALSE, remote)
  if (id %in% v) {
    orderly_log("push", sprintf("%s:%s already exists, skipping", name, id))
  } else {
    orderly_log("push", sprintf("%s:%s", name, id))
    remote$push(name, id, config$root)
  }
}


##' Run a report on a remote server.
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
##'   for it and instead thrown an error.
##'
##' @param poll Period to poll the server for results (in seconds)
##'
##' @param open Logical, indicating if the report should be opened in
##'   a browser on completion
##'
##' @param stop_on_error Logical, indicating if we should throw an
##'   error if the report fails.  If you set this to \code{FALSE} it
##'   will be much easier to debug, but more annoying in scripts.  If
##'   the times out on the server (i.e., takes longer than
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
orderly_run_remote <- function(name, parameters = NULL, ref = NULL,
                               timeout = NULL, wait = 3600, poll = 1,
                               open = TRUE, stop_on_error = TRUE,
                               stop_on_timeout = TRUE,
                               progress = TRUE,
                               root = NULL, locate = TRUE, remote = NULL) {
  remote <- get_remote(remote, orderly_config_get(root, locate))
  remote$run(name, parameters = parameters, ref = ref,
             timeout = timeout, wait = wait, poll = poll, progress = progress,
             stop_on_error = stop_on_error, stop_on_timeout = stop_on_timeout,
             open = open)
}


##' Set and get default remote locations
##'
##' @title Set default remote location
##' @param value A string describing a remote, or \code{NULL} to clear
##' @inheritParams orderly_list
##' @export
##' @rdname orderly_default_remote
orderly_default_remote_set <- function(value, root = NULL, locate = TRUE) {
  config <- orderly_config_get(root, locate)

  if (is.null(value)) {
    remote <- NULL
  } else {
    assert_scalar_character(value)
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
  if (is.null(cache$remotes[[hash]])) {
    driver <- getExportedValue(remote$driver[[1L]], remote$driver[[2L]])
    args <- resolve_secrets(remote$args, config)
    cache$remotes[[hash]] <- do.call(driver, args)
  }
  cache$remotes[[hash]]
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
    is.function(x$run)
}
