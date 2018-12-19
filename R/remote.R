##' Download dependent reports (WARNING: this is still in transition
##' and the docs may not be entirely accurate)
##'
##' If \code{remote} is a \code{montagu_server} object then this
##' requires the montagu package and for montagu's credentials to be
##' correctly set up.  The \code{pull_archive} function pulls report
##' directly (without it being a dependent report).
##'
##' To use this functionality you will need to have \code{montagu}
##' installed, and be part of the VIMC team.  To set it up run
##'
##' \preformatted{
##' options(montagu.username = "your.email@imperial.ac.uk")
##' }
##'
##' You can add that line to your \code{~/.Rprofile} file perhaps -
##' see \code{path.expand("~/.Rprofile")} for where this file lives on
##' your computer.  After setting your username up you can run
##' \code{pull_dependencies("reportname")} to pull the
##' \emph{dependencies} of \code{"reportname"} down so that
##' \code{"reportname"} can be run, or you can run
##' \code{pull_archive("reportname")} to pull a copy of
##' \code{"reportname"} that has been run on the production server.
##'
##' @title Download dependent reports
##' @param name Name of the report to download dependencies for
##'
##' @param remote Description of the location.  This must be an
##'   \code{orderly_remote} object.
##'
##' @inheritParams orderly_list
##' @export
pull_dependencies <- function(name, config = NULL, locate = TRUE,
                              remote = NULL) {
  config <- orderly_config_get(config, locate)
  remote <- get_remote(remote, config)

  path <- file.path(path_src(config$path), name)
  depends <- recipe_read(path, config, FALSE)$depends

  for (i in seq_len(nrow(depends))) {
    if (!isTRUE(depends$draft[[i]])) {
      remote$pull(depends$name[[i]], depends$id[[i]], config)
    }
  }
}


##' @export
##' @rdname pull_dependencies
##'
##' @param id The identifier (for \code{pull_archive}.  The default is
##'   to use the latest report.
pull_archive <- function(name, id = "latest", config = NULL, locate = TRUE,
                         remote = NULL) {
  config <- orderly_config_get(config, locate)
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

  dest <- file.path(path_archive(config$path), name, id)
  if (file.exists(dest)) {
    orderly_log("pull", sprintf("%s:%s already exists, skipping", name, id))
  } else {
    orderly_log("pull", sprintf("%s:%s", name, id))
    remote$pull(name, id, config$path)
  }
}


##' Push an archive report to a remote location.
##'
##' This is experimental and only supported using paths for
##' \code{remote}.  It will be useful for doing something like sharing
##' preliminary artefacts peer-to-peer before running centrally.  It
##' is not supported (yet) for pushing onto a montagu server, but that
##' would be nice to have (probably locked down the same way that the
##' \code{ref} argument is).
##'
##' @title Push an archive report to a remote location
##' @inheritParams pull_dependencies
##' @export
push_archive <- function(name, id = "latest", config = NULL, locate = TRUE,
                         remote = NULL) {
  config <- orderly_config_get(config, locate)
  remote <- get_remote(remote, config)

  if (id == "latest") {
    id <- orderly_latest(name, config, FALSE)
  }

  v <- remote_report_versions(name, config, FALSE, remote)
  if (id %in% v) {
    orderly_log("push", sprintf("%s:%s already exists, skipping", name, id))
  } else {
    orderly_log("push", sprintf("%s:%s", name, id))
    remote$push(name, id, config$path)
  }
}


##' Run a report on a remote server (for now this means montagu).  Be
##' careful doing this because once a report is run to completion on
##' production it cannot be deleted.  So get things working locally,
##' test on science and then run on production.
##'
##' @title Run a report on montagu
##'
##' @param name Name of the report
##'
##' @param parameters Parameters for the reprt
##'
##' @param timeout Time to wait for the report to be returned (in seconds)
##'
##' @param poll Period to poll the server for results (in seconds)
##'
##' @param open Logical, indicating if the report should be opened in
##'   a browser on completion
##'
##' @param stop_on_error Logical, indicating if we should throw an
##'   error if the report fails.  If you set this to \code{FALSE} it
##'   will be much easier to debug, but more annoying in scripts.
##'
##' @param progress Logical, indicating if a progress spinner should
##'   be included.
##'
##' @param ref Optional reference, indicating which branch should be
##'   used.  This cannot be used on production.
##'
##' @inheritParams pull_dependencies
##'
##' @export
orderly_run_remote <- function(name, parameters = NULL, ref = NULL,
                               timeout = 3600, poll = 1,
                               open = TRUE, stop_on_error = TRUE,
                               progress = TRUE,
                               config = NULL, locate = TRUE, remote = NULL) {
  remote <- get_remote(remote, orderly_config_get(config, locate))
  remote$run(name, parameters, ref, timeout, poll, open, stop_on_error,
             progress, config)
}

##' Publish a report on a remote server
##'
##' @title Publish orderly report on remote server
##'
##' @param name Name of the report (unlike
##'   \code{\link{orderly_publish}} this must be provided sorry)
##'
##' @param id The report id
##'
##' @param value As \code{\link{orderly_publish}}, \code{TRUE} or
##'   \code{FALSE} to publish or unpublish a report (respectively)
##'
##' @inheritParams orderly_run_remote
##' @export
orderly_publish_remote <- function(name, id, value = TRUE,
                                   config = NULL, locate = TRUE,
                                   remote = NULL) {
  assert_scalar_character(name)
  assert_scalar_character(id)
  assert_scalar_logical(value)

  config <- orderly_config_get(config, locate)
  remote <- get_remote(remote, config)
  remote$publish(name, id, value)
}


##' Set and get default remote locations
##'
##' @title Set default remote location
##' @param value A string describing a remote, or \code{NULL} to clear
##' @inheritParams orderly_list
##' @export
##' @rdname default_remote
set_default_remote <- function(value, config = NULL, locate = TRUE) {
  config <- orderly_config_get(config, locate)

  if (is.null(value)) {
    remote <- NULL
  } else {
    assert_scalar_character(value)
    remote <- get_remote(value, config)
  }

  cache$default_remote[[config$path]] <- remote
  invisible(remote)
}


##' @rdname default_remote
get_default_remote <- function(config = NULL, locate = TRUE) {
  config <- orderly_config_get(config, locate)
  if (!is.null(cache$default_remote[[config$path]])) {
    return(cache$default_remote[[config$path]])
  }
  if (length(config$remote) > 0L) {
    browser()
    stop("needs porting")
    return(get_remote(names(remote)[[1L]], config))
  }
  ## TODO: all this comes out too
  ## default_remote_path <- Sys.getenv("ORDERLY_DEFAULT_REMOTE_PATH",
  ##                                   NA_character_)
  ## if (!is.na(default_remote_path)) {
  ##   return(orderly_remote_path(default_remote_path))
  ## }
  stop("default remote has not been set yet: use 'orderly::set_default_remote'")
}


get_remote <- function(remote, config) {
  if (is.null(remote)) {
    return(get_default_remote(config))
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
    if (length(remote$driver) == 2L) {
      driver <- getExportedValue(remote$driver[[1L]], remote$driver[[2L]])
    } else {
      driver <- remote$driver[[1L]]
    }
    args <- resolve_secrets(remote$args, config)
    cache$remotes[[hash]] <- do.call(driver, args)
  }
  cache$remotes[[hash]]
}


## Most of these functions can really shrink now?
remote_report_names <- function(config = NULL, locate = TRUE, remote = NULL) {
  remote <- get_remote(remote, orderly_config_get(config, locate))
  remote$list()
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
    is.function(x$list) &&
    is.function(x$list_versions) &&
    is.function(x$pull) &&
    is.function(x$publish) &&
    is.function(x$run)
}
