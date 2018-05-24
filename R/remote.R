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

  ## For now, we need to assume that this is valid formatted orderly
  ## yaml, until I resolve VIMC-506 I have to read everything
  ## manually:
  path <- file.path(path_src(config$path), name)
  filename <- file.path(path, "orderly.yml")
  if (!file.exists(filename)) {
    stop("Did not find file 'orderly.yml' at path ", path)
  }
  info <- yaml_read(filename)

  depends <- info$depends
  for (i in seq_along(depends)) {
    if (!isTRUE(depends[[i]]$draft)) {
      pull_archive(names(depends)[[i]], depends[[i]]$id, config, remote)
    }
  }
}


##' @export
##' @rdname pull_dependencies
##'
##' @param id The identifier (for \code{pull_archive}.  The default is
##'   to pull the latest report.
pull_archive <- function(name, id = "latest", config = NULL, locate = TRUE,
                         remote = NULL) {
  config <- orderly_config_get(config, locate)

  remote <- get_remote(remote, config)
  if (inherits(remote, "orderly_api_server")) {
    pull_archive_api(name, id, config, remote)
  } else if (inherits(remote, "orderly_remote_path")) {
    pull_archive_path(name, id, config, remote)
  } else {
    stop("Unknown remote type ", paste(squote(class(remote)), collapse = " / "))
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
  config <- orderly_config_get(config, locate)
  remote <- get_remote(remote, config)
  if (inherits(remote, "orderly_api_server")) {
    orderly_run_remote_api(name = name, parameters = parameters, ref = ref,
                           timeout = timeout, poll = poll, open = open,
                           stop_on_error = stop_on_error,
                           progress = progress,
                           config = config, remote = remote)
  } else if (inherits(remote, "orderly_remote_path")) {
    stop("Can't run reports with remote type ",
         paste(squote(class(remote)), collapse = " / "))
  }
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
                                   config = NULL, locate = TRUE, remote = NULL) {
  config <- orderly_config_get(config, locate)
  remote <- get_remote(remote, config)
  if (inherits(remote, "orderly_api_server")) {
    orderly_publish_remote_api(name = name, id = id, config = config,
                               value = value, remote = remote)
  } else if (inherits(remote, "orderly_remote_path")) {
    ## This one can actually be done over disk too
    stop("Can't publish reports with remote type ",
         paste(squote(class(remote)), collapse = " / "))
  }
}


##' Set and get default remote locations
##'
##' @title Set default remote location
##' @param value An \code{orderly_remote_location} object.
##' @export
##' @rdname default_remote
set_default_remote <- function(value) {
  if (!is.null(value)) {
    assert_is(value, "orderly_remote_location")
  }
  cache$default_remote <- value
}


##' @rdname default_remote
##' @inheritParams orderly_list
get_default_remote <- function(config = NULL, locate = TRUE) {
  if (!is.null(cache$default_remote)) {
    return(cache$default_remote)
  }
  config <- orderly_config_get(config, locate)
  if (length(config$api_server) > 0L) {
    return(names(config$api_server)[[1]])
  }
  default_remote_path <- Sys.getenv("ORDERLY_DEFAULT_REMOTE_PATH", NA_character_)
  if (!is.na(default_remote_path)) {
    return(orderly_remote_path(default_remote_path))
  }
  stop("default remote has not been set yet: use 'orderly::set_default_remote'")
}


get_remote <- function(remote, config) {
  remote %||% get_default_remote(config)
}
