## Communication with the API.  This is almost impossible to test
## without a working copy of the montagu reporting api.  I guess the
## simplest solution will be to have a copy running on support that we
## can point this at.

##' Download dependent reports from Montagu.  This requires the
##' montagu package and for montagu's credentials to be correctly set
##' up.  The \code{pull_archive} function pulls report directly
##' (without it being a dependent report).
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
##' @inheritParams orderly_list
##' @export
pull_dependencies <- function(name, config = NULL, locate = TRUE) {
  config <- orderly_config_get(config, locate)

  ## This is going to require use of montagu's API.  Later this will
  ## get split into bits that are more orderly specific.
  loadNamespace("montagu")

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
      pull_report(names(depends)[[i]], depends[[i]]$id, config)
    }
  }
}

##' @export
##' @rdname pull_dependencies
##'
##' @param id The identifier (for \code{pull_archive}.  The default is
##'   to pull the latest report.
pull_archive <- function(name, id = "latest", config = NULL, locate = TRUE) {
  config <- orderly_config_get(config, locate)
  pull_report(name, id, config)
}

pull_report <- function(name, id, config) {
  assert_is(config, "orderly_config")
  if (id == "latest") {
    ## Resolve id
    v <- montagu::montagu_reports_report_versions(name)
    ## TODO: more work needed here if we have two identical timestamps!
    id <- last(v)
  }
  dest <- file.path(path_archive(config$path), name, id)
  if (file.exists(dest)) {
    orderly_log("pull", sprintf("%s:%s already exists, skipping", name, id))
  } else {
    orderly_log("pull", sprintf("%s:%s", name, id))
    tmp <- montagu::montagu_reports_report_download(name, id)
    cat("\n") # httr's progress bar is rubbish
    on.exit(file.remove(tmp))
    tmp2 <- tempfile()
    code <- utils::unzip(tmp, exdir = tmp2)
    on.exit(unlink(tmp2, recursive = TRUE), add = TRUE)

    ## R's file.copy is exceedingly rubbish
    dir.create(dirname(dest), FALSE, TRUE)
    file.copy(file.path(tmp2, basename(dest)),
              dirname(dest), recursive = TRUE)
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
##' @param server Name of the montagu server to use.  Optionas are
##'   \code{production}, \code{science} and \code{uat}.
##'
##' @export
orderly_run_remote <- function(name, parameters = NULL, ref = NULL,
                               timeout = 3600, poll = 1,
                               open = TRUE, stop_on_error = TRUE,
                               progress = TRUE, server = NULL) {
  loadNamespace("montagu")
  if (server == "production" && !is.null(ref)) {
    stop("Can't specify 'ref' on production")
  }
  montagu::montagu_reports_run(name, parameters = parameters, ref = ref,
                               timeout = timeout, poll = poll,
                               open = open, stop_on_error = stop_on_error,
                               progress = progress, location = server)
}
