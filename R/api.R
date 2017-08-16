## Communication with the API.  This is almost impossible to test
## without a working copy of the montagu reporting api.  I guess the
## simplest solution will be to have a copy running on support that we
## can point this at.

##' Download dependent reports from Montagu.  This requires the
##' montagu package and for montagu's credentials to be correctly set
##' up.
##' @title Download dependent reports
##' @param name Name of the report to download dependencies for
##' @inheritParams orderly_query
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

pull_report <- function(name, id, config) {
  assert_is(config, "orderly_config")
  if (id == "latest") {
    ## Resolve id
    v <- montagu::montagu_reports_report_versions(name)
    ## TODO: more work needed here if we have two identical timestamps!
    id <- tail(v, 1)
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
    code <- unzip(tmp, exdir = tmp2)
    on.exit(unlink(tmp2, recursive = TRUE), add = TRUE)

    ## R's file.copy is exceedingly rubbish
    dir.create(dirname(dest), FALSE, TRUE)
    file.copy(file.path(tmp2, basename(dest)),
              dirname(dest), recursive = TRUE)
  }
}
