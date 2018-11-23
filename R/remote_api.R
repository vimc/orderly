## Communication with the API.  This is almost impossible to test
## without a working copy of the montagu reporting api.  I guess the
## simplest solution will be to have a copy running on support that we
## can point this at.
remote_report_pull_archive_api <- function(name, id, config, remote) {
  tmp <- montagu::montagu_reports_report_download(name, id, location = remote)
  cat("\n") # httr's progress bar is rubbish
  on.exit(file.remove(tmp))
  unzip_archive(tmp, config$path, name, id)
}


orderly_run_remote_api <- function(name, config, parameters = NULL, ref = NULL,
                                   timeout = 3600, poll = 1,
                                   open = TRUE, stop_on_error = TRUE,
                                   progress = TRUE, remote = NULL) {
  ## TODO: this should come out and be replaced by control at the
  ## level of the *server*.
  if (remote$name == "production" && !is.null(ref)) {
    stop("Can't specify 'ref' on production")
  }
  montagu::montagu_reports_run(name, parameters = parameters, ref = ref,
                               timeout = timeout, poll = poll,
                               open = open, stop_on_error = stop_on_error,
                               progress = progress, location = remote)
}


orderly_publish_remote_api <- function(name, id, config, value = TRUE,
                                       remote = NULL) {
  montagu::montagu_reports_publish(name, id, value, remote)
}


remote_report_names_api <- function(remote) {
  montagu::montagu_reports_list(remote)$name
}


remote_report_versions_api <- function(name, remote) {
  montagu::montagu_reports_report_versions(name, error_if_missing = FALSE,
                                           location = remote)
}


## This works around a series of failure modes in unpacking an archive
## that tries to minimise the chance that an invalid archive is
## unpacked.
unzip_archive <- function(zip, root, name, id) {
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE))
  res <- utils::unzip(zip, exdir = tmp)

  files <- dir(tmp, all.files = TRUE, no.. = TRUE)
  if (length(files) == 0L) {
    stop("Corrupt zip file? No files extracted")
  } else if (length(files) > 1L) {
    stop("Invalid orderly archive", call. = FALSE)
  }
  if (files != id) {
    stop(sprintf("This is archive '%s' but expected '%s'",
                 files, id), call. = FALSE)
  }

  expected <- c("orderly.yml", "orderly_run.yml", "orderly_run.rds")
  msg <- !file.exists(file.path(tmp, id, expected))
  if (any(msg)) {
    stop(sprintf("Invalid orderly archive: missing files %s",
                 paste(expected[msg], collapse = ", ")), call. = FALSE)
  }

  ## R's file.copy is exceedingly rubbish
  dest <- file.path(root, "archive", name)
  dir.create(dest, FALSE, TRUE)
  file_copy(file.path(tmp, id), dest, recursive = TRUE)
}


check_remote_api_server <- function(config, remote) {
  server <- remote$server
  if (is.null(server)) {
    stop("The 'montagu' package is required to use an api server")
  }

  loadNamespace("montagu")
  assert_is(config, "orderly_config")

  if (!server$is_authorised()) {
    ## TODO: this used to cache the vault client; that was the job of
    ## the vault package I think, but it doesn't matter here because
    ## we rewrite the values.
    ##
    ## TODO: This looks confised to me - why do we go through
    ## resolve_secrets 3 times?
    auth <- resolve_secrets(
      list(username = server$username, password = server$password),
      config)
    server$username <- resolve_secrets(auth$username, config)
    server$password <- resolve_secrets(auth$password, config)
  }

  server
}
