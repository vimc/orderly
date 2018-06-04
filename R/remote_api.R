## Communication with the API.  This is almost impossible to test
## without a working copy of the montagu reporting api.  I guess the
## simplest solution will be to have a copy running on support that we
## can point this at.
pull_archive_api <- function(name, id, config, remote) {
  loadNamespace("montagu")
  assert_is(config, "orderly_config")
  orderly_remote_resolve_secrets(config, remote)

  v <- withCallingHandlers(
    montagu::montagu_reports_report_versions(name, remote),
    error = function(e) {
      valid <- montagu::montagu_reports_list(remote)
      if (!(name %in% valid$name)) {
        stop(sprintf("No versions of report '%s' found at '%s'",
                     name, remote$name),
             call. = FALSE)
      }
    })

  if (id == "latest") {
    ## Resolve id
    v <- montagu::montagu_reports_report_versions(name, remote)
    ## TODO: more work needed here if we have two identical timestamps!
    id <- latest_id(v)
  } else if (!(id %in% v)) {
    ## Confirm that the report does actually exist, working around
    ## VIMC-1281:
    stop(sprintf(
      "Version '%s' not found at '%s': valid versions are:\n%s",
      id, remote$name, paste(sprintf("  - %s", v), collapse = "\n")),
      call. = FALSE)
  }
  dest <- file.path(path_archive(config$path), name, id)
  if (file.exists(file.path(dest, "orderly_run.yml"))) {
    orderly_log("pull", sprintf("%s:%s already exists, skipping", name, id))
  } else {
    orderly_log("pull", sprintf("%s:%s", name, id))
    tmp <- montagu::montagu_reports_report_download(name, id,
                                                    location = remote)
    cat("\n") # httr's progress bar is rubbish
    on.exit(file.remove(tmp))
    unzip_archive(tmp, config$path, name, id)
  }
}


orderly_run_remote_api <- function(name, config, parameters = NULL, ref = NULL,
                                   timeout = 3600, poll = 1,
                                   open = TRUE, stop_on_error = TRUE,
                                   progress = TRUE, remote = NULL) {
  assert_is(config, "orderly_config")
  loadNamespace("montagu")
  orderly_remote_resolve_secrets(config, remote)

  if (remote == "production" && !is.null(ref)) {
    stop("Can't specify 'ref' on production")
  }
  montagu::montagu_reports_run(name, parameters = parameters, ref = ref,
                               timeout = timeout, poll = poll,
                               open = open, stop_on_error = stop_on_error,
                               progress = progress, location = remote)
}


orderly_publish_remote_api <- function(name, id, config, value = TRUE,
                                       remote = NULL) {
  assert_is(config, "orderly_config")
  ## This one can actually be done over disk too
  loadNamespace("montagu")
  orderly_remote_resolve_secrets(config, remote)
  assert_scalar_character(name)
  assert_scalar_character(id)
  assert_scalar_logical(value)
  montagu::montagu_reports_publish(name, id, value, remote)
}


orderly_remote_resolve_secrets <- function(config, remote) {
  ## Look up the secrets in the vault: this might move into montagu,
  ## because then this gets heaps easier
  if (!remote$is_authorised()) {
    ## TODO: this used to cache the vault client; that was the job of
    ## the vault package I think, but it doesn't matter here because
    ## we rewrite the values.
    auth <- resolve_secrets(list(username = remote$username,
                                 password = remote$password),
                            config)
    remote$username <- resolve_secrets(auth$username, config)
    remote$password <- resolve_secrets(auth$password, config)
  }
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
