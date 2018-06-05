## Communication with the API.  This is almost impossible to test
## without a working copy of the montagu reporting api.  I guess the
## simplest solution will be to have a copy running on support that we
## can point this at.
pull_archive_api <- function(name, id, config, remote) {
  loadNamespace("montagu")
  assert_is(config, "orderly_config")
  orderly_remote_resolve_secrets(config, remote)
  if (id == "latest") {
    ## Resolve id
    v <- montagu::montagu_reports_report_versions(name, remote)
    ## TODO: more work needed here if we have two identical timestamps!
    id <- last(v)
  }
  dest <- file.path(path_archive(config$path), name, id)
  if (file.exists(dest)) {
    orderly_log("pull", sprintf("%s:%s already exists, skipping", name, id))
  } else {
    orderly_log("pull", sprintf("%s:%s", name, id))
    tmp <- montagu::montagu_reports_report_download(name, id,
                                                    location = remote)
    cat("\n") # httr's progress bar is rubbish
    on.exit(file.remove(tmp))
    tmp2 <- tempfile()
    code <- utils::unzip(tmp, exdir = tmp2)
    on.exit(unlink(tmp2, recursive = TRUE), add = TRUE)

    ## R's file.copy is exceedingly rubbish
    dir.create(dirname(dest), FALSE, TRUE)
    file_copy(file.path(tmp2, basename(dest)),
              dirname(dest), recursive = TRUE)
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
