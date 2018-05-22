orderly_api_server <- function(location) {
  structure(list(location = location),
            class = "orderly_api_server")
}

## Communication with the API.  This is almost impossible to test
## without a working copy of the montagu reporting api.  I guess the
## simplest solution will be to have a copy running on support that we
## can point this at.
pull_archive_api <- function(name, id, config, remote) {
  loadNamespace("montagu")
  assert_is(config, "orderly_config")
  remote <- orderly_remote_api_server(config, remote)
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
    file.copy(file.path(tmp2, basename(dest)),
              dirname(dest), recursive = TRUE)
  }
}


orderly_run_remote_api <- function(name, parameters = NULL, ref = NULL,
                                   timeout = 3600, poll = 1,
                                   open = TRUE, stop_on_error = TRUE,
                                   progress = TRUE, remote = NULL) {
  loadNamespace("montagu")
  remote <- orderly_remote_api_server(config, remote)

  if (remote == "production" && !is.null(ref)) {
    stop("Can't specify 'ref' on production")
  }
  montagu::montagu_reports_run(name, parameters = parameters, ref = ref,
                               timeout = timeout, poll = poll,
                               open = open, stop_on_error = stop_on_error,
                               progress = progress, location = remote)
}


orderly_publish_remote_api <- function(name, id, value = TRUE, remote = NULL) {
  ## This one can actually be done over disk too
  loadNamespace("montagu")
  remote <- orderly_remote_api_server(config, remote)
  assert_scalar_character(name)
  assert_scalar_character(id)
  assert_scalar_logical(value)
  montagu::montagu_reports_publish(name, id, value, remote)
}


orderly_remote_api_server <- function(config, remote) {
  api_server <- config$api_server
  if (is.null(api_server)) {
    ## If there is no api_server section then none of this matters at all.
    return()
  }

  ## This is the server that we're going to go for
  remote <- monagu::montagu_location(remote %||% names(api_server)[[1L]])

  ## Look up the secrets in the vault
  server_data <- resolve_secrets(api_server[[remote]], config$vault_server)

  ## Then set the username/password varibles so that they can be found
  ## easily:
  for (nm in c("username", "password")) {
    key <- sprintf("montagu.%s.%s", remote, nm)
    if (!is.null(server_data[[nm]]) && is.null(getOption(key))) {
      do.call("options", set_names(server_data[nm], key))
    }
  }

  remote
}
