orderly_remote_path <- function(path) {
  assert_file_exists(path)
  path <- normalizePath(path, mustWork = TRUE)
  if (!file.exists(path_orderly_config_yml(path))) {
    stop("Does not look like an orderly repository: ", squote(path))
  }
  structure(path,
            class = c("orderly_remote_path",
                      "orderly_remote_location"))
}


pull_archive_path <- function(name, id, config, from, overwrite = FALSE) {
  ## This is a little awkward because we need to read the remote
  ## configuration but don't want to get involved with any of the auth
  ## stuff there.  Ignore that for now and see how bad that is in
  ## reality.  It's quite likely we can use .Rprofile remotely and
  ## orderly_envir.yml locally to make this behave ok, and if we use
  ## vault for password management that should also work ok.  However,
  ## practically all we really need here is the path.
  config_remote <- orderly_config(from)

  if (id == "latest") {
    id <- orderly_latest(name, config_remote, FALSE)
  }

  src <- file.path(path_archive(config_remote$path), name, id)
  dest <- file.path(path_archive(config$path), name, id)

  if (!file.exists(file.path(src, "orderly.yml"))) {
    stop(sprintf("Did not find archived report at '%s'",
                 src))
  }

  if (file.exists(dest)) {
    orderly_log("pull", sprintf("%s:%s already exists, skipping", name, id))
  } else {
    orderly_log("pull", sprintf("%s:%s", name, id))
    dest_dir <- dirname(dest)
    dir.create(dest_dir, FALSE, TRUE)
    ## This little dance means that in the event of a failure we don't
    ## get partial copies.
    on.exit(unlink(dest, recursive = TRUE))
    ok <- file.copy(src, dest_dir, recursive = TRUE)
    if (!ok) {
      stop("Some sort of error copying %s => %s", src, dest)
    }
    on.exit()
  }
}
