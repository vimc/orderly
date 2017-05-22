## There are going to be so many configuration options that we need to
## make another file to hold them I think.  The basic organisation can
## be such:
config_read <- function(path) {
  filename <- file.path(path, "orderly_config.yml")
  if (!file.exists(filename)) {
    stop("Did not find file 'orderly_config.yml' at path ", path)
  }
  config_read_yaml(filename, path)
}

config_read_yaml <- function(filename, path) {
  info <- yaml_read(filename)
  required <- c("source", "destination")
  msg <- setdiff(required, names(info))
  if (length(msg) > 0L) {
    stop(sprintf("Fields missing from %s: %s",
                 filename, paste(msg, collapse = ", ")))
  }

  ## There's heaps of really boring validation to do here that I am
  ## going to skip.  The drama that we will have is that there are
  ## things that should be interpreted relative to different
  ## directories; things like the sqlite path.  And I want to be able
  ## to encrypt and decrypt things like passwords.  So we probably
  ## cannot be totally opaque when reading information in.

  driver_config <- function(name) {
    driver <- check_symbol_from_str(info[[name]]$driver,
                                    sprintf("%s:%s:driver", filename, name))
    args <- info[[name]][setdiff(names(info[[name]]), "driver")]
    list(driver = driver, args = args)
  }

  info$source <- driver_config("source")
  info$destination <- driver_config("destination")

  info$path <- path
  class(info) <- "orderly_config"
  info
}
