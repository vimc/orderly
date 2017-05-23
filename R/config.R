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

    if (info[[name]]$driver == "RSQLite::SQLite") {
      ## TODO: error on empty string or ":memory:"
      ## TODO: don't join path on absolute path
      args$dbname <- file.path(normalizePath(path, mustWork = TRUE),
                               args$dbname)
    }
    list(driver = driver, args = args)
  }

  info$source <- driver_config("source")
  info$destination <- driver_config("destination")

  info$path <- path
  class(info) <- "orderly_config"
  info
}

## package level stuff; we need to arrange to try and find the
## appropriate configuration.
orderly_default_config_set <- function(x) {
  if (!is.null(x)) {
    assert_is(x, "orderly_config")
  }
  options(orderly.config = x)
}

orderly_default_config <- function(locate = FALSE) {
  cfg <- getOption("orderly.config")
  if (is.null(cfg)) {
    if (locate) {
      path <- find_file_descend("orderly_config.yml")
      if (is.null(path)) {
        stop("Reached root without finding 'orderly_config.yml'")
      }
      cfg <- config_read(path)
    } else {
      stop("orderly configuration not found")
    }
  }
  cfg
}

orderly_get_config <- function(x, locate) {
  if (inherits(x, "orderly_config")) {
    x
  } else if (is.null(x)) {
    orderly_default_config(locate)
  } else if (is.character(x)) {
    config_read(x)
  } else {
    stop("Invalid input")
  }
}

orderly_init <- function(root, doc = TRUE) {
  if (file.exists(root)) {
    if (!file.info(root)$isdir || length(dir(root)) > 0) {
      stop("'root', if it already exists, must be an empty directory")
    }
  } else {
    dir.create(root, FALSE, TRUE)
  }
  dir.create(file.path(root, "data"))
  dir.create(file.path(root, "src"))
  dir.create(file.path(root, "archive"))
  if (doc) {
    readme <- function(path) {
      file.path(root, path, "README.md")
    }
    file.copy(orderly_file("readme_src.md"), readme("src"))
    file.copy(orderly_file("readme_data.md"), readme("data"))
    file.copy(orderly_file("readme_archive.md"), readme("archive"))
    file.copy(orderly_file("readme_root.md"),
              file.path(root, "README.md"))
  }
  file.copy(orderly_file("orderly_config_example.yml"),
            file.path(root, "orderly_config.yml"))
  message(sprintf("Now, edit the file 'orderly_root.yml' within '%s'", root))
  root
}

orderly_db <- function(type, config = NULL, locate = TRUE) {
  config <- orderly_get_config(config, locate)
  if (type == "rds") {
    file_store_rds(path_rds(config$path))
  } else if (type == "csv") {
    file_store_rds(path_csv(config$path))
  } else if (type %in% c("source", "destination")) {
    x <- config[[type]]
    driver <- getExportedValue(x$driver[[1L]], x$driver[[2L]])
    do.call(DBI::dbConnect, c(list(driver()), x$args))
  } else {
    stop(sprintf("Invalid db type '%s'", type))
  }
}
c
