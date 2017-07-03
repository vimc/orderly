orderly_config <- function(path) {
  filename <- path_orderly_config_yml(path)
  if (!file.exists(filename)) {
    stop("Did not find file 'orderly_config.yml' at path ", path)
  }
  orderly_config_read_yaml(filename, path)
}

orderly_config_read_yaml <- function(filename, path) {
  info <- yaml_read(filename)
  check_fields(info, filename, "source", c("destination", "fields"))

  ## There's heaps of really boring validation to do here that I am
  ## going to skip.  The drama that we will have is that there are
  ## things that should be interpreted relative to different
  ## directories; things like the sqlite path.  And I want to be able
  ## to encrypt and decrypt things like passwords.  So we probably
  ## cannot be totally opaque when reading information in.

  driver_config <- function(name) {
    if (name == "destination" && is.null(info[[name]])) {
      info[[name]] <- list(driver = "RSQLite::SQLite",
                           dbname = "orderly.sqlite")
    }
    driver <- check_symbol_from_str(info[[name]]$driver,
                                    sprintf("%s:%s:driver", filename, name))
    args <- info[[name]][setdiff(names(info[[name]]), "driver")]

    if (info[[name]]$driver == "RSQLite::SQLite") {
      dbname <- args$dbname
      if (!nzchar(dbname) || tolower(dbname) == ":memory:") {
        stop("Cannot use a transient SQLite database with orderly")
      }
      if (is_relative_path(args$dbname)) {
        args$dbname <- file.path(normalizePath(path, mustWork = TRUE),
                                 args$dbname)
      }
    }
    list(driver = driver, args = args)
  }

  info$fields <- config_check_fields(info$fields, filename)
  info$source <- driver_config("source")
  info$destination <- driver_config("destination")

  info$path <- path
  class(info) <- "orderly_config"
  info
}

config_check_fields <- function(x, filename) {
  if (is.null(x)) {
    return(data.frame(name = character(0), required = logical(0),
                      type = character(0), type_sql = character(0),
                      stringsAsFactors = FALSE))
  }
  types <- c("character", "numeric")
  assert_named(x, TRUE, sprintf("%s:fields", filename))
  check1 <- function(nm) {
    d <- x[[nm]]
    check_fields(d, sprintf("%s:fields:%s", filename, nm),
                 c("required", "type"), NULL)
    assert_scalar_logical(d$required,
                          sprintf("%s:fields:%s:required", filename, nm))
    assert_scalar_character(d$type,
                            sprintf("%s:fields:%s:type", filename, nm))
    d$type_sql <- sql_type(d$type, sprintf("%s:fields:%s:type", filename, nm))
    d
  }
  dat <- lapply(names(x), check1)
  data.frame(name = names(x),
             required = vlapply(dat, "[[", "required"),
             type = vcapply(dat, "[[", "type"),
             type_sql = vcapply(dat, "[[", "type_sql"),
             stringsAsFactors = FALSE)
}

sql_type <- function(type, name) {
  tr <- c(numeric = "DECIMAL",
          character = "TEXT")
  tr[[match_value(type, names(tr), name)]]
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
      cfg <- orderly_locate_config()
    } else {
      stop("orderly configuration not found")
    }
  }
  cfg
}

orderly_locate_config <- function() {
  path <- find_file_descend("orderly_config.yml")
  if (is.null(path)) {
    stop("Reached root without finding 'orderly_config.yml'")
  }
  orderly_config(path)
}

orderly_config_get <- function(x, locate) {
  if (inherits(x, "orderly_config")) {
    x
  } else if (is.null(x)) {
    orderly_default_config(locate)
  } else if (is.character(x)) {
    orderly_config(x)
  } else {
    stop("Invalid input")
  }
}
