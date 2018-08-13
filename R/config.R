orderly_config <- function(path) {
  filename <- path_orderly_config_yml(path)
  if (!file.exists(filename)) {
    stop("Did not find file 'orderly_config.yml' at path ", path)
  }
  withr::with_envvar(
    orderly_envir_read(path),
    orderly_config_read_yaml(filename, path))
}

orderly_config_read_yaml <- function(filename, path) {
  info <- yaml_read(filename)
  check_fields(info, filename, "source",
               c("destination", "fields", "minimum_orderly_version",
                 "api_server", "vault_server"))

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
    list(driver = driver, args = args)
  }

  info$fields <- config_check_fields(info$fields, filename)
  info$source <- driver_config("source")
  info$destination <- driver_config("destination")

  v <- info$minimum_orderly_version
  if (!is.null(v) && utils::packageVersion("orderly") < v) {
    stop(sprintf(
      "Orderly version '%s' is required, but only '%s' installed",
      v, utils::packageVersion("orderly")))
  }

  if (!is.null(info$vault_server)) {
    assert_scalar_character(info$vault_server,
                            sprintf("%s:vault_server", filename))
  }

  api_server <- info$api_server
  if (!is.null(api_server)) {
    info$api_server <- config_check_api_server(api_server, filename)
  }

  info$path <- normalizePath(path, mustWork = TRUE)
  info$archive_version <- read_orderly_archive_version(path)
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
                 c("required", "type"), "description")
    assert_scalar_logical(d$required,
                          sprintf("%s:fields:%s:required", filename, nm))
    assert_scalar_character(d$type,
                            sprintf("%s:fields:%s:type", filename, nm))
    if (is.null(d$description)) {
      d$description <- NA_character_
    } else {
      assert_scalar_character(d$description,
                              sprintf("%s:fields:%s:description", filename, nm))
    }
    d$type_sql <- sql_type(d$type, sprintf("%s:fields:%s:type", filename, nm))
    d
  }
  dat <- lapply(names(x), check1)
  data.frame(name = names(x),
             required = vlapply(dat, "[[", "required"),
             type = vcapply(dat, "[[", "type"),
             type_sql = vcapply(dat, "[[", "type_sql"),
             description = vcapply(dat, "[[", "description"),
             stringsAsFactors = FALSE)
}

config_check_api_server <- function(dat, filename) {
  if (is.null(dat)) {
    return(NULL)
  }

  assert_named(dat, unique = TRUE)

  check1 <- function(name) {
    server <- dat[[name]]
    check_fields(server,
                 sprintf("%s:api_server:%s", filename, name),
                 c("host", "port"),
                 c("basic", "username", "password"))
    check_field <- function(nm, required, fn) {
      x <- server[[nm]]
      if (required || !is.null(x)) {
        fn(x, sprintf("%s:api_server:%s:%s", filename, name, nm))
      }
    }

    server <- resolve_env(server, error = FALSE)
    if (is.null(server$basic)) {
      server$basic <- FALSE
    } else {
      check_field("basic", TRUE, assert_scalar_logical)
    }
    ## check_field("port", TRUE, assert_scalar_integer)
    check_field("host", TRUE, assert_scalar_character)
    check_field("username", FALSE, assert_scalar_character)
    check_field("password", FALSE, assert_scalar_character)

    if (requireNamespace("montagu", quietly = TRUE)) {
      ret <- montagu::montagu_server(
        name, server$host, server$port, server$basic,
        server$username, server$password)
    } else {
      ret <- NULL
    }
    ret
  }

  set_names(lapply(names(dat), check1), names(dat))
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
