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
                 "remote", "vault_server", "global_resources",
                 "changelog"))

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

  if (!is.null(info$changelog)) {
    info$changelog <- config_check_changelog(info$changelog, filename)
  }

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

  info$remote <- config_check_remote(info$remote, filename)

  info$path <- normalizePath(path, mustWork = TRUE)

  remote_identity <- Sys.getenv("ORDERLY_API_SERVER_IDENTITY", "")
  if (nzchar(remote_identity)) {
    info$remote_identity <-
      match_value(remote_identity, names(info$remote))
  }

  if (!is.null(info$global_resources)) {
    assert_is_directory(info$global_resources, name = "global resource",
                        workdir = path)
  }

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


config_check_remote <- function(dat, filename) {
  if (is.null(dat)) {
    return(NULL)
  }
  assert_named(dat, unique = TRUE)

  check1 <- function(name) {
    remote <- dat[[name]]
    check_fields(remote, sprintf("%s:remote:%s", filename, name),
                 c("driver", "args"),
                 c("url", "slack_url", "primary", "master_only"))
    field_name <- function(nm) {
      sprintf("%s:remote:%s:%s", filename, name, nm)
    }
    assert_scalar_character(remote$driver, field_name("driver"))
    assert_named(remote$args, name = field_name("args"))
    remote <- resolve_env(remote, error = FALSE)
    remote$args <- resolve_env(remote$args, error = FALSE, default = NULL)

    ## optionals:
    if (!is.null(remote$url)) {
      assert_scalar_character(remote$url, field_name("url"))
      remote$url <- sub("/$", "", remote$url)
    }
    if (!is.null(remote$slack_url)) {
      assert_scalar_character(remote$slack_url, field_name("slack_url"))
    }
    if (is.null(remote$primary)) {
      remote$primary <- FALSE
    } else {
      assert_scalar_logical(remote$primary, field_name("primary"))
    }
    if (is.null(remote$master_only)) {
      remote$master_only <- FALSE
    } else {
      assert_scalar_logical(remote$master_only, field_name("master_only"))
    }

    remote$driver <- check_symbol_from_str(remote$driver, field_name("driver"))
    remote$args <- c(remote$args, list(name = name))
    remote$name <- name
    remote
  }

  ret <- set_names(lapply(names(dat), check1), names(dat))
  primary <- vlapply(ret, "[[", "primary")
  if (sum(primary) > 1L) {
    stop(sprintf(
      "At most one remote can be listed as primary but here %d are: %s",
      sum(primary), paste(squote(names(which(primary))), collapse = ", ")),
      call. = FALSE)
  }
  ret
}

config_check_changelog <- function(x, filename) {
  assert_named(x, unique = TRUE, sprintf("%s:changelog", filename))
  for (i in names(x)) {
    assert_scalar_logical(
      x[[i]]$public,
      sprintf("%s:changelog:%s:public", filename, i))
  }

  data_frame(id = names(x),
             public = vlapply(x, function(x) x$public, USE.NAMES = FALSE))
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
