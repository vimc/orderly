orderly_config <- function(root) {
  filename <- path_orderly_config_yml(root)
  if (!file.exists(filename)) {
    stop("Did not find file 'orderly_config.yml' at path ", root)
  }
  withr::with_envvar(
    orderly_envir_read(root),
    orderly_config_read_yaml(filename, root))
}

orderly_config_read_yaml <- function(filename, root) {
  info <- yaml_read(filename)
  check_fields(info, filename, character(),
               c("destination", "fields", "minimum_orderly_version",
                 "remote", "vault", "vault_server", "global_resources",
                 "changelog", "source", "database"))

  ## There's heaps of really boring validation to do here that I am
  ## going to skip.  The drama that we will have is that there are
  ## things that should be interpreted relative to different
  ## directories; things like the sqlite path.  And I want to be able
  ## to encrypt and decrypt things like passwords.  So we probably
  ## cannot be totally opaque when reading information in.
  info$fields <- config_check_fields(info$fields, filename)
  if (!is.null(info$source)) {
    if (!is.null(info$database)) {
      stop("Both 'database' and 'source' fields may not be used")
    }
    msg <- c("Use of 'source' is deprecated and will be removed in a",
             "future orderly version - please use 'database' instead.",
             "See the main package vignette for details.")
    orderly_warning(flow_text(msg))
    info$database_old_style <- TRUE
    info$database <- list(source = config_read_db("source", info, filename))
  } else if (!is.null(info$database)) {
    assert_named(info$database, unique = TRUE)
    info$database_old_style <- FALSE
    for (nm in names(info$database)) {
      info$database[[nm]] <- config_read_db(c("database", nm), info, filename)
    }
  }
  info$destination <- config_read_db("destination", info, filename)

  if (!is.null(info$changelog)) {
    info$changelog <- config_check_changelog(info$changelog, filename)
  }

  v <- info$minimum_orderly_version
  if (!is.null(v) && utils::packageVersion("orderly") < v) {
    stop(sprintf(
      "Orderly version '%s' is required, but only '%s' installed",
      v, utils::packageVersion("orderly")))
  }

  info$vault <- config_check_vault(info[['vault']], info[['vault_server']], filename)
  info$remote <- config_check_remote(info$remote, filename)

  info$root <- normalizePath(root, mustWork = TRUE)

  remote_identity <- Sys.getenv("ORDERLY_API_SERVER_IDENTITY", "")
  if (nzchar(remote_identity)) {
    info$remote_identity <-
      match_value(remote_identity, names(info$remote))
    excl <- c("driver", "args", "name")
    server_options <- info$remote[[remote_identity]]
    info$server_options <- server_options[setdiff(names(server_options), excl)]
  }

  if (!is.null(info$global_resources)) {
    assert_is_directory(info$global_resources, name = "global resource",
                        workdir = root)
  }

  info$archive_version <- read_orderly_archive_version(root)

  class(info) <- "orderly_config"
  info
}

config_check_fields <- function(x, filename) {
  if (is.null(x)) {
    return(data.frame(name = character(0), required = logical(0),
                      stringsAsFactors = FALSE))
  }
  assert_named(x, TRUE, sprintf("%s:fields", filename))
  check1 <- function(nm) {
    d <- x[[nm]]
    ## TODO: See VIMC-2930; "type" can be removed once the reports are
    ## updated, but it's best to do that in a staged way (deploy
    ## VIMC-2768, remove entries from the montagu-reports, then remove
    ## the entry here).
    check_fields(d, sprintf("%s:fields:%s", filename, nm),
                 "required", c("description", "type"))
    assert_scalar_logical(d$required,
                          sprintf("%s:fields:%s:required", filename, nm))
    if (is.null(d$description)) {
      d$description <- NA_character_
    } else {
      assert_scalar_character(d$description,
                              sprintf("%s:fields:%s:description", filename, nm))
    }
    d
  }
  dat <- lapply(names(x), check1)
  data.frame(name = names(x),
             required = vlapply(dat, "[[", "required"),
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
      msg <- c("The 'url' field (used in",
               sprintf("%s:remote:%s", filename, name),
               "is deprecated and will be dropped in a future version of",
               "orderly.  Please remove it from your orderly_config.yml")
      orderly_warning(flow_text(msg))
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


orderly_locate_config <- function() {
  root <- find_file_descend("orderly_config.yml")
  if (is.null(root)) {
    stop("Reached root without finding 'orderly_config.yml'")
  }
  orderly_config(root)
}


orderly_config_get <- function(x, locate = FALSE) {
  if (inherits(x, "orderly_config")) {
    x
  } else if (is.null(x) && locate) {
    orderly_locate_config()
  } else if (is.character(x)) {
    orderly_config(x)
  } else {
    stop("Invalid input")
  }
}


config_read_db <- function(name, info, filename) {
  if (identical(name, "destination") && is.null(info[[name]])) {
    dat <- list(driver = "RSQLite::SQLite",
                args = list(dbname = "orderly.sqlite"))
  } else {
    dat <- info[[name]]
  }
  label <- sprintf("%s:%s:driver", filename, paste(name, collapse = ":"))
  driver <- check_symbol_from_str(dat$driver, label)
  instances <- NULL

  ## There's a bit of faff required here now - this would be simpler
  ## if we dropped old-style databases (vimc-3315) but that's
  ## awkward in its own right because it affects the configuration
  ## during migration tests.
  if (any(c("args", "instances") %in% names(dat))) {
    if (identical(name, "destination")) {
      optional <- "args"
    } else {
      optional <- c("args", "instances", "default_instance")
    }
    label <- sprintf("%s:%s", filename, paste(name, collapse = ":"))
    check_fields(dat, label, "driver", optional)

    if (!is.null(dat$instances)) {
      assert_named(dat$instances, TRUE, paste0(label, ":instances"))
      for (i in names(dat$instances)) {
        assert_named(dat$instances[[i]], TRUE, paste0(label, ":instances:", i))
      }
      base <- dat$args %||% set_names(list(), character())
      assert_named(base, TRUE, paste0(label, ":args"))
      instances <- lapply(dat$instances, utils::modifyList, x = base)
    } else {
      assert_named(dat$args, TRUE, paste0(label, ":args"))
    }

    if (!is.null(dat$default_instance)) {
      if (is.null(instances)) {
        msg <- c(
          "Can't specify 'default_instance' with no defined instances in",
          label)
        stop(flow_text(msg), call. = FALSE)
      }
      dat["default_instance"] <-
        resolve_env(dat["default_instance"], error = FALSE, default = NULL)
      if (!is.null(dat$default_instance)) {
        match_value(dat$default_instance, names(instances),
                    paste0(label, ":default_instance"))
      }
    }

    if (is.null(instances)) {
      args <- dat$args
    } else {
      args <- instances[[dat$default_instance %||% 1L]]
    }
  } else {
    if (!info$database_old_style) {
      msg <- c("Please move your database arguments within an 'args'",
               "block, as detecting them will be deprecated in a future",
               "orderly version.  See the main package vignette for",
               "details.  Reported for: ", label)
      orderly_warning(flow_text(msg))
    }
    args <- dat[setdiff(names(dat), "driver")]
  }

  list(driver = driver, args = args, instances = instances)
}


config_check_vault <- function(vault, vault_server, filename) {
  if (!is.null(vault_server)) {
    if (!is.null(vault)) {
      stop(sprintf("Can't specify both 'vault' and 'vault_server' in %s",
                   filename))
    }
    msg <- c("Use of 'vault_server' is deprecated and will be removed in a",
             "future orderly version.  Please use the new 'vault' server",
             "field, which offers more flexibility")
    orderly_warning(flow_text(msg))
    assert_scalar_character(vault_server, sprintf("%s:vault_server", filename))
    vault <- list(addr = vault_server)
  }
  if (!is.null(vault)) {
    assert_named(vault, TRUE, sprintf("%s:vault", filename))
  }

  vault
}
