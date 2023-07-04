##' Retrieve orderly config object.
##'
##' @inheritParams orderly_list
##'
##' @return An R6 object representing the orderly config.
##' @export
##'
##' @examples
##' # The orderly demo, with lots of potential reports:
##' path <- orderly1::orderly_example("demo")
##'
##' orderly1::orderly_config(path)
orderly_config <- function(root = NULL, locate = TRUE) {
  if (inherits(root, "orderly_config")) {
    root
  } else if (is.null(root) && locate) {
    orderly_locate_config()
  } else if (is.character(root)) {
    orderly_config_$new(root)
  } else {
    stop("Invalid input")
  }
}

orderly_locate_config <- function() {
  root <- find_file_descend("orderly_config.yml")
  if (is.null(root)) {
    stop("Reached root without finding 'orderly_config.yml'")
  }
  orderly_config_$new(root)
}

##'
##' @rdname orderly_config
orderly_config_ <- R6::R6Class(
  "orderly_config",
  cloneable = FALSE,

  public = list(
    ## The core data will remain available as top-level keys, but
    ## we'll make this extensible when we do the plugins.
    ##' @field root Root dir of the orderly repository
    root = NULL,
    ##' @field raw The raw orderly config yaml
    raw = NULL,

    ##' @field destination DB connection configuration for where
    ##' to store orderly output database. Defaults to local SQLite
    ##' db `orderly.sqlite`
    destination = NULL,
    ##' @field fields Configuration of fields in reports, specifying
    ##' which are required
    fields = NULL,
    ##' @field remote Configuration of remote sources i.e. shared
    ##' copy of orderly on a remote machine
    remote = NULL,
    ##' @field vault Vault server connection information
    vault = NULL,
    ##' @field global_resources Path to dir containing global resources.
    global_resources = NULL,
    ##' @field changelog Changelog type configuration
    changelog = NULL,
    ##' @field tags List of available tags for orderly reports.
    tags = NULL,
    ##' @field database Database configuration specifying driver and
    ##' connection args for (possibly multiple) databases
    database = NULL,
    ##' @field archive_version Orderly version number of the archive
    archive_version = NULL,
    ##' @field run_options List of run options
    run_options = list(),


    ##' @description
    ##' Create an object representing orderly config
    ##' @param root Root dir of the orderly repository
    ##' @param validate If TRUE migrate cfg to handle any
    ##' format changes and validate structure if well formed
    ##' for each of the cfg fields
    initialize = function(root, validate = TRUE) {
      assert_is_directory(root, FALSE)
      self$root <- normalizePath(root, mustWork = TRUE)
      filename <- path_orderly_config_yml(self$root)
      assert_file_exists(basename(filename), workdir = self$root,
                         name = "Orderly configuration")
      self$raw <- yaml_read(filename)

      v <- self$raw$minimum_orderly_version
      if (!is.null(v) && utils::packageVersion("orderly1") < v) {
        stop(sprintf(
          "Orderly version '%s' is required, but only '%s' installed",
          v, utils::packageVersion("orderly1")))
      }

      if (validate) {
        private$validate()
      }
      lock_bindings(c("raw", "destination", "fields", "remote", "vault",
                      "global_resources", "changelog", "tags", "database"),
                    self)
    },

    ##' @description
    ##' Get connection options for the current server. This is
    ##' the details from the "remote" section for the server
    ##' being run on. Server identified via env var
    ##' `ORDERLY_API_SERVER_IDENTITY`
    ##' @return Options for current server if can be identified,
    ##' otherwise NULL
    server_options = function() {
      i <- vlapply(self$remote, function(x) isTRUE(x$identity))
      if (!any(i)) {
        return(NULL)
      }
      ## TODO(VIMC-3590): Let's move these all under options at some point
      ret <- self$remote[[which(i)]]
      ret[setdiff(names(ret), c("identity", "driver", "args"))]
    },


    ##' @description
    ##' Add a key-value pair run option
    ##' @param name Name of run option
    ##' @param value Value for run option
    add_run_option = function(name, value) {
      self$run_options[[name]] <- value
    },


    ##' @description
    ##' Retrieve value of a run option
    ##' @param name Name of run option
    get_run_option = function(name) {
      self$run_options[[name]]
    }
  ),

  private = list(
    migrate = function() {
      self$raw <- config_migrate(self$raw, "orderly_config.yml")
    },

    validate = function() {
      private$migrate()
      withr::with_dir(
        self$root,
        withr::with_envvar(
          orderly_envir_read("."),
          config_validate(self, "orderly_config.yml")))
    }
  )
)


config_migrate <- function(raw, filename) {
  if (!is.null(raw[["vault_server"]])) {
    if (!is.null(raw[["vault"]])) {
      stop(sprintf("Can't specify both 'vault' and 'vault_server' in %s",
                   filename))
    }
    msg <- c("Use of 'vault_server' is deprecated and will be removed in a",
             "future orderly version.  Please use the new 'vault' server",
             "field, which offers more flexibility")
    orderly_warning(flow_text(msg))
    assert_scalar_character(raw[["vault_server"]],
                            "orderly_config.yml:vault_server")
    raw$vault <- list(addr = raw[["vault_server"]])
    raw$vault_server <- NULL
  }

  if (!is.null(raw[["source"]])) {
    if (!is.null(raw[["database"]])) {
      stop("Both 'database' and 'source' fields may not be used")
    }
    msg <- c("Use of 'source' is deprecated and will be removed in a",
             "future orderly version - please use 'database' instead.",
             "See the main package vignette for details.")
    orderly_warning(flow_text(msg))
    src <- raw$source
    raw$database <- list(
      source = list(
        driver = src$driver,
        args = src[setdiff(names(src), "driver")]))
    raw$source <- NULL
  }

  for (i in seq_along(raw[["database"]])) {
    x <- raw[["database"]][[i]]
    if (!any(c("instances", "args") %in% names(x))) {
      label <- sprintf("orderly_config.yml:database:%s", names(x)[[i]])
      msg <- c("Please move your database arguments within an 'args'",
               "block, as detecting them will be deprecated in a future",
               "orderly version.  See the main package vignette for",
               "details.  Reported for: ", label)
      orderly_warning(flow_text(msg))
      v <- setdiff(names(x), "driver")
      raw[["database"]][[i]] <- list(
        driver = x$driver,
        args = x[setdiff(names(x), "driver")])
    }
  }

  raw
}


config_validate <- function(self, filename) {
  ## There are no required fields, and soon we will let the optional
  ## fields grow as the plugin interface develops; that will require
  ## looking in some plugins field fairly early?

  ## An important concept here is that none of the configuration
  ## fields depend on each other - we just plough through and read
  ## them one after another.  That makes things considerably easier to
  ## reason about
  raw <- self$raw

  check_fields(raw, filename, character(),
               c("minimum_orderly_version", "destination", "fields",
                 "remote", "vault", "global_resources",
                 "changelog", "tags", "database"))

  self$destination <- config_validate_destination(
    raw[["destination"]], filename)
  self$fields <- config_validate_fields(
    raw[["fields"]], filename)
  self$remote <- config_validate_remote(
    raw[["remote"]], filename)
  self$vault <- config_validate_vault(
    raw[["vault"]], filename)
  self$global_resources <- config_validate_global_resources(
    raw[["global_resources"]], filename)
  self$changelog <- config_validate_changelog(
    raw[["changelog"]], filename)
  self$tags <- config_validate_tags(
    raw[["tags"]], filename)
  self$database <- config_validate_database(
    raw[["database"]], filename)

  self$archive_version <- read_orderly_archive_version(".")
  invisible(self)
}


config_validate_destination <- function(destination, filename) {
  if (is.null(destination)) {
    destination <- list(driver = "RSQLite::SQLite",
                        args = list(dbname = "orderly.sqlite"))
  }
  label <- sprintf("%s:destination", filename)

  check_fields(destination, label, c("driver", "args"), character())
  destination$driver <-
    check_symbol_from_str(destination$driver, paste0(label, ":driver"))
  assert_named(destination$args, TRUE, paste0(label, ":args"))
  destination
}


config_validate_fields <- function(fields, filename) {
  if (is.null(fields)) {
    return(data.frame(name = character(0), required = logical(0),
                      stringsAsFactors = FALSE))
  }
  assert_named(fields, TRUE, sprintf("%s:fields", filename))
  check1 <- function(nm) {
    d <- fields[[nm]]
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
  dat <- lapply(names(fields), check1)
  data_frame(name = names(fields),
             required = vlapply(dat, "[[", "required"),
             description = vcapply(dat, "[[", "description"))
}


config_validate_remote <- function(remote, filename) {
  if (is.null(remote)) {
    return(NULL)
  }
  assert_named(remote, unique = TRUE)

  check1 <- function(name) {
    d <- remote[[name]]
    check_fields(d, sprintf("%s:remote:%s", filename, name),
                 c("driver", "args"),
                 c("url", "slack_url", "teams_url", "primary",
                   "master_only", "default_branch_only",
                   "default_branch"))
    field_name <- function(nm) {
      sprintf("%s:remote:%s:%s", filename, name, nm)
    }
    assert_scalar_character(d$driver, field_name("driver"))
    assert_named(d$args, name = field_name("args"))

    ## optionals:
    if (!is.null(d$url)) {
      msg <- c("The 'url' field (used in",
               sprintf("%s:remote:%s", filename, name),
               "is deprecated and will be dropped in a future version of",
               "orderly.  Please remove it from your orderly_config.yml")
      orderly_warning(flow_text(msg))
    }
    if (is.null(d$primary)) {
      d$primary <- FALSE
    } else {
      assert_scalar_logical(d$primary, field_name("primary"))
    }

    if (!is.null(d$master_only)) {
      if (!is.null(d$default_branch_only)) {
        msg <- c("Can't specify both 'master_only' and 'default_branch_only': ",
                 sprintf("see %s:remote:%s", filename, name))
        stop(msg)
      }
      msg <- c("The 'master_only' field (used in",
               sprintf("%s:remote:%s", filename, name),
               "is deprecated and replaced with 'default_branch_only'",
               "and will be dropped in a future version of",
               "orderly.  Please rename it in your orderly_config.yml")
      orderly_warning(flow_text(msg))
      d$default_branch_only <- d$master_only
      d$master_only <- NULL
    }

    if (is.null(d$default_branch_only)) {
      d$default_branch_only <- FALSE
    } else {
      assert_scalar_logical(d$default_branch_only,
                            field_name("default_branch_only"))
    }

    ## use '[[' not '$' here to avoid partial matches.
    if (is.null(d[["default_branch"]])) {
      d[["default_branch"]] <- "master"
    } else {
      assert_scalar_character(d[["default_branch"]],
                              field_name("default_branch"))
    }

    d$driver <- check_symbol_from_str(d$driver, field_name("driver"))
    d$args <- c(d$args, list(name = name))
    d$name <- name
    d
  }

  ret <- set_names(lapply(names(remote), check1), names(remote))
  primary <- vlapply(ret, "[[", "primary")
  if (sum(primary) > 1L) {
    stop(sprintf(
      "At most one remote can be listed as primary but here %d are: %s",
      sum(primary), paste(squote(names(which(primary))), collapse = ", ")),
      call. = FALSE)
  }

  identity <- Sys.getenv("ORDERLY_API_SERVER_IDENTITY", "")
  if (nzchar(identity)) {
    match_value(identity, names(ret))
    ret[[identity]]$identity <- TRUE
  }

  ret
}


config_validate_vault <- function(vault, filename) {
  if (!is.null(vault)) {
    assert_named(vault, TRUE, sprintf("%s:vault", filename))
  }
  vault
}


config_validate_global_resources <- function(global_resources, filename) {
  if (!is.null(global_resources)) {
    assert_is_directory(global_resources, name = "Global resource directory")
    global_resources
  }
}


config_validate_changelog <- function(changelog, filename) {
  if (is.null(changelog)) {
    return(NULL)
  }

  assert_named(changelog, unique = TRUE, sprintf("%s:changelog", filename))
  for (i in names(changelog)) {
    assert_scalar_logical(
      changelog[[i]]$public,
      sprintf("%s:changelog:%s:public", filename, i))
  }

  data_frame(
    id = names(changelog),
    public = vlapply(changelog, "[[", "public", USE.NAMES = FALSE))
}


config_validate_tags <- function(tags, filename) {
  if (is.null(tags)) {
    return(NULL)
  }

  assert_character(tags, sprintf("%s:tags", filename))
  tags
}


config_validate_database <- function(database, filename) {
  if (is.null(database)) {
    return(NULL)
  }

  assert_named(database, unique = TRUE, sprintf("%s:database", filename))
  for (nm in names(database)) {
    prefix <- sprintf("%s:database:%s", filename, nm)
    database[[nm]] <- config_validate_database1(database[[nm]], prefix)
  }

  database
}


config_validate_database1 <- function(db, prefix) {
  optional <- c("args", "instances", "default_instance")
  check_fields(db, prefix, "driver", optional)

  driver <- check_symbol_from_str(db$driver, paste0(prefix, ":driver"))
  instances <- NULL

  if (!is.null(db$args)) {
    assert_named(db$args, TRUE, paste0(prefix, ":args"))
  }

  if (!is.null(db$instances)) {
    assert_named(db$instances, TRUE, paste0(prefix, ":instances"))
    for (i in names(db$instances)) {
      assert_named(db$instances[[i]], TRUE, paste0(prefix, ":instances:", i))
    }
    base <- db$args %||% set_names(list(), character())
    instances <- lapply(db$instances, utils::modifyList, x = base)
  }

  if (!is.null(db$default_instance)) {
    if (is.null(instances)) {
      msg <- c(
        "Can't specify 'default_instance' with no defined instances in",
        prefix)
      stop(flow_text(msg), call. = FALSE)
    }
    db["default_instance"] <-
      resolve_env(db["default_instance"], "default_instance",
                  error = FALSE, default = NULL)
    if (!is.null(db$default_instance)) {
      match_value(db$default_instance, names(instances),
                  paste0(prefix, ":default_instance"))
    }
  }

  if (is.null(instances)) {
    args <- db$args
  } else {
    args <- instances[[db$default_instance %||% 1L]]
    v <- c(db$default_instance,
           setdiff(names(instances), db$default_instance))
    instances <- instances[v]
  }

  list(driver = driver, args = args, instances = instances)
}
