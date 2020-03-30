orderly_recipe <- R6::R6Class(
  "orderly_recipe",

  public = list(
    ## TODO: We run very quickly into a clash between methods and
    ## fieldnames if not careful (data is one example, used both in
    ## the yml and here for referring to the raw data) - we'll need to
    ## think about that...
    config = NULL,
    raw = NULL,
    develop = NULL,

    packages = NULL,
    script = NULL,
    sources = NULL,
    resources = NULL,
    global_resources = NULL,
    parameters = NULL,
    fields = NULL,
    changelog = NULL,
    tags = NULL,
    secrets = NULL,
    environment = NULL,

    depends = NULL,
    artefacts = NULL,

    connection = NULL,
    data = NULL,
    views = NULL,

    name = NULL,
    path = NULL,

    initialize = function(name, config, develop = FALSE) {
      assert_is(config, "orderly_config")

      self$name <- name
      self$path <- file.path(config$root, "src", name)

      filename <- file.path(self$path, "orderly.yml")
      assert_file_exists(self$path, name = "Report working directory")
      assert_file_exists(filename, name = "Orderly configuration")
      self$raw <- yaml_read(filename)
      self$config <- config
      self$develop <- develop

      self$validate()
    },

    migrate = function() {
      self$raw <- recipe_migrate(self$raw, self$config, "orderly.yml")
    },

    validate = function() {
      self$migrate()
      withr::with_dir(
        self$path,
        recipe_validate(self, "orderly.yml"))
      invisible(self)
    },

    resolve_dependencies = function(use_draft = FALSE, parameters = NULL,
                                    remote = NULL) {
      withr::with_dir(
        self$path,
        recipe_resolve_dependencies(self, use_draft, parameters, remote))
      invisible(self)
    }
  ))


recipe_migrate <- function(raw, config, filename) {
  ## TODO: should move custom fields within their own section I think,
  ## so for now I'm going to process this with a migration and we can
  ## set up deprecating it later.
  v <- intersect(config$fields$name, names(raw))
  if (length(v) > 0L) {
    raw$fields <- raw[intersect(config$fields$name, names(raw))]
    raw <- raw[setdiff(names(raw), v)]
  }

  if (is.character(raw$parameters)) {
    msg <- c("Use of strings for parameters: is deprecated and will be",
             "removed in a future orderly version - please use",
             "'name: ~' for each parameter instead (or add a default).",
             "See the main package vignette for details")
    orderly_warning(flow_text(msg))
    raw$parameters <- set_names(rep(list(NULL), length(raw$parameters)),
                                raw$parameters)
  }

  if (is.character(raw$global_resources)) {
    msg <- c("Use of strings for global_resources: is deprecated and will be",
             "removed in a future orderly version - please use",
             "<as>: <from> mapping pairs instead.")
    orderly_warning(flow_text(msg))
    raw$global_resources <- set_names(as.list(raw$global_resources),
                                      basename(raw$global_resources))
  }

  if (is.character(raw$connection)) {
    ## TODO: Better message?
    msg <- c("Use of strings for connection: is deprecated and will be",
             "removed in a future orderly version - please use",
             "connection: <object>: <dbname> instead.  See the main",
             "package vignette for details")
    orderly_warning(flow_text(msg))
    if (length(config$database) > 1L) {
      msg <- paste("More than one database configured; update 'connection'",
                   sprintf("from '%s' to '%s: <dbname>' in '%s'",
                           raw$connection, raw$connection, filename))
      stop(msg, call. = FALSE)
    }
    if (!is.null(config$database)) {
      raw$connection <- set_names(as.list(names(config$database)),
                                  raw$connection)
    }
  }

  for (v in c("data", "views")) {
    for (i in seq_along(raw[[v]])) {
      if (is.character(raw[[v]][[i]])) {
        msg <- c("Use of strings for queries is deprecated and will be",
                 "removed in a future orderly version - please use",
                 "query: <yourstring> instead.  See the main package",
                 "vignette for details")
        orderly_warning(flow_text(msg))
        raw[[v]][[i]] <- list(query = raw[[v]][[i]])
      }
    }
  }

  raw
}



recipe_validate <- function(self, filename) {
  raw <- self$raw
  config <- self$config

  required <- c("script", # filename
                "artefacts",
                config$fields$name[config$fields$required])
  optional <- c("displayname",
                "description",
                "data",
                "parameters",
                "views",
                "packages",
                "sources",
                "resources",
                "connection",
                "depends",
                "global_resources",
                "tags",
                "secrets",
                "environment",
                config$fields$name[!config$fields$required])

  ## TODO: let's deal with these properly by throwing a skippable
  ## error.  The other option is to wrap the calls with something that
  ## will do the skip for us.
  check_fields(raw, filename, required, optional)

  self$packages <- recipe_validate_packages(raw$packages, config, filename)

  self$script <- recipe_validate_script(raw$script, config, filename)
  self$sources <- recipe_validate_sources(raw$sources, config, filename)
  self$resources <- recipe_validate_resources(raw$resources, config, filename)
  self$global_resources <- recipe_validate_global_resources(
    raw$global_resources, config, filename)

  self$parameters <- recipe_validate_parameters(
    raw$parameters, config, filename)
  self$fields <- recipe_validate_fields(raw$fields, config, filename)

  self$tags <- recipe_validate_tags(raw$tags, config, filename)
  self$secrets <- recipe_validate_secrets(raw$secrets, config, filename)
  self$environment <- recipe_validate_environment(
    raw$environment, config, filename)

  self$connection <- recipe_validate_connection(
    raw$connection, config, filename)
  self$data <- recipe_validate_database(raw$data, config, filename)
  self$views <- recipe_validate_views(raw$views, config, filename)

  self$artefacts <- recipe_validate_artefacts(raw$artefacts, config, filename)
  self$depends <- recipe_validate_depends(raw$depends, config, filename)

  ## TODO: odd one out here; should probably read whole thing?
  if (file.exists("changelog.txt")) {
    self$changelog <- "changelog.txt"
  }

  ## Combined validation:
  err <- intersect(self$sources, self$resources)
  if (length(err)) {
    stop(sprintf("Do not list source files (sources) as resources:%s",
                 paste(sprintf("\n  - %s", err), collapse = "")),
         call. = FALSE)
  }

  ## This is a bit weird, but we'll generalise this once we get plugins
  self$resources <- c(self$resources,
                      attr(self$data, "resources"),
                      attr(self$views, "resources"))
}


recipe_validate_packages <- function(packages, config, filename) {
  if (is.null(packages)) {
    return(NULL)
  }
  assert_character(packages, sprintf("%s:packages", filename))
  packages
}


recipe_validate_script <- function(script, config, filename) {
  assert_scalar_character(script, sprintf("%s:script", filename))
  assert_file_exists(script, name = "Script file")
  script
}


recipe_validate_sources <- function(sources, config, filename) {
  if (is.null(sources)) {
    return()
  }
  assert_character(sources, sprintf("%s:%s", filename, "sources"))
  assert_file_exists(sources, name = "Source file")
  sources
}


recipe_validate_resources <- function(resources, config, filename) {
  if (is.null(resources)) {
    return(NULL)
  }

  ## make sure that a directory resource does not have a trailing /
  ## There is much more sanitation that could be done here...
  is_dir <- is_directory(resources)
  trailing <- grepl(pattern = "(\\/)$", resources)
  bad_resource <- is_dir & trailing
  if (any(bad_resource)) {
    resources[bad_resource] <- sub("(\\/)$", "", resources[bad_resource])
  }

  assert_character(resources, sprintf("%s:%s", filename, "resouces"))
  assert_file_exists(resources, name = "Resource file")
  ## TODO: this is not quite right because the files need to be
  ## tested (as done here) with names within that directory.
  err <- resources[!is_within_dir(resources, ".")]
  if (length(err) > 0L) {
    stop("Declared resources not in right place: ",
         paste(err, collapse = ", "))
  }

  resources
}


recipe_validate_global_resources <- function(global_resources, config,
                                             filename) {
  if (is.null(global_resources)) {
    return(NULL)
  }

  ## TODO: verify that global resources are supported
  prefix <- sprintf("%s:global_resources", filename)

  assert_named(global_resources, name = prefix)
  for (i in seq_along(global_resources)) {
    assert_scalar_character(
      global_resources[[i]],
      sprintf("%s:%s", prefix, names(global_resources)[[i]]))
  }
  global_resources <- list_to_character(global_resources)

  global_path <- file.path(config$root, config$global_resources)
  assert_file_exists(
    global_resources, check_case = TRUE, workdir = global_path,
    name = sprintf("Global resources in '%s'", global_path))

  if (any(is_directory(file.path(global_path, global_resources)))) {
    stop("global resources cannot yet be directories")
  }

  global_resources
}


recipe_validate_parameters <- function(parameters, config, filename) {
  if (is.null(parameters) || length(parameters) == 0L) {
    return(NULL)
  }

  name <- function(p) {
    sprintf("%s:parameters:%s", filename, p)
  }

  assert_named(parameters, TRUE, sprintf("%s:parameters", filename))
  for (p in names(parameters)) {
    check_fields(parameters[[p]], name(p), NULL, "default")
  }

  parameters
}


recipe_validate_fields <- function(fields, config, filename) {
  if (nrow(config$fields) == 0L) {
    return(NULL)
  }

  ## Fill any any missing optional fields:
  msg <- setdiff(config$fields$name, names(fields))
  if (length(msg) > 0L) {
    fields[msg] <- NA_character_
  }

  fields
}


recipe_validate_tags <- function(tags, config, filename) {
  if (is.null(tags)) {
    return(NULL)
  }

  if (is.null(config$tags)) {
    stop("Tags are not supported; please edit orderly_config.yml to enable")
  }
  assert_character(tags, name)
  err <- setdiff(tags, config$tags)
  if (length(err) > 0L) {
    stop("Unknown tag: ", paste(squote(err), collapse = ", "),
         call. = FALSE)
  }
  err <- unique(tags[duplicated(tags)])
  if (length(err) > 0L) {
    stop("Duplicated tag: ", paste(squote(err), collapse = ", "),
         call. = FALSE)
  }
  tags
}


recipe_validate_secrets <- function(secrets, config, filename) {
  if (is.null(secrets)) {
    return(NULL)
  }
  if (is.null(config$vault)) {
    stop("Vault not enabled in orderly_config.yml")
  }

  assert_named(secrets, TRUE, name = sprintf("%s:secrets", filename))
  as <- names(secrets)

  for (i in seq_along(secrets)) {
    assert_scalar_character(secrets[[i]],
                            sprintf("orderly.yml:secrets:%s", as[[i]]))
  }

  path <- list_to_character(secrets, FALSE)
  re <- "^([^:]+):([^:]+)$"
  err <- !grepl(re, path)
  if (any(err)) {
    msg <- sprintf("'%s' for '%s'", path[err], as[err])
    stop("Misformatted secret path: ", paste(msg, collapse = ", "),
         call. = FALSE)
  }

  secrets[] <- paste0("VAULT:", path)

  secrets
}


recipe_validate_environment <- function(environment, config, filename) {
  if (is.null(environment)) {
    return(NULL)
  }
  assert_named(environment, TRUE,
               name = sprintf("%s:environment", filename))
  for (name in names(environment)) {
    assert_scalar_character(
      environment[[name]],
      sprintf("orderly.yml:environment:%s", name))
  }
  environment
}


recipe_validate_connection <- function(connection, config, filename) {
  if (is.null(connection)) {
    return(NULL)
  }
  if (length(config$database) == 0L) {
    stop("No databases are configured - can't use a 'connection' section")
  }

  assert_named(connection, unique = TRUE,
               name = sprintf("%s:connection", filename))
  for (i in names(connection)) {
    match_value(connection[[i]], names(config$database),
                name = sprintf("%s:connection:%s", filename, i))
  }
  connection
}



recipe_validate_database <- function(database, config, filename) {
  recipe_validate_query(database, "data", config, filename)
}


recipe_validate_views <- function(views, config, filename) {
  recipe_validate_query(views, "views", config, filename)
}


recipe_validate_query <- function(d, field, config, filename) {
  if (is.null(d)) {
    return(NULL)
  }

  if (length(config$database) == 0L) {
    stop(sprintf(
      "No databases are configured - can't use a '%s' section", field))
  }
  assert_named(d, TRUE, sprintf("%s:%s", filename, field))
  f <- function(nm) {
    name <- sprintf("%s:%s:%s", filename, field, nm)
    d <- d[[nm]]
    check_fields(d, name, "query", "database")
    dat <- string_or_filename(d$query, ".", sprintf("%s:query", name))
    d[names(dat)] <- dat

    if (is.null(d$database)) {
      if (length(config$database) > 1L) {
        msg <- paste("More than one database configured; a 'database'",
                     sprintf("field is required for '%s'", name))
        stop(msg, call. = FALSE)
      }
      d$database <- names(config$database)[[1]]
    } else {
      match_value(d$database, names(config$database),
                  sprintf("%s:database", name))
    }
    d
  }

  d[] <- lapply(names(d), f)

  query_files <- unlist(lapply(d, "[[", "query_file"), FALSE, FALSE)

  ## TODO: this is not amazing
  attr(d, "resources") <- query_files
  d
}
