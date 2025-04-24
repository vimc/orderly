orderly_recipe <- R6::R6Class(
  "orderly_recipe",

  public = list(
    ## NOTE: We run very quickly into a clash between methods and
    ## fieldnames if not careful (data is one example, used both in
    ## the yml and here for referring to the raw data) - we'll need to
    ## think about that
    config = NULL,
    raw = NULL,

    packages = NULL,
    script = NULL,
    sources = NULL,
    resources = NULL,
    global_resources = NULL,
    parameters = NULL,
    fields = NULL,
    tags = NULL,
    secrets = NULL,
    environment = NULL,
    displayname = NULL,
    description = NULL,

    readme = NULL,
    changelog = NULL,

    depends = NULL,
    artefacts = NULL,

    connection = NULL,
    data = NULL,
    views = NULL,

    name = NULL,
    path = NULL,

    initialize = function(name, config, develop = FALSE, path = NULL) {
      assert_is(config, "orderly_config")

      self$name <- name
      self$path <- path %||% file.path(config$root, "src", name)

      filename <- file.path(self$path, "orderly.yml")
      assert_is_directory(basename(self$path), workdir = dirname(self$path),
                          name = "Report working directory")
      assert_file_exists(basename(filename), workdir = dirname(filename),
                         name = "Orderly configuration")
      self$raw <- yaml_read(filename)
      self$config <- config

      self$validate(develop)
    },

    migrate = function() {
      self$raw <- recipe_migrate(self$raw, self$config, "orderly.yml")
    },

    validate = function(develop) {
      self$migrate()
      withr::with_dir(
        self$path,
        recipe_validate(self, develop, "orderly.yml"))
      invisible(self)
    },

    inputs = function(path = ".") {
      withr::with_path(path, recipe_file_inputs(self))
    },

    resolve_dependencies = function(use_draft = FALSE, parameters = NULL,
                                    remote = NULL) {
      withr::with_dir(
        self$path,
        recipe_resolve_dependencies(self, use_draft, parameters, remote))
    }
  ))


recipe_migrate <- function(raw, config, filename, relaxed = FALSE) {
  ## TODO(VIMC-3613): should move custom fields within their own
  ## section I think, so for now I'm going to process this with a
  ## migration and we can set up deprecating it later.
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
    msg <- c("Use of strings for connection: is deprecated and will be",
             "removed in a future orderly version - please use",
             "connection: <object>: <dbname> instead.  See the main",
             "package vignette for details")
    orderly_warning(flow_text(msg))
    database <- config$database
    if (length(database) > 1L) {
      msg <- paste("More than one database configured; update 'connection'",
                   sprintf("from '%s' to '%s: <dbname>' in '%s'",
                           raw$connection, raw$connection, filename))
      if (!relaxed) {
        stop(msg)
      }
      message(msg)
      database <- database[[1]]
    }
    if (!is.null(database)) {
      raw$connection <- set_names(as.list(names(database)), raw$connection)
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


recipe_validate <- function(self, develop, filename) {
  raw <- self$raw
  config <- self$config

  check <- list(packages = recipe_validate_packages,
                script = recipe_validate_script,
                sources = recipe_validate_sources,
                resources = recipe_validate_resources,
                global_resources = recipe_validate_global_resources,
                parameters = recipe_validate_parameters,
                fields = recipe_validate_fields,
                tags = recipe_validate_tags,
                secrets = recipe_validate_secrets,
                environment = recipe_validate_environment,
                connection = recipe_validate_connection,
                data = recipe_validate_database,
                views = recipe_validate_views,
                artefacts = recipe_validate_artefacts,
                depends = recipe_validate_depends,
                displayname = recipe_validate_displayname,
                description = recipe_validate_description)

  required <- c("script", "artefacts")
  optional <- setdiff(names(check), required)

  recipe_validate_skip_on_develop(
    develop,
    check_fields(raw, filename, required, optional))
  for (x in names(check)) {
    recipe_validate_skip_on_develop(
      develop,
      self[[x]] <- check[[x]](raw[[x]], config, filename))
  }

  recipe_validate_skip_on_develop(
    develop,
    self$changelog <- recipe_validate_changelog("."))
  recipe_validate_skip_on_develop(
    develop,
    self$readme <- recipe_validate_readme("."))

  ## Combined validation:
  err <- intersect(self$sources, self$resources)
  if (length(err) && !develop) {
    stop(sprintf("Do not list source files (sources) as resources:%s",
                 paste(sprintf("\n  - %s", err), collapse = "")),
         call. = FALSE)
  }

  ## This is a bit weird, but we'll generalise this once we get plugins
  self$resources <- c(self$resources,
                      attr(self$data, "resources"),
                      attr(self$views, "resources"))

  recipe_check_unique_inputs(recipe_file_inputs(self), self$depends)
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
  exprs <- parse(file = script, keep.source = TRUE)
  for (i in seq_along(exprs)) {
    if (is_global_rm(exprs[[i]])) {
      stop(sprintf(
        "Do not use 'rm(list = ls())' or similar in your script (%s:%s)",
        script, utils::getSrcLocation(exprs[i], "line")))
    }
  }

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
  err <- resources[!is_within_dir(resources, ".")]
  if (length(err) > 0L) {
    stop("Declared resources not in right place: ",
         paste(err, collapse = ", "))
  }

  i <- grepl("README(|\\.md)$", resources, ignore.case = TRUE)
  if (any(i)) {
    orderly_log(
      "warning",
      sprintf("'%s' should not be listed as a resource", resources[i]))
    resources <- resources[!i]
  }

  if (any(is_dir)) {
    resources <- as.list(resources)
    resources[is_dir] <- lapply(resources[is_dir], function(p)
      file.path(p, dir(p, recursive = TRUE, all.files = TRUE)))
    resources <- unlist(resources)
  }

  resources
}


recipe_validate_global_resources <- function(global_resources, config,
                                             filename) {
  if (is.null(global_resources)) {
    return(NULL)
  }

  if (is.null(config$global_resources)) {
    stop(paste("'global_resources' is not supported;",
               "please edit orderly_config.yml to enable"),
         call. = FALSE)
  }

  prefix <- sprintf("%s:global_resources", filename)

  assert_named(global_resources, name = prefix)
  for (i in seq_along(global_resources)) {
    assert_scalar_character(
      global_resources[[i]],
      sprintf("%s:%s", prefix, names(global_resources)[[i]]))
  }
  global_resources <- list_to_character(global_resources)

  ## This check is made optional for the case where we're reading in a
  ## bundle, where the root may not exist and the global resources are
  ## already in place.
  if (!is.null(config$root)) {
    global_path <- file.path(config$root, config$global_resources)
    assert_file_exists(
      global_resources, check_case = TRUE, workdir = global_path,
      name = sprintf("Global resources in '%s'", global_path))

    if (any(is_directory(file.path(global_path, global_resources)))) {
      stop("global resources cannot yet be directories")
    }
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

  err <- setdiff(config$fields$name[config$fields$required], names(fields))
  if (length(err) > 0L) {
    stop(sprintf("Fields missing from %s: %s",
                 filename, paste(squote(err), collapse = ", ")),
         call. = FALSE)
  }

  ## Fill any any missing optional fields, and validate type
  for (i in seq_len(nrow(config$fields))) {
    nm <- config$fields$name[[i]]
    if (config$fields$required[[i]] || !is.null(fields[[nm]])) {
      assert_scalar_character(fields[[nm]], sprintf("%s:%s", filename, nm))
    } else {
      if (length(fields) == 0L) {
        ## This is needed in for 3.6 compatibility, as '[[' assignment
        ## into NULL with 3.6 coerces to character vector, while for
        ## 4.0 and it coerces to list:
        ## https://bugs.r-project.org/bugzilla/show_bug.cgi?id=17719
        fields <- as.list(fields)
      }
      fields[[nm]] <- NA_character_
    }
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
  name <- if (is.null(filename)) "tags" else sprintf("%s:tags", filename)
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

  attr(d, "resources") <- query_files
  d
}


recipe_validate_displayname <- function(displayname, config, filename) {
  if (is.null(displayname)) {
    return(NULL)
  }
  assert_scalar_character(displayname, sprintf("%s:displayname", filename))
  displayname
}


recipe_validate_description <- function(description, config, filename) {
  if (is.null(description)) {
    return(NULL)
  }
  assert_scalar_character(description, sprintf("%s:description", filename))
  description
}


recipe_validate_skip_on_develop <- function(develop, expr) {
  if (develop) {
    tryCatch(expr, error = function(e) orderly_log("warning", e$message))
  } else {
    force(expr)
  }
}


recipe_validate_readme <- function(path) {
  readme <- dir(path, pattern = "^README(|\\.md)$", ignore.case = TRUE,
                recursive = TRUE)
  if (length(readme) == 0L) {
    return(NULL)
  }

  ## Surprisingly awful; convert readme.md -> README.md readme ->
  ## README, README.MD -> README.md etc
  re <- "^(.*)?(README)(|\\.md)$"
  names(readme) <- paste0(sub(re, "\\1", readme, ignore.case = TRUE),
                          toupper(sub(re, "\\2", readme, ignore.case = TRUE)),
                          tolower(sub(re, "\\3", readme, ignore.case = TRUE)))
  readme
}


recipe_validate_changelog <- function(path) {
  filename <- path_changelog_txt(path)
  if (!file_exists(filename)) {
    return(NULL)
  }
  ## This takes care of the canonical casing for us, as people might
  ## be tempted to use something like ChangeLog.txt, as capital 'L' is
  ## canonical: https://en.wikipedia.org/wiki/Changelog
  assert_file_exists(basename(filename), workdir = path, check_case = TRUE)
  list(filename = basename(filename),
       contents = readLines(filename))
}


string_or_filename <- function(x, path, name) {
  assert_scalar_character(x, name)
  if (grepl("\\.sql$", x)) {
    file <- x
    assert_file_exists(file, workdir = path, name = "SQL file")
    query <- read_lines(file.path(path, file))
  } else {
    file <- NULL
    query <- x
  }
  list(query = query, query_file = file)
}


recipe_check_unique_inputs <- function(inputs, depends) {
  tmp <- rbind(
    inputs[c("filename", "file_purpose")],
    data_frame(filename = depends$as,
               file_purpose = rep("depends", NROW(depends))))
  err <- tmp[tmp$filename %in% tmp$filename[duplicated(tmp$filename)], ]
  if (nrow(err) > 0L) {
    err <- split(err$file_purpose, err$filename)
    details <- sprintf("\n  - %s: %s",
                       names(err), vcapply(err, paste, collapse = ", "))
    stop(sprintf("Orderly configuration implies duplicate files:%s",
                 paste(details, collapse = "")),
         call. = FALSE)
  }
}


recipe_file_inputs <- function(info) {
  file_in_data(
    orderly_yml = file_info("orderly.yml"),
    script = file_info(info$script),
    readme = file_info(names(info$readme)),
    source = file_info(info$sources),
    resource = file_info(info$resources),
    global = file_info(names(info$global_resources)))
}


is_global_rm <- function(expr) {
  is.recursive(expr) &&
    identical(expr[[1]], quote(rm)) &&
    is.recursive(expr[[2]]) &&
    identical(expr[[2]][[1]], quote(ls))
}
