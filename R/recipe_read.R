## The bulk of this is validating the yaml; that turns out to be quite
## unpleasant unfortunately.
recipe_read <- function(path, config, validate = TRUE, use_draft = FALSE,
                        remote = NULL) {
  assert_is(config, "orderly_config")
  filename <- file.path(path, "orderly.yml")
  assert_file_exists(path, name = "Report working directory")
  assert_file_exists(filename, name = "Orderly configuration")
  info <- yaml_read(filename)

  if (is.logical(use_draft)) {
    assert_scalar_logical(use_draft)
  } else {
    match_value(use_draft, c("always", "newer", "never"))
  }

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
                config$fields$name[!config$fields$required])
  check_fields(info, filename, required, optional)

  ## Fill any any missing optional fields:
  i <- !(config$fields$name %in% names(info))
  if (any(i)) {
    info[config$fields$name[i]] <- NA_character_
  }

  if (file.exists(file.path(path, "changelog.txt"))) {
    info$changelog <- "changelog.txt"
  }

  fieldname <- function(name) {
    sprintf("%s:%s", filename, name)
  }

  info$artefacts <- recipe_read_check_artefacts(info$artefacts, filename, path)
  info$resources <- recipe_read_check_resources(info$resources, filename, path)
  info$global_resources <- recipe_read_check_global_resources(
    info$global_resources, filename, config)
  info$depends <-
    recipe_read_check_depends(info$depends, filename, config)
  if (validate) {
    info$depends <-
      resolve_dependencies(info$depends, config, use_draft, remote)
  }

  assert_scalar_character(info$script, fieldname("script"))

  info$parameters <- recipe_read_check_parameters(info$parameters, filename)
  if (!is.null(info$packages)) {
    assert_character(info$packages, fieldname("packages"))
  }

  recipe_read_check_sources(info$sources, info$resources, filename, path)
  if (!is.null(info$sources)) {
    assert_character(info$sources, fieldname("sources"))
    assert_file_exists(file.path(path, info$sources))
  }
  if (!is.null(info$connection)) {
    if (length(config$database) == 0L) {
      stop("No databases are configured - can't use a 'connection' section")
    }
    if (is.character(info$connection)) {
      if (!config$database_old_style) {
        ## TODO: Better message?
        msg <- c("Use of strings for connection: is deprecated and will be",
                 "removed in a future orderly version - please use",
                 "connection: <object>: <dbname> instead.  See the main",
                 "package vignette for details")
        orderly_warning(flow_text(msg))
      }
      if (length(config$database) > 1L) {
        msg <- paste("More than one database configured; update 'connection'",
                     sprintf("from '%s' to '%s: <dbname>' in '%s'",
                             info$connection, info$connection, filename))
        stop(msg, call. = FALSE)
      }
      assert_scalar_character(info$connection, fieldname("connection"))
      info$connection <- set_names(as.list(names(config$database)),
                                   info$connection)
    } else {
      assert_named(info$connection, unique = TRUE,
                   name = fieldname("connection"))
      for (i in names(info$connection)) {
        match_value(info$connection[[i]], names(config$database),
                    name = sprintf("%s:%s", fieldname("connection"), i))
      }
    }
  }

  info <- recipe_read_query("data", info, filename, config)
  info <- recipe_read_query("views", info, filename, config)

  assert_scalar_character(info$script, fieldname("script"))
  assert_file_exists(info$script, workdir = path, name = "Script file")

  for (i in seq_len(nrow(config$fields))) {
    el <- config$fields[i, ]
    x <- info[[el$name]]
    if (!is.null(x) || el$required) {
      assert_scalar_character(x, fieldname(el$name))
    }
  }

  info$name <- basename(normalizePath(path))

  ## TODO: we probably need to reference any additional on-disk
  ## resources here too
  info$script_hash <- unname(tools::md5sum(file.path(path, info$script)))
  info$hash <- digest::digest(info)

  ## Must add this after hashing
  info$path <- path

  info
}

## TODO: this whole thing needs thought: this is not really the
## *format* (which we can get from the extension) but an intent of
## use.
valid_formats <- function() {
  c("staticgraph", "interactivegraph", "data", "report", "interactivehtml")
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

recipe_read_check_artefacts <- function(x, filename, path) {
  check_artefact <- function(i) {
    format <- names(x)[[i]]
    el <- x[[i]]
    v <- c("filenames", "description")
    check_fields(el, sprintf("%s:artefacts[%d]", filename, i), v, NULL)
    for (j in v) {
      assert_character(el[[j]], sprintf("artefacts:%s:%s", i, j))
    }
    c(el[v], list(format = format))
  }
  if (length(x) == 0L) {
    stop("At least one artefact required")
  }

  if (is.character(x)) {
    msg <- c("Your artefacts are misformatted.  You must provide a 'type'",
             "and a description for each, and each logical artefact may",
             "contain multiple files.  For example, you might use",
             "",
             "artefacts:",
             "  - data:",
             "      description: These are data for x, y, z",
             "      filenames:",
             sprintf("        - %s", x),
             "",
             sprintf("other alternatives to 'data' are %s",
                     paste(squote(setdiff(valid_formats(), "data")),
                           collapse = ", ")))
    stop(paste(msg, collapse = "\n"), call. = FALSE)
  }

  ## There are two valid options here:
  ##
  ## artefacts:
  ##   staticgraph:
  ##     ...
  ##
  ## or
  ##
  ## artefacts:
  ##   - staticgraph:
  ##       ...
  ##   - staticgraph:
  ##       ...
  ##
  ## This converts the latter into the former:
  if (!is.null(names(x)) && length(x) != 1L) {
    if (any(names(x) %in% valid_formats())) {
      x <- utils::tail(x, 3)
      correct <- list(artefacts = lapply(seq_along(x), function(i) x[i]))
      msg <- c("Your artefacts look incorrectly formatted; they must be",
               "an _ordered map_.  Currently you have something like",
               "",
               indent(yaml::as.yaml(list(artefacts = x)), 4),
               "",
               "but you should reformat that as something like",
               "",
               indent(yaml::as.yaml(correct), 4),
               "",
               "otherwise with duplicate entries with the same report type",
               "your yaml will be invalid (this format is permitted for",
               "single artefacts only)")
      message(paste(msg, collapse = "\n"))
    }
    stop("Expected an ordered map!")
  }

  if (is.null(names(x)) && all(lengths(x) == 1L)) {
    x <- set_names(lapply(x, "[[", 1L), vcapply(x, names))
  }
  assert_named(x, FALSE, "artefacts")
  unk <- setdiff(names(x), valid_formats())
  if (length(unk) > 0L) {
    stop(sprintf("Unknown artefact %s: '%s'",
                 ngettext(length(unk), "type", "types"),
                 paste(unk, collapse = ", ")))
  }

  res <- t(vapply(seq_along(x), check_artefact, vector("list", 3L)))

  filenames <- unlist(res[, "filenames"])
  dups <- unique(filenames[duplicated(filenames)])
  if (length(dups) > 0L) {
    stop("Duplicate artefact filenames are not allowed: ",
         paste(squote(dups), collapse = ", "))
  }

  res
}

recipe_read_check_resources <- function(x, filename, path) {
  if (is.null(x)) {
    return(NULL)
  }

  # make sure that a directory resource does not have a trailing /
  # There is much more sanitation that could be done here...
  is_dir <- is_directory(file.path(path, x))
  trailing <- grepl(pattern = "(\\/)$", x)
  bad_resource <- is_dir & trailing
  if (any(bad_resource)) {
    x[bad_resource] <- sub("(\\/)$", "", x[bad_resource])
  }

  assert_character(x, sprintf("%s:%s", filename, "resouces"))
  assert_file_exists(x, workdir = path, name = "Resource file")
  ## TODO: this is not quite right because the files need to be
  ## tested (as done here) with names within that directory.
  err <- x[!is_within_dir(file.path(path, x), path)]
  if (length(err) > 0L) {
    stop("Declared resources not in right place: ",
         paste(err, collapse = ", "))
  }
  ## TODO: At this point we should also return the normalised
  ## *relative* path perhaps to prevent valid, but absolute, paths
  ## being used.
  x
}


recipe_read_check_global_resources <- function(x, filename, config) {
  if (is.null(x)) {
    return(NULL)
  }

  if (is.character(x)) {
    msg <- c("Use of strings for global_resources: is deprecated and will be",
             "removed in a future orderly version - please use",
             "<as>: <from> mapping pairs instead.")
    orderly_warning(flow_text(msg))
    x <- set_names(as.list(x), basename(x))
  }

  prefix <- sprintf("%s:global_resources", filename)

  assert_named(x, name = prefix)
  for (i in seq_along(x)) {
    assert_scalar_character(x[[i]], sprintf("%s:%s", prefix, names(x)[[i]]))
  }
  x <- list_to_character(x)

  global_path <- file.path(config$root, config$global_resources)
  assert_file_exists(
    x, check_case = TRUE, workdir = global_path,
    name = sprintf("Global resources in '%s'", global_path))

  if (any(is_directory(file.path(global_path, x)))) {
    stop("global resources cannot yet be directories")
  }

  x
}


recipe_read_check_sources <- function(sources, resources, filename, path) {
  if (is.null(sources)) {
    return()
  }
  assert_character(sources, sprintf("%s:%s", filename, "sources"))
  assert_file_exists(sources, workdir = path, name = "Source file")
  err <- intersect(sources, resources)
  if (length(err)) {
    stop(sprintf("Do not list source files (sources) as resources:%s",
                 paste(sprintf("\n  - %s", err), collapse = "")),
         call. = FALSE)
  }
}


recipe_read_query <- function(field, info, filename, config) {
  d <- info[[field]]
  path <- dirname(filename)

  if (!is.null(d)) {
    if (length(config$database) == 0L) {
      stop("No databases are configured - can't use a 'data' section")
    }
    assert_named(d, TRUE, sprintf("%s:%s", filename, field))
    f <- function(nm) {
      name <- sprintf("%s:%s:%s", filename, field, nm)
      d <- d[[nm]]
      if (is.character(d)) {
        if (!config$database_old_style) {
          msg <- c("Use of strings for queries is deprecated and will be",
                   "removed in a future orderly version - please use",
                   "query: <yourstring> instead.  See the main package",
                   "vignette for details")
          orderly_warning(flow_text(msg))
        }
        d <- string_or_filename(d, path, name)
      } else {
        check_fields(d, name, "query", "database")
        dat <- string_or_filename(d$query, path, sprintf("%s:query", name))
        d[names(dat)] <- dat
      }
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
    info$resources <- c(info$resources, query_files)
    info[[field]] <- d
  }
  info
}


recipe_read_check_parameters <- function(parameters, filename) {
  if (is.null(parameters) || length(parameters) == 0L) {
    return(NULL)
  }

  if (is.character(parameters)) {
    msg <- c("Use of strings for parameters: is deprecated and will be",
             "removed in a future orderly version - please use",
             "'name: ~' for each parameter instead (or add a default).",
             "See the main package vignette for details")
    orderly_warning(flow_text(msg))
    parameters <- set_names(rep(list(NULL), length(parameters)),
                            parameters)
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
