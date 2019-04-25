## The bulk of this is validating the yaml; that turns out to be quite
## unpleasant unfortunately.
recipe_read <- function(path, config, validate = TRUE) {
  assert_is(config, "orderly_config")
  filename <- file.path(path, "orderly.yml")
  assert_file_exists(path, name = "Report working directory")
  assert_file_exists(filename, name = "Orderly configuration")
  info <- yaml_read(filename)

  required <- c("script", # filename
                "artefacts",
                config$fields$name[config$fields$required])
  optional <- c("displayname",
                "description",
                "data",
                "parameters", # character >= 1
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
    info[config$fields$name[i]] <-
      lapply(config$fields$type[i], set_mode, x = NA)
  }

  fieldname <- function(name) {
    sprintf("%s:%s", filename, name)
  }

  info$artefacts <- recipe_read_check_artefacts(info$artefacts, filename, path)
  info$resources <- recipe_read_check_resources(info$resources, filename, path)
  info$depends <-
    recipe_read_check_depends(info$depends, filename, config, validate)

  assert_scalar_character(info$script, fieldname("script"))

  if (!is.null(info$parameters)) {
    assert_character(info$parameters, fieldname("parameters"))
  }
  if (!is.null(info$packages)) {
    assert_character(info$packages, fieldname("packages"))
  }
  if (!is.null(info$sources)) {
    assert_character(info$sources, fieldname("sources"))
    assert_file_exists(file.path(path, info$sources))
    ## TODO: check relative path
    info$resources <- c(info$resources, info$sources)
  }
  if (!is.null(info$connection)) {
    if (is.character(info$connection)) {
      if (!config$database_old_style) {
        ## TODO: Better message?
        msg <- c("Use of strings for connection: is deprecated and will be",
                 "removed in a future orderly version - please use",
                 "connection: <object>: <dbname> instead.  See the main",
                 "package vignette for details")
        warning(flow_text(msg), immediate. = TRUE, call. = FALSE)
      }
      if (length(config$database) > 1L) {
        ## TODO: better message
        stop("More than one database configured; fix connection export")
      }
      assert_scalar_character(info$connection, fieldname("connection"))
      info$connection <- set_names(as.list(names(config$database)),
                                   info$connection)
    } else {
      assert_named(info$connection, unique = TRUE,
                   name = fieldname("connection"))
      for (i in names(config$connection)) {
        match_value(info$connection[[i]], names(config$database),
                    name = sprintf("%s:%s", fieldname("connection"), i))
      }
    }
  }

  ## Then some processing - this should be factored out because it's
  ## huge and ugly.
  if (!is.null(info$data)) {
    if (length(config$database) == 0L) {
      stop("No databases are configured - can't use a 'data' section")
    }
    assert_named(info$data, TRUE, fieldname("data"))
    f <- function(nm) {
      name <- sprintf("%s:%s", fieldname("data"), nm)
      d <- info$data[[nm]]
      if (is.character(d)) {
        if (!config$database_old_style) {
          msg <- c("Use of strings for queries is deprecated and will be",
                   "removed in a future orderly version - please use",
                   "query: <yourstring> instead.  See the main package",
                   "vignette for details")
          warning(flow_text(msg), immediate. = TRUE, call. = FALSE)
        }
        d <- list(query = string_or_filename(d, path, name))
      } else {
        check_fields(d, name, "query", "database")
        d$query <- string_or_filename(d$query, path, sprintf("%s:query", name))
      }
      d$query_file <- attr(d$query, "files")
      if (is.null(d$database)) {
        if (length(config$database) > 1L) {
          stop("More than one database configured; a 'database' field required")
        }
        d$database <- names(config$database)[[1]]
      } else {
        if (config$database_old_style) {
          stop("Can't specfify database without moving to new style")
        }
        match_value(d$database, names(config$database),
                    sprintf("%s:database", name))
      }
      d
    }

    info$data[] <- lapply(names(info$data), f)

    query_files <- unlist(lapply(info$data, "[[", "query_file"), FALSE, FALSE)
    info$resources <- c(info$resources, query_files)
  }

  if (!is.null(info$views)) {
    assert_named(info$views, TRUE, fieldname("views"))
    info$views <- string_or_filename(info$views, path, fieldname("views"))
    info$resources <- c(info$resources, attr(info$views, "files"))
  }

  assert_scalar_character(info$script, fieldname("script"))
  assert_file_exists(info$script, workdir = path, name = "Script file")

  for (i in seq_len(nrow(config$fields))) {
    el <- config$fields[i, ]
    x <- info[[el$name]]
    if (!is.null(x) || el$required) {
      assert_type(x, el$type, fieldname(el$name))
      assert_scalar(x, fieldname(el$name))
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
  if (is.list(x)) {
    if (all(vlapply(x, is.character))) {
      x <- vcapply(x, identity)
    }
  }
  assert_character(x, name)
  i <- grepl("\\.sql$", x)
  if (any(i)) {
    files <- x[i]
    assert_file_exists(files, workdir = path, name = "SQL file")
    x[i] <- vcapply(file.path(path, files), read_lines, USE.NAMES = FALSE)
    attr(x, "files") <- unname(files)
  }
  x
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

recipe_read_check_depends <- function(x, filename, config, validate) {
  ## TODO: this is going to assume that the artefacts are all in place
  ## - that need not actually be the case here - so we need a flag on
  ## this function that indicates that we're actually going to try and
  ## run things!
  if (is.null(x)) {
    return(NULL)
  }
  if (is.null(names(x))) {
    x <- ordered_map_to_list(x)
  }

  check_use1 <- function(i) {
    name <- names(x)[[i]]
    el <- x[[i]]
    v <- c("id", "use")
    check_fields(el, sprintf("%s:depends:%s", filename, name), v, "draft")
    el$name <- name

    assert_character(el$id, sprintf("%s:depends:%s:id", filename, name))
    if (is.null(el$draft)) {
      el$draft <- FALSE
    } else {
      assert_scalar_logical(el$draft,
                            sprintf("%s:depends:%s:draft", filename, name))
    }

    assert_named(el$use, TRUE, sprintf("%s:depends:%s:use", filename, name))
    err <- !vlapply(el$use, function(x) is.character(x) && length(x) == 1)
    if (any(err)) {
      stop(sprintf("%s:depends:%s:use must all be scalar character",
                   filename, name))
    }

    el$filename <- vcapply(el$use, identity, USE.NAMES = FALSE)
    el$as <- names(el$use)
    el$use <- NULL

    ## TODO: this is a giant hack that is required until VIMC-506 is
    ## fixed; we need to be able to load the "processed" version of
    ## the orderly yaml for import into the database but we _don't_
    ## want to look up anything about the reports because by the time
    ## we come to getting them in the database this is not necessarily
    ## correct!
    if (validate) {
      el$path <- orderly_find_report(el$id, name, config, draft = el$draft,
                                     must_work = TRUE)
      filename_full <- file.path(el$path, el$filename)

      ## TODO: VIMC-889
      msg <- !file.exists(filename_full)
      if (any(msg)) {
        stop(sprintf("Did not find file %s at %s",
                     el$filename[msg],
                     el$path))
      }
      el$hash <- hash_files(filename_full, FALSE)

      ## VIMC-2017: check that a file is actually an artefact
      meta <- readRDS(path_orderly_run_rds(el$path))
      ok <- el$filename %in% names(meta$meta$hash_artefacts)
      if (any(!ok)) {
        stop(sprintf(
          "Dependency %s not an artefact of %s/%s:\n%s",
          ngettext(sum(!ok), "file", "files"),
          el$name, basename(el$path),
          paste(sprintf("- '%s'", el$filename[!ok]), collapse = "\n")),
          call. = FALSE)
      }

      el$hash <- hash_files(filename_full, FALSE)

      el$time <- readRDS(path_orderly_run_rds(el$path))$time

      ## Is this considered to be the "latest" copy of a dependency?
      el$is_latest <- el$id == "latest" ||
        basename(el$path) == latest_id(dir(dirname(el$path)))
      el$is_pinned <- el$id != "latest"
    }

    ## Bit of a faff here to get the format into something that will
    ## serialise and interrogate nicely.
    as.data.frame(el, stringsAsFactors = FALSE)
  }

  rbind_df(lapply(seq_along(x), check_use1))
}
