## The bulk of this is validating the yaml; that turns out to be quite
## unpleasant unfortunately.
recipe_read <- function(path, config) {
  assert_is(config, "orderly_config")
  filename <- file.path(path, "orderly.yml")
  if (!file.exists(filename)) {
    stop("Did not find file 'orderly.yml' at path ", path)
  }
  info <- yaml_read(filename)

  required <- c("script", # filename
                "artefacts",
                "data",
                config$fields$name[config$fields$required])
  optional <- c("displayname",
                "description",
                "parameters", # character >= 1
                "views",
                "packages",
                "sources",
                "resources",
                "connection",
                "depends",
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

  info$artefacts <- recipe_read_check_artefacts(info$artefacts, filename)
  info$resources <- recipe_read_check_resources(info$resources, filename, path)
  info$depends <- recipe_read_check_depends(info$depends, filename, config)

  assert_scalar_character(info$script, fieldname("script"))

  if (!is.null(info$parameters)) {
    assert_character(info$parameters, fieldname("parameters"))
  }
  if (!is.null(info$packages)) {
    assert_character(info$packages, fieldname("packages"))
  }
  if (!is.null(info$sources)) {
    assert_character(info$sources, fieldname("sources"))
    assert_file_exists(file.path(path,info$sources))
    ## TODO: check relative path
    info$resources <- c(info$resources, info$sources)
  }
  if (!is.null(info$connection)) {
    assert_scalar_character(info$connection)
  }

  ## Then some processing:
  if (!is.null(info$data)) {
    assert_named(info$data, TRUE, fieldname("data"))
    info$data <- string_or_filename(info$data, path, fieldname("data"))
    info$resources <- c(info$resources, attr(info$data, "files"))
  }

  if (!is.null(info$views)) {
    assert_named(info$views, TRUE, fieldname("views"))
    info$views <- string_or_filename(info$views, path, fieldname("views"))
    info$resources <- c(info$resources, attr(info$views, "files"))
  }

  assert_scalar_character(info$script, fieldname("script"))
  if (!file.exists(file.path(path, info$script))) {
    stop(sprintf("script file %s does not exist", info$script))
  }

  for (i in seq_len(nrow(config$fields))) {
    el <- config$fields[i, ]
    x <- info[[el$name]]
    if (!is.null(x)) {
      assert_type(x, el$type, fieldname(el$name))
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
  c("staticgraph", "interactivegraph", "data", "report")
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
    msg <- !file.exists(file.path(path, files))
    if (any(msg)) {
      stop(sprintf("File for %s does not exist: %s",
                   name, paste(files[msg], collapse = ", ")))
    }
    x[i] <- vcapply(file.path(path, files), read_lines, USE.NAMES = FALSE)
    attr(x, "files") <- unname(files)
  }
  x
}

recipe_read_check_artefacts <- function(x, filename) {
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
      if (length(x) > 3) {
        x <- x[1:3] # keep things reasonable
      }
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
    stop("Unknown artefact %s: %s",
         ngettext(length(unk), "type", "types"),
         paste(unk, collapse = ", "))
  }

  res <- t(vapply(seq_along(x), check_artefact, vector("list", 3L)))

  filenames <- unlist(res[, "filenames"])
  dups <- unique(filenames[duplicated(filenames)])
  if (length(dups) > 0L) {
    stop("Duplicate artefact filenames are not allowed: ",
         paste(dups, collapse = ", "))
  }

  res
}

recipe_read_check_resources <- function(x, filename, path) {
  if (is.null(x)) {
    return(NULL)
  }
  assert_character(x, sprintf("%s:%s", filename, "resouces"))
  msg <- x[!file.exists(file.path(path, x))]
  if (length(msg) > 0L) {
    stop("Declared resources missing: ", paste(msg, collapse = ", "))
  }
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

recipe_read_check_depends <- function(x, filename, config) {
  ## TODO: this is going to assume that the artefacts are all in place
  ## - that need not actually be the case here - so we need a flag on
  ## this function that indicates that we're actually going to try and
  ## run things!
  if (is.null(x)) {
    return(NULL)
  }
  assert_named(x, TRUE, sprintf("%s:%s", filename, "depends"))

  msg <- setdiff(names(x), orderly_list(config))
  if (length(msg) > 0L) {
    stop("Declared upstream reports missing: ", paste(msg, collapse = ", "))
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
    el$path <- orderly_find_report(el$id, name, config, draft = el$draft,
                                   must_work = TRUE)
    assert_named(el$use, TRUE, sprintf("%s:depends:%s:use", filename, name))
    ## TODO: should check that these are not listed as resources I think
    err <- !vapply(el$use, function(x) is.character(x) && length(x) == 1, TRUE)
    if (any(err)) {
      stop(sprintf("%s:depends:%s:use must all be scalar character",
                   filename, name))
    }

    el$filename = vapply(el$use, identity, character(1), USE.NAMES = FALSE)
    el$as = names(el$use)
    el$use <- NULL

    filename_full <- file.path(el$path, el$filename)

    msg <- !file.exists(filename_full)
    if (any(msg)) {
      stop(sprintf("Did not find file %s at %s",
                   el$filename[msg],
                   el$path))
    }
    el$hash <- hash_files(filename_full, FALSE)

    ## Bit of a faff here to get the format into something that will
    ## serialise and interrogate nicely.
    as.data.frame(el, stringsAsFactors = FALSE)
  }

  rbind_df(lapply(seq_along(x), check_use1))
}
