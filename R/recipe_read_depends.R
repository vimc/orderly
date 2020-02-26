recipe_read_check_depends <- function(x, filename, config, use_draft,
                                      validate) {
  ## TODO: this is going to assume that the artefacts are all in place
  ## - that need not actually be the case here - so we need a flag on
  ## this function that indicates that we're actually going to try and
  ## run things!
  if (is.null(x)) {
    return(NULL)
  }

  ## Deal with yaml weirdness:
  if (is.null(names(x))) {
    x <- ordered_map_to_list(x)
  }

  ## This part is *just* the bits that can be easily dealt with for
  ## any valid orderly.yml; it does not actually check that these
  ## dependencies exist or that the files declared in them are there.
  ## This is because we might run this in situations where we want to
  ## fetch these dependencies!
  check_depends1 <- function(i) {
    name <- names(x)[[i]]
    el <- x[[i]]
    v <- c("id", "use")
    check_fields(el, sprintf("%s:depends:%s", filename, name), v, "draft")
    el$name <- name

    assert_character(el$id, sprintf("%s:depends:%s:id", filename, name))
    if (!is.null(el$draft)) {
      msg <- c("Using 'draft:' within an ordery.yml is deprecated and",
               "will be removed in a future version of orderly.  Please",
               "use the 'use_draft' argument to control draft usage.",
               "If you want to use a recent version of a report that you",
               'are developing simultaneously, use_draft = "newer"',
               "will probably do what you want.")
      orderly_warning(flow_text(msg))
      assert_scalar_logical(el$draft,
                            sprintf("%s:depends:%s:draft", filename, name))
    } else {
      el$draft <- NA
    }

    assert_named(el$use, TRUE, sprintf("%s:depends:%s:use", filename, name))
    err <- !vlapply(el$use, function(x) is.character(x) && length(x) == 1)
    if (any(err)) {
      stop(sprintf("%s:depends:%s:use must all be scalar character",
                   filename, name))
    }

    el$filename <- list_to_character(el$use, FALSE)
    el$as <- names(el$use)
    el$use <- NULL
    el$is_pinned <- el$id != "latest"
    el$index <- i

    ## Bit of a faff here to get the format into something that will
    ## serialise and interrogate nicely.
    as_data_frame(el)
  }

  depends <- rbind_df(lapply(seq_along(x), check_depends1))

  remote <- NULL
  if (validate) {
    depends <- resolve_dependencies(depends, config, use_draft, remote)
  }

  depends
}


resolve_dependencies <- function(depends, config,
                                 use_draft = FALSE, remote = NULL) {
  assert_is(config, "orderly_config")
  if (is.null(depends)) {
    return(NULL)
  }

  if (!is.null(remote)) {
    if (use_draft) {
      stop("Can't use 'use_draft' with remote")
    }
    remote <- get_remote(remote, config)
  }

  depends_split <- unname(split(depends, depends$index))
  for (i in seq_along(depends_split)) {
    name <- depends_split[[i]]$name[[1]]
    id <- depends_split[[i]]$id[[1]]
    filename <- depends_split[[i]]$filename
    if (is.null(remote)) {
      ## This is ugly but needs to stay for another couple of versions
      use_draft_i <- depends_split[[i]]$draft[[1]]
      use_draft_i <- if (is.na(use_draft_i)) use_draft else use_draft_i
      res <- resolve_dependencies_local(id, name, config, use_draft_i)
    } else {
      res <- resolve_dependencies_remote(id, name, config, remote)
    }
    info <- resolve_dependencies_validate(id, name, res$path, filename)
    extra <- as_data_frame(c(res, info))
    depends_split[[i]] <- cbind(depends_split[[i]], extra)
  }

  rbind_df(depends_split)
}


resolve_dependencies_local <- function(id, name, config, use_draft) {
  path <- orderly_find_report(id, name, config, draft = use_draft,
                              must_work = TRUE)
  is_latest <- id == "latest" ||
    basename(path) == orderly_latest(name, config, draft = use_draft)
  list(path = path, is_latest = is_latest)
}


resolve_dependencies_remote <- function(id, name, config, remote) {
  remote <- get_remote(remote, config)

  versions <- remote$list_versions(name)
  latest_version <- versions[length(versions)]
  if (id == "latest" && length(versions) > 0L) {
    id <- versions[[length(versions)]]
    is_latest <- TRUE
  } else if (!(id %in% versions)) {
    stop(sprintf("Did not find report %s:%s on remote %s",
                 name, id, remote$name))
  } else {
    is_latest <- id == versions[[length(versions)]]
  }
  orderly_pull_archive(name, id, config, FALSE, remote)
  path <- orderly_find_report(id, name, config, draft = FALSE,
                              must_work = TRUE)
  list(path = path, is_latest = is_latest)
}


resolve_dependencies_validate <- function(id, name, path, filename) {
  run_info <- readRDS(path_orderly_run_rds(path))
  file_info <- run_info$meta$file_info_artefacts

  filename_full <- file.path(path, filename)
  msg <- !file.exists(file.path(path, filename))
  if (any(msg)) {
    stop(sprintf("Did not find file %s at %s",
                 paste(squote(filename[msg]), collapse = ", "),
                 path))
  }

  ## VIMC-2017: check that a file is actually an artefact
  ## VIMC-889: enforce case sensitivity of filenames
  i <- match(filename, file_info$filename)
  err <- is.na(i)
  if (any(err)) {
    stop(sprintf(
      "Dependency %s not an artefact of %s/%s:\n%s",
      ngettext(sum(err), "file", "files"),
      name, basename(path),
      paste(sprintf("- '%s'", filename[err]), collapse = "\n")),
      call. = FALSE)
  }

  hash <- hash_files(filename_full, FALSE)
  err <- hash != file_info$file_hash[i]
  if (any(err)) {
    stop(sprintf(
      paste("Validation of dependency %s (%s/%s) failed:",
            "artefact has been modified"),
      paste(squote(filename[err]), collapse = ", "), name, id),
      call. = FALSE)
  }

  list(hash = hash, time = run_info$time)
}
