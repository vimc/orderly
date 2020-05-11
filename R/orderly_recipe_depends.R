recipe_validate_depends <- function(depends, config, filename) {
  if (is.null(depends)) {
    return(NULL)
  }

  ## Deal with yaml weirdness:
  if (is.null(names(depends))) {
    depends <- ordered_map_to_list(depends)
  }

  for (i in seq_along(depends)) {
    depends[[i]]$index <- i
    depends[[i]]$name <- names(depends)[[i]]
  }

  ret <- rbind_df(lapply(depends, recipe_validate_depend1, filename))
  rownames(ret) <- NULL
  ret
}


recipe_validate_depend1 <- function(depend, filename) {
  name <- depend$name
  v <- c("id", "use", "index", "name")
  check_fields(depend, sprintf("%s:depends:%s", filename, name), v, "draft")

  assert_character(depend$id, sprintf("%s:depends:%s:id", filename, name))
  if (!is.null(depend$draft)) {
    msg <- c("Using 'draft:' within an ordery.yml is deprecated and",
             "will be removed in a future version of orderly.  Please",
             "use the 'use_draft' argument to control draft usage.",
             "If you want to use a recent version of a report that you",
             'are developing simultaneously, use_draft = "newer"',
             "will probably do what you want.")
    orderly_warning(flow_text(msg))
    assert_scalar_logical(depend$draft,
                          sprintf("%s:depends:%s:draft", filename, name))
  } else {
    depend$draft <- NA
  }

  assert_named(depend$use, TRUE, sprintf("%s:depends:%s:use", filename, name))
  err <- !vlapply(depend$use, function(x) is.character(x) && length(x) == 1)
  if (any(err)) {
    stop(sprintf("%s:depends:%s:use must all be scalar character",
                 filename, name),
         call. = FALSE)
  }

  depend$filename <- list_to_character(depend$use, FALSE)
  depend$as <- names(depend$use)
  depend$use <- NULL
  depend$is_pinned <- depend$id != "latest"

  ## Bit of a faff here to get the format into something that will
  ## serialise and interrogate nicely.
  as_data_frame(depend)
}


recipe_resolve_dependencies <- function(self, use_draft, parameters, remote) {
  if (is.null(self$depends)) {
    return(NULL)
  }

  if (!is.null(remote)) {
    if (use_draft) {
      stop("Can't use 'use_draft' with remote")
    }
    remote <- get_remote(remote, self$config)
  }

  depends_split <- unname(split(self$depends, self$depends$index))
  for (i in seq_along(depends_split)) {
    name <- depends_split[[i]]$name[[1]]
    id <- depends_split[[i]]$id[[1]]
    filename <- depends_split[[i]]$filename
    if (is.null(remote)) {
      ## This is ugly but needs to stay for another couple of versions
      use_draft_i <- depends_split[[i]]$draft[[1]]
      use_draft_i <- if (is.na(use_draft_i)) use_draft else use_draft_i
      res <- resolve_dependencies_local(id, name, self$config, parameters,
                                        use_draft_i)
    } else {
      res <- resolve_dependencies_remote(id, name, self$config, remote)
    }
    info <- resolve_dependencies_validate(id, name, res$path, filename)
    extra <- as_data_frame(c(res, info))
    depends_split[[i]] <- cbind(depends_split[[i]], extra)
  }

  depends <- rbind_df(depends_split)

  ## Could do this above, but this works ok at least for now:
  depends$id_requested <- depends$id
  depends$id <- basename(depends$path)

  self$depends <- depends
}


resolve_dependencies_local <- function(id, name, config, parameters,
                                       use_draft) {
  is_query <- id_is_query(id)
  is_latest <- is_query || id == "latest"
  if (is_query) {
    query <- id
    id <- orderly_search(query, name, parameters, draft = use_draft,
                         root = config, locate = FALSE)
    if (is.na(id)) {
      stop(sprintf("Query '%s' did not find suitable version", query),
           call. = FALSE)
    }
  }
  path <- orderly_find_report(id, name, config, draft = use_draft,
                              must_work = TRUE)
  is_latest <- is_latest ||
    basename(path) == orderly_latest(name, config, draft = use_draft)
  list(path = path, is_latest = is_latest)
}


resolve_dependencies_remote <- function(id, name, config, remote) {
  if (id_is_query(id)) {
    stop("Can't (yet) use query dependencies with remotes",
         call. = FALSE)
  }
  remote <- get_remote(remote, config)

  versions <- remote$list_versions(name)
  latest_version <- versions[length(versions)]
  if (id == "latest" && length(versions) > 0L) {
    id <- latest_version
    is_latest <- TRUE
  } else if (!(id %in% versions)) {
    stop(sprintf("Did not find report '%s:%s' on remote '%s'",
                 name, id, remote$name), call. = FALSE)
  } else {
    is_latest <- id == latest_version
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
