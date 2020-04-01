resolve_dependencies <- function(depends, config, use_draft, parameters,
                                 remote) {
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
      res <- resolve_dependencies_local(id, name, config, parameters,
                                        use_draft_i)
    } else {
      res <- resolve_dependencies_remote(id, name, config, remote)
    }
    info <- resolve_dependencies_validate(id, name, res$path, filename)
    extra <- as_data_frame(c(res, info))
    depends_split[[i]] <- cbind(depends_split[[i]], extra)
  }

  rbind_df(depends_split)
}


resolve_dependencies_local <- function(id, name, config, parameters,
                                       use_draft) {
  is_latest <- grepl("^latest(\\(|$)", id)
  if (grepl("^latest\\s*\\(", id)) {
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
  if (grepl("^latest\\s*\\(", id)) {
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
