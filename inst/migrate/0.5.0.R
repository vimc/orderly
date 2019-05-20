.order <- NULL
get_order <- function(root) {
  if (is.null(.order)) {
    archive <- orderly_list_archive(root, FALSE)
    archive <- archive[order(archive$id), ]
    rownames(archive) <- NULL
    .order <<- archive
  }
  .order
}


migrate <- function(data, path, config) {
  if (is.null(data$meta$depends)) {
    return(migration_result(FALSE, data))
  }
  if ("is_pinned" %in% names(data$meta$depends)) {
    return(migration_result(FALSE, data))
  }

  recipe <- recipe_read(path, config, FALSE)
  depends <- recipe$depends

  id_requested <- depends$id
  is_pinned <- id_requested != "latest"

  if (any(is_pinned)) {
    archive <- get_order(config$root)
    tmp <- archive[archive$id < data$meta$id &
                   archive$name %in% data$meta$depends$name, ]
    is_latest <- id_requested %in% tapply(tmp$id, tmp$name, max)
  } else {
    is_latest <- rep(TRUE, length(id_requested))
  }

  ok <- isTRUE(all.equal(
    data$meta$depends[c("name", "filename", "as")],
    depends[c("name", "filename", "as")]))
  stopifnot(ok)

  data$meta$depends$id_requested <- id_requested
  data$meta$depends$is_latest <- is_latest
  data$meta$depends$is_pinned <- is_pinned

  ## Before fixing VIMC-2017 non-artefact files can be depended on,
  ## which does not fit with the data schema we're going to use.  So
  ## we're going to just discard these here.  It's not wonderful but
  ## it affects very few reports and seems less obnoxious than
  ## modifying the upstream reports retrospectively to allow this
  ## link.
  d <- data$meta$depends

  p <- unique(file.path(path_archive(config$root), d$name, d$id))
  valid <- lapply(path_orderly_run_rds(p), function(x)
    names(readRDS(x)$meta$hash_artefacts))
  names(valid) <- basename(p)
  err <- !vlapply(seq_len(nrow(depends)), function(i)
    d$filename[[i]] %in% valid[[d$id[[i]]]])

  if (any(err)) {
    str <- c(
      sprintf("%s/%s: discarding %d invalid %s",
              data$meta$name, data$meta$id,
              sum(err),
              ngettext(sum(err), "dependency", "dependencies")),
      sprintf("- %s/%s : %s", d$name[err], d$id[err], d$filename[err]))
    orderly_log("warn", str)
    data$meta$depends <- data$meta$depends[!err, , drop = FALSE]
  }

  migration_result(TRUE, data)
}
