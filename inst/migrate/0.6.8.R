migrate <- function(data, path, config) {
  ## Inputs:
  info_readme <- info_sources <- info_resources <- info_global <- NULL
  if (length(data$meta$hash_readme) > 0L) {
    info_readme <- file_info(names(data$meta$hash_readme), path)
    stopifnot(identical(info_readme$file_hash,
                        list_to_character(data$meta$hash_readme, FALSE)))
  }

  if (length(data$meta$hash_sources) > 0L) {
    info_sources <- file_info(names(data$meta$hash_sources), path)
    stopifnot(identical(info_sources$file_hash,
                        list_to_character(data$meta$hash_sources, FALSE)))
  }

  if (length(data$meta$hash_resources) > 0L) {
    resources <- setdiff(names(data$meta$hash_resources),
                         info_sources$filename)
    info_resources <- file_info(resources, path)
  }

  if (length(data$meta$hash_global) > 0L) {
    info_global <- file_info(names(data$meta$hash_global), path)
    stopifnot(identical(info_global$file_hash,
                        list_to_character(data$meta$hash_global, FALSE)))

  }

  info_orderly_yml <- file_info(names(data$meta$hash_orderly_yml), path)
  info_script <- file_info(names(data$meta$hash_script), path)

  file_info_inputs <- file_in_data(
    orderly_yml = info_orderly_yml,
    script = info_script,
    readme = info_readme,
    source = info_sources,
    resource = info_resources,
    global = info_global)

  ## Data:
  if (!is.null(data$meta$data)) {
    hash_data <- data$meta$data$hash
    data$meta$data$size_csv <-
      file_size(orderly_db("csv", config)$filename(hash_data))
    data$meta$data$size_rds <-
      file_size(orderly_db("rds", config)$filename(hash_data))
  }

  ## Artefacts:
  hash_artefacts <- list_to_character(data$meta$hash_artefacts)
  info_artefacts <- data$meta$artefacts

  artefacts <- data_frame(
    format = list_to_character(info_artefacts[, "format"], FALSE),
    description = list_to_character(info_artefacts[, "description"], FALSE),
    order = seq_len(nrow(info_artefacts)))

  n <- lengths(info_artefacts[, "filenames"])
  file_info_artefacts <- data_frame(
    order = rep(seq_along(n), n),
    filename = names(hash_artefacts),
    file_hash = unname(hash_artefacts),
    file_size = file_size(file.path(path, names(hash_artefacts))))

  ## At this point we can clear out lots of old things:
  drop <- c("hash_orderly", "hash_orderly_yml", "hash_script", "hash_readme",
            "hash_sources", "hash_resources", "hash_global", "hash_data",
            "hash_artefacts")
  data$meta <- data$meta[setdiff(names(data$meta), drop)]
  data$meta$artefacts <- artefacts
  data$meta$file_info_inputs <- file_info_inputs
  data$meta$file_info_artefacts <- file_info_artefacts

  migration_result(TRUE, data)
}
