migrate <- function(data, path, config) {
  if (!is.null(data$archive_version) && data$archive_version >= "0.6.2") {
    migration_result(FALSE, data)
  }

  prev <- data

  filename <- file.path(path, "orderly.yml")
  dat_yml <- yaml_read(filename)

  data$meta$displayname <- dat_yml$displayname %||% NA_character_
  data$meta$description <- dat_yml$description %||% NA_character_

  names(data$meta$hash_script) <- dat_yml$script
  data$meta$hash_orderly_yml <-
    set_names(hash_files(file.path(path, "orderly.yml")), "orderly.yml")

  extra_fields <- drop_null(set_names(
    lapply(config$fields$name, function(x) dat_yml[[x]]),
    config$fields$name))
  if (length(extra_fields) > 0L) {
    data$meta$extra_fields <- as_data_frame(extra_fields)
  }

  data$meta$artefacts <-
    recipe_read_check_artefacts(dat_yml$artefacts, filename, path, FALSE)

  if (!is.null(dat_yml$sources)) {
    data$meta$hash_sources <- data$meta$hash_resources[dat_yml$sources]
  }
  if (!is.null(dat_yml$packages)) {
    data$meta$packages <- dat_yml$packages
  }

  updated <- !identical(prev, data)
  migration_result(updated, data)
}
