migrate <- function(data, path, config) {
  if (!is.null(data$archive_version) && data$archive_version >= "0.6.0") {
    migration_result(FALSE, data)
  }

  updated <- FALSE
  dat_in <- yaml_read(file.path(path, "orderly.yml"))

  if (is.null(data$meta$data) && length(data$meta$hash_data) > 0) {
    updated <- TRUE
    data$meta$data <- data_frame(
      name = names(dat_in$data),
      database = "source",
      query = list_to_character(dat_in$data, FALSE),
      hash = list_to_character(data$meta$hash_data, FALSE))
  }

  if (is.null(data$meta$view) && !is.null(dat_in$views)) {
    updated <- TRUE
    data$meta$view <- data_frame(
      name = names(dat_in$views),
      database = "source",
      query = list_to_character(dat_in$views, FALSE))
  }

  migration_result(updated, data)
}
