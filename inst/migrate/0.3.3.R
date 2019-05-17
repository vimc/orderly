migrate <- function(data, path, config) {
  if (!is.null(data$meta)) {
    return(migration_result(FALSE, data))
  }

  meta <- yaml_read(file.path(path, "orderly_run.yml"))
  if (!is.null(meta$depends)) {
    cols <- c("name", "id", "filename", "as", "hash")
    meta$depends <- as.data.frame(set_names(
      lapply(cols, function(v) vcapply(meta$depends, "[[", v)),
      cols),
      stringsAsFactors = FALSE)
  }

  data$meta <- meta
  migration_result(TRUE, data)
}
