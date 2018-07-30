migrate <- function(data, path) {
  if (!is.null(data$meta)) {
    return(list(changed = FALSE, data = data))
  }

  meta <- yaml_read(path_orderly_run_yml(path))
  if (!is.null(meta$depends)) {
    cols <- c("name", "id", "filename", "as", "hash")
    meta$depends <- as.data.frame(set_names(
      lapply(cols, function(v) vcapply(meta$depends, "[[", v)),
      cols),
      stringsAsFactors = FALSE)
  }

  data$meta <- meta
  list(changed = TRUE, data = data)
}
