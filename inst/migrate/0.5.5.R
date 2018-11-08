migrate <- function(data, path, config) {
  if (!is.null(data$meta$connection)) {
    return(migration_result(FALSE, data))
  }
  d <- recipe_read(path, config, FALSE)
  data$meta$connection <- !is.null(d$connection)
  migration_result(TRUE, data)
}
