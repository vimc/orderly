migrate <- function(data, path, config) {
  p <- file.path(path, "changelog.json")
  if (!file.exists(p)) {
    return(migration_result(FALSE, data))
  }

  name <- data$meta$name
  id <- data$meta$id

  d <- jsonlite::fromJSON(p)
  d <- cbind(id = NA_character_, d, stringsAsFactors = FALSE)
  i <- d$report_version == id
  d$id[i] <- ids::random_id(sum(i))

  j <- !i
  if (any(j)) {
    prev_id <- d$report_version[j][[1]]
    prev_path <- file.path(config$root, "archive", name, prev_id)
    prev_changelog <- readRDS(path_orderly_run_rds(prev_path))$meta$changelog
    stopifnot(nrow(prev_changelog) == sum(j))
    d$id[j] <- prev_changelog$id
  }

  data$meta$changelog <- d

  migration_result(TRUE, data)
}
