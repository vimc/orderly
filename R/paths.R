path_data <- function(root) {
  file.path(root, "data")
}
path_rds <- function(root) {
  file.path(path_data(root), "rds")
}
path_csv <- function(root) {
  file.path(path_data(root), "csv")
}
path_draft <- function(root) {
  file.path(root, "draft")
}
path_src <- function(root) {
  file.path(root, "src")
}
path_archive <- function(root, name = NULL) {
  if (is.null(name)) {
    file.path(root, "archive")
  } else {
    file.path(root, "archive", name)
  }
}
path_orderly_config_yml <- function(root) {
  file.path(root, "orderly_config.yml")
}

path_orderly_run_yml <- function(path) {
  file.path(path, "orderly_run.yml")
}
path_orderly_run_rds <- function(path) {
  file.path(path, "orderly_run.rds")
}
path_orderly_published_yml <- function(path) {
  file.path(path, "orderly_published.yml")
}
