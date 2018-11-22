remote_report_pull_archive_path <- function(name, id, config, remote) {
  src <- file.path(path_archive(remote), name, id)
  dest <- file.path(path_archive(config$path), name, id)
  copy_directory(src, dest, TRUE)
}


orderly_remote_path <- function(path) {
  assert_file_exists(path)
  path <- normalizePath(path, mustWork = TRUE)
  if (!file.exists(path_orderly_config_yml(path))) {
    stop("Does not look like an orderly repository: ", squote(path))
  }
  structure(path,
            class = c("orderly_remote_path",
                      "orderly_remote_location"))
}


remote_report_names_path <- function(remote) {
  orderly_list(orderly_config(remote))
}


remote_report_versions_path <- function(name, remote) {
  config_remote <- orderly_config(remote)
  d <- orderly_list_archive(config_remote)
  d$id[d$name == name]
}


push_archive_path <- function(name, id, config, remote, overwrite = FALSE) {
  src <- file.path(path_archive(config$path), name, id)
  dest <- file.path(path_archive(remote), name, id)
  copy_directory(src, dest, rollback_on_error = TRUE)
}
