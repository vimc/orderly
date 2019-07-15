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


path_archive_broken <- function() {
  "archive_broken"
}


path_orderly_config_yml <- function(root) {
  file.path(root, "orderly_config.yml")
}

path_orderly_run_rds <- function(path) {
  file.path(path, "orderly_run.rds")
}

path_orderly_envir_yml <- function(path) {
  file.path(path, "orderly_envir.yml")
}

path_runner_log <- function(path) {
  file.path(path, "runner", "log")
}
path_runner_id <- function(path) {
  file.path(path, "runner", "id")
}


## Used in migrations
path_orderly_run_rds_backup <- function(path, version) {
  file.path(path, sprintf("orderly_run_%s.rds", version))
}


path_orderly_archive_version <- function(path) {
  file.path(path, "archive", ".orderly_archive_version")
}


path_changelog_txt <- function(path, type) {
  file.path(path, "changelog.txt")
}


path_db_backup <- function(root, file) {
  file.path(root, "backup", "db", basename(file), fsep = "/")
}


path_demo_yml <- function(root) {
  file.path(root, "demo.yml")
}
