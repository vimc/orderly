##' Deploy generated shiny apps onto a shiny server
##' @title Deploy shiny apps onto server
##' @param dest Destination (path) for the shiny apps
##' @param info Name of the shiny metadata file (relative to orderly root)
##' @inheritParams orderly_list
##' @export
orderly_deploy_shiny <- function(dest, info = "shiny.yaml",
                                 config = NULL, locate = TRUE) {
  assert_scalar_character(dest)
  config <- orderly_config_get(config, locate)
  info <- yaml::yaml.load_file(file.path(config$path, info))
  assert_named(info, TRUE, "shiny.yaml")
  for (i in seq_along(info)) {
    info[[i]]$dest <- names(info)[[i]]
  }
  info <- lapply(unname(info), orderly_deploy_shiny_check, dest, config)
  for (x in info) {
    orderly_deploy_1(x)
  }
}

orderly_deploy_shiny_check <- function(x, dest, config) {
  if (!all(c("name", "id") %in% names(x))) {
    stop(sprintf(
      "Invalid orderly.yml entry '%s' - expected properties 'name' and 'id'",
      x$dest))
  }
  if (x$id == "latest") {
    x$id <- orderly_latest(x$name, config)
  }
  x$path <- file.path("archive", x$name, x$id)
  if (!file.exists(x$path)) {
    stop(sprintf("archived orderly report does not exist: %s %s",
                 x$name, x$id))
  }
  x$path_shiny <- file.path(x$path, "shiny")
  if (!file.exists(x$path_shiny)) {
    stop(sprintf("archived orderly report does not include shiny: %s %s",
                 x$name, x$id))
  }
  x$path_dest <- file.path(dest, x$dest)
  x$path_id <- file.path(x$path_dest, "orderly_id")
  if (file.exists(x$path_dest)) {
    if (!file.exists(x$path_id)) {
      stop("did not find id file - not continuing")
    }
    x$id_prev <- readLines(x$path_id)
    x$deploy <- x$id_prev != x$id
  } else {
    x$deploy <- TRUE
  }
  x
}

orderly_deploy_1 <- function(x) {
  if (x$deploy) {
    message(sprintf("Copying %s:%s as %s", x$name, x$id, x$dest))
    if (file.exists(x$path_dest)) {
      unlink(x$path_dest, recursive = TRUE)
    }
    copy_directory(x$path_shiny, x$path_dest)
    writeLines(x$id, x$path_id)
  }
}
