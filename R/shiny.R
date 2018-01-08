##' Deploy generated shiny apps onto a shiny server
##' @title Deploy shiny apps onto server
##' @param dest Destination (path) for the shiny apps
##' @param info Name of the shiny metadata file (relative to orderly root)
##' @inheritParams orderly_list
##' @export
orderly_deploy_shiny <- function(dest, info = "shiny.yml",
                                 config = NULL, locate = TRUE) {
  assert_scalar_character(dest)
  config <- orderly_config_get(config, locate)
  dat <- yaml_read(file.path(config$path, info))
  apps <- dat$apps
  check_fields(dat, info, c("apps", "title"), NULL)
  assert_named(apps, TRUE, "shiny.yaml")

  for (i in seq_along(apps)) {
    apps[[i]]$dest <- names(apps)[[i]]
  }
  apps <- lapply(unname(apps), orderly_deploy_shiny_check, dest, config)
  did <- vlapply(apps, orderly_deploy_1)
  if (any(did)) {
    orderly_log("shiny", "writing index")
    ## TODO: can't link back to the report server until we have shiny
    ## behind a proxy.
    fmt <- '  <li><a href="%s">%s</a> (%s)</li>'
    app_dest <- vcapply(apps, "[[", "dest")
    app_displayname <- vcapply(apps, "[[", "displayname")
    app_id <- vcapply(apps, "[[", "id")
    app_list <- sprintf(fmt, app_dest, app_displayname, app_id)
    app_ul <- paste0(strrep(" ", 8L), c("<ul>", app_list, "</ul>"), "\n",
                     collapse = "")

    index <- readLines(orderly_file("shiny_index.html"))
    index <- gsub("{{TITLE}}", dat$title, index, fixed = TRUE)
    index <- sub("{{APPS}}", app_ul, index, fixed = TRUE)
    writeLines(index, file.path(dest, "index.html"))
  }
}

orderly_deploy_shiny_check <- function(x, dest, config) {
  check_fields(x, paste0("shiny.yml:", x$dest), c("name", "id", "dest"), NULL)
  if (x$id == "latest") {
    x$id <- orderly_latest(x$name, config)
  }
  x$path <- file.path(config$path, "archive", x$name, x$id)
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

  dat <- yaml_read(file.path(x$path, "orderly.yml"))
  x$displayname <- dat$displayname %||% x$name

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
    orderly_log("shiny", sprintf("Copying %s:%s as %s", x$name, x$id, x$dest))
    if (file.exists(x$path_dest)) {
      unlink(x$path_dest, recursive = TRUE)
    }
    copy_directory(x$path_shiny, x$path_dest)
    writeLines(x$id, x$path_id)
  }
  x$deploy
}
