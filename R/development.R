orderly_status <- function(path = NULL) {
  path <- path %||% getwd()
  assert_is_directory(path, FALSE)
  assert_file_exists(file.path(path, "orderly.yml"))
  ## TODO: this needs making more robust
  config <- orderly_config_get(file.path(path, "..", ".."), FALSE)
  info <- recipe_read(path, config, FALSE)

  ## There are the file that orderly cares about (TODO, does source get here?)
  internal <- list(orderly = "orderly.yml",
                   script = info$script,
                   source = info$sources,
                   resource = info$resources,
                   dependency = info$depends$as,
                   artefact = unlist(info$artefacts[, "filenames"]))
  status <- data_frame(
    filename = unlist(internal, FALSE, FALSE),
    type = rep(names(internal), lengths(internal)))
  extra <- setdiff(dir(path, recursive = TRUE), status$filename)
  if (length(extra) > 0L) {
    status <- rbind(status, data_frame(filename = extra, type = "unknown"))
  }
  status$present <- file.exists(file.path(path, status$filename))
  ## status$origin <- NA_character_
  ## This is going to be fairly hard to get right.  Better might be to
  ## check if it is *consistent*?
  ## if (any(status$type == "dependency" & status$present)) {
  ##   con <- orderly_db("destination", config)
  ##   browser()
  ## }

  class(status) <- c("orderly_status", "data.frame")
  status
}
