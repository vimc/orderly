orderly_task_pack <- function(dest, name = NULL, parameters = NULL,
                              envir = NULL, root = NULL, locate = NULL,
                              message = NULL, instance = NULL,
                              remote = NULL, tags = NULL) {
  version <- orderly_version$new(name, root, locate)
  version$task_pack(dest, parameters, instance, envir, remote = NULL)
}


orderly_task_run <- function(path, workdir = tempfile(), echo = TRUE,
                             envir = NULL) {
  if (file.exists(workdir)) {
    ## TODO: ensure workdir is empty
    browser()
  }

  dir_create(workdir)
  zip::unzip(path, exdir = workdir)
  ## TODO: validate the archive:
  ## - contains expected files
  ## - manifest is correct
  ## - check signature
  ## - return name/id/paths etc
  id <- dir(workdir)
  path_pack <- file.path(workdir, id, "pack")
  path_meta <- file.path(workdir, id, "meta")
  info <- readRDS(file.path(workdir, id, "meta", "info.rds"))

  config <- readRDS(file.path(path_meta, "config.rds"))
  config$root <- path_meta
  recipe <- orderly_recipe$new(info$name, config, FALSE, path_pack)
  version <- orderly_version$new(info$name, config, FALSE)
  version$task_run(recipe, info, echo, envir)

  zip <- zip_dir(file.path(workdir, id))
  unlink(file.path(workdir, id), recursive = TRUE)
  zip
}

orderly_task_import <- function(path, root = NULL, locate = NULL) {
  config <- orderly_config_get(root, locate)

  tmp <- tempfile()
  dir.create(tmp)
  zip::unzip(path, exdir = tmp)

  ## TODO: validate the archive
  info <- orderly_task_info(path)

  name <- info$name
  id <- info$id

  ## TODO: validate that this pack was created by us?

  dest <- file.path(path_archive(config$root), name, id)
  label <- sprintf("%s:%s", name, id)
  if (file.exists(dest)) {
    stop(sprintf("%s already exists", label))
  }

  path_pack <- file.path(tmp, id, "pack")

  ## TODO: In the highly unlikely possibility that we upgraded orderly
  ## between exporting and running the task:
  migrate_single(path_pack, config)

  withCallingHandlers({
    copy_directory(path_pack, dest)
    report_db_import(name, id, config)
  }, error = function(e) unlink(dest, recursive = TRUE))
}


## Then we need something for interacting with these:
## orderly_task_list -> tasks and their status
## orderly_task_path -> given an id give the path to zip
## orderly_task_run -> given a path to an zip run it
## orderly_task_import -> given a path to a completed task import it

orderly_task_info <- function(path) {
  tmp <- tempfile()
  dir_create(tmp)
  on.exit(unlink(tmp))
  id <- sub("\\.zip$", "", basename(path))
  zip::unzip(path, sprintf("%s/meta/info.rds", id),
             junkpaths = TRUE, exdir = tmp)
  readRDS(file.path(tmp, "info.rds"))
}
