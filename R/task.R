orderly_task_pack <- function(dest, name = NULL, parameters = NULL,
                              envir = NULL, root = NULL, locate = NULL,
                              message = NULL, instance = NULL,
                              remote = NULL, tags = NULL) {
  version <- orderly_version$new(name, root, locate)
  version$task_pack(dest, parameters, instance, envir, remote = NULL)
}


orderly_task_run <- function(path, workdir = tempfile(), echo = TRUE,
                             envir = NULL) {
  dir_create(workdir)

  info <- orderly_task_info(path)
  id <- info$id
  if (file.exists(file.path(workdir, id))) {
    stop(sprintf(
      "Can't unpack task '%s' here; it has already been extracted", id))
  }

  if (orderly_task_complete(path)) {
    stop(sprintf("Task '%s' has already been run", id))
  }

  zip::unzip(path, exdir = workdir)

  ## TODO: validate the archive:
  ## - contains expected files
  ## - manifest is correct
  ## - check signature
  ## - return name/id/paths etc

  path_pack <- file.path(workdir, id, "pack")
  path_meta <- file.path(workdir, id, "meta")

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


orderly_task_list <- function(path) {
  f <- function(p) {
    info <- orderly_task_info(p)
    status <- orderly_task_status(p)
    data_frame(id = info$id,
               status = status,
               name = info$name,
               parameters = I(list(info$parameters)),
               time = info$preflight_info$time)
  }

  files <- orderly_task_list_files(path)
  do.call("rbind", lapply(files, f))
}


orderly_task_list_files <- function(path) {
  assert_is_directory(path)
  sort(dir(path, full.names = TRUE,
           pattern = "[0-9]{8}-[0-9]{6}-[[:xdigit:]]{8}\\.zip$"))
}


orderly_task_status <- function(path) {
  if (orderly_task_complete(path)) "complete" else "incomplete"
}


orderly_task_complete <- function(path) {
  re <- "^[0-9]{8}-[0-9]{6}-[[:xdigit:]]{8}/pack/orderly_run.rds$"
  any(grepl(re, zip::zip_list(path)$filename))
}


orderly_task_info <- function(path) {
  tmp <- tempfile()
  dir_create(tmp)
  on.exit(unlink(tmp))
  id <- sub("\\.zip$", "", basename(path))
  zip::unzip(path, sprintf("%s/meta/info.rds", id),
             junkpaths = TRUE, exdir = tmp)
  readRDS(file.path(tmp, "info.rds"))
}
