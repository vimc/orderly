##' Pack up and run orderly tasks to run elsewhere. By using these
##' functions you can safely copy all requirements of an orderly
##' report into a portable archive and run them on another machine
##' (perhaps a cluster or HPC), then import the completed archive into
##' your orderly tree. There is considerable overhead to using these
##' functions (mostly due to transport costs) so they are intended
##' primarily for very computationally demanding patterns.
##'
##' @title Pack and run orderly "tasks"
##'
##' @param path A path, whose interpretation depends on the function:
##'
##' \describe{
##'
##' \item{\code{orderly_task_pack}}{A directory to save tasks to.  If
##'   it does not exist it will be created for you.}
##'
##' \item{\code{orderly_task_run}}{The path to the packed task (a zip
##'   file created by \code{orderly_task_pack})}
##'
##' \item{\code{orderly_task_run}}{The path to the run task task (a
##'   zip file created by \code{orderly_task_run}}
##'
##' \item{\code{orderly_task_list}}{The path to a directory that might
##'   contain either incomplete or complete tasks (created by either
##'   \code{orderly_task_pack} or \code{orderly_task_run})}
##'
##' }
##'
##' @param name Name of the report to pack (see
##'   \code{\link{orderly_list}}).  A leading \code{src/} will be
##'   removed if provided, allowing easier use of autocomplete.
##'
##' @inheritParams orderly_run
##'
##' @return For \code{orderly_task_pack} and \code{orderly_task_run},
##'   a list with elements \code{path} (the pth to the packed task)
##'   and \code{id} (its orderly id).  For \code{orderly_task_list} a
##'   data.frame with key information about the tasks (id, name,
##'   parameters, status, time).  The function
##'   \code{orderly_task_import} is called for its side effect only
##'   and does not return anything useful.
##'
##' @export
##' @examples
##' path <- orderly::orderly_example("minimal")
##'
##' # A working directory to export tasks to:
##' workdir <- tempfile()
##'
##' # Pack up the "example" report to go:
##' res <- orderly::orderly_task_pack(workdir, "example", root = path)
##'
##' # The return value is a list with the id and the path to the zip
##' # file created:
##' res
##'
##' # A list of tasks saved to this directory and their status
##' orderly::orderly_task_list(workdir)
##'
##' # Run the task (this would ordinarily be done on another computer)
##' zip <- orderly::orderly_task_run(res$path, workdir)
##' zip
##'
##' # The status has now been updated to reflect the status
##' orderly::orderly_task_list(workdir)
##'
##' # We can import this into the orderly tree
##' orderly::orderly_task_import(zip$path, root = path)
##'
##' # This has now been included in your orderly archive and the
##' # workdir can be safely deleted
##' unlink(workdir, recursive = TRUE)
##' orderly::orderly_list_archive(path)
orderly_task_pack <- function(path, name, parameters = NULL,
                              envir = NULL, root = NULL, locate = NULL,
                              message = NULL, instance = NULL,
                              remote = NULL, tags = NULL) {
  version <- orderly_version$new(name, root, locate)
  version$task_pack(path, parameters, instance, envir, remote = NULL)
}


##' @param workdir The path in which to run tasks.  If it does not
##'   exist it will be created for you.  The completed task will be
##'   saved in this directory as \code{<id>.zip}.
##'
##' @inheritParams orderly_run
##' @rdname orderly_task_pack
##' @export
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
  config$root <- NULL
  recipe <- orderly_recipe$new(info$name, config, FALSE, path_pack)
  version <- orderly_version$new(info$name, config, FALSE)
  version$task_run(recipe, info, echo, envir)

  zip <- zip_dir(file.path(workdir, id))
  unlink(file.path(workdir, id), recursive = TRUE)
  list(id = id, path = zip)
}



##' @rdname orderly_task_pack
##' @export
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


##' @rdname orderly_task_pack
##' @export
orderly_task_list <- function(path) {
  f <- function(p) {
    info <- orderly_task_info(p)
    status <- orderly_task_status(p)
    data_frame(id = info$id,
               name = info$name,
               parameters = I(list(info$parameters)),
               status = status,
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
