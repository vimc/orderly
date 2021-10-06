##' Pack up and run orderly reports to run elsewhere. By using these
##' functions you can safely copy all requirements of an orderly
##' report into a portable archive and run them on another machine
##' (perhaps a cluster or HPC), then import the completed archive into
##' your orderly tree. There is considerable overhead to using these
##' functions (mostly due to transport costs) so they are intended
##' primarily for very computationally demanding patterns.
##'
##' @title Pack and run orderly "bundles"
##'
##' @param path A path, whose interpretation depends on the function:
##'
##' `orderly_bundle_pack`: A directory to save bundles to.  If
##'   it does not exist it will be created for you.
##'
##' `orderly_bundle_run`: The path to the packed bundle (a zip
##'   file created by `orderly_bundle_pack`)
##'
##' `orderly_bundle_import`: The path to unpack and import
##'   (a zip file created by `orderly_bundle_run`)
##'
##' `orderly_bundle_list`: The path to a directory that might
##'   contain either incomplete or complete bundles (created by either
##'   `orderly_bundle_pack` or `orderly_bundle_run`)
##'
##' @param name Name of the report to pack (see
##'   [orderly::orderly_list()].  A leading `src/` will be removed if
##'   provided, allowing easier use of autocomplete.
##'
##' @inheritParams orderly_run
##'
##' @return For `orderly_bundle_pack` and
##'   `orderly_bundle_run`, a list with elements `path` (the
##'   path to the bundle) and `id` (its orderly id).  For
##'   `orderly_bundle_list` a data.frame with key information
##'   about the report in the bundles (id, name, parameters, status,
##'   time).  The function `orderly_bundle_import` is called for
##'   its side effect only and does not return anything useful.
##'
##' @export
##' @examples
##' path <- orderly::orderly_example("minimal")
##'
##' # A working directory to export bundles to:
##' workdir <- tempfile()
##'
##' # Pack up the "example" report to go:
##' res <- orderly::orderly_bundle_pack(workdir, "example", root = path)
##'
##' # The return value is a list with the id and the path to the zip
##' # file created:
##' res
##'
##' # A list of reports bundled in this directory and their status
##' orderly::orderly_bundle_list(workdir)
##'
##' # Run the bundle (this would ordinarily be done on another computer)
##' zip <- orderly::orderly_bundle_run(res$path, workdir)
##' zip
##'
##' # The status has now been updated to reflect the status
##' orderly::orderly_bundle_list(workdir)
##'
##' # We can import this into the orderly tree
##' orderly::orderly_bundle_import(zip$path, root = path)
##'
##' # This has now been included in your orderly archive and the
##' # workdir can be safely deleted
##' unlink(workdir, recursive = TRUE)
##' orderly::orderly_list_archive(path)
orderly_bundle_pack <- function(path, name, parameters = NULL,
                              envir = NULL, root = NULL, locate = TRUE,
                              message = NULL, instance = NULL,
                              remote = NULL, tags = NULL) {
  version <- orderly_version$new(name, root, locate)
  version$bundle_pack(path, parameters, instance, envir, remote = NULL)
}


##' @param workdir The path in which to run bundles.  If it does not
##'   exist it will be created for you.  The completed bundle will be
##'   saved in this directory as `<id>.zip`.
##'
##' @inheritParams orderly_run
##' @rdname orderly_bundle_pack
##' @export
orderly_bundle_run <- function(path, workdir = tempfile(), echo = TRUE,
                               envir = NULL) {
  assert_file_exists(path, check_case = FALSE)
  dir_create(workdir)

  info <- orderly_bundle_info(path)
  id <- info$id
  if (file.exists(file.path(workdir, id))) {
    stop(sprintf(
      "Can't unpack bundle '%s' here; it has already been extracted", id))
  }

  if (orderly_bundle_complete(path)) {
    stop(sprintf("Bundle '%s' has already been run", id))
  }

  zip::unzip(path, exdir = workdir)

  ## TODO(VIMC-3975): validate the archive before run

  path_pack <- file.path(workdir, id, "pack")
  path_meta <- file.path(workdir, id, "meta")

  config <- readRDS(file.path(path_meta, "config.rds"))
  config$root <- NULL
  recipe <- orderly_recipe$new(info$name, config, FALSE, path_pack)
  version <- orderly_version$new(info$name, config, FALSE)
  version$bundle_run(recipe, info, echo, envir)

  zip <- zip_dir(file.path(workdir, id))
  unlink(file.path(workdir, id), recursive = TRUE)
  list(id = id, path = zip, filename = basename(zip))
}



##' @rdname orderly_bundle_pack
##' @export
orderly_bundle_import <- function(path, root = NULL, locate = TRUE) {
  assert_file_exists(path, check_case = FALSE)
  config <- orderly_config(root, locate)

  ## TODO(VIMC-3975): validate the archive before import
  info <- orderly_bundle_info(path)

  tmp <- tempfile()
  dir.create(tmp)
  zip::unzip(path, exdir = tmp)

  name <- info$name
  id <- info$id
  contents <- zip_list2(path)$filename

  if (!(sprintf("%s/pack/orderly_run.rds", id) %in% contents)) {
    stop("This does not look like a complete bundle (one that has been run)")
  }

  dest <- file.path(path_archive(config$root), name, id)
  label <- sprintf("%s:%s", name, id)
  if (file.exists(dest)) {
    stop(sprintf("%s already exists", label))
  }

  path_pack <- file.path(tmp, id, "pack")

  ## NOTE: In the highly unlikely possibility that we upgraded orderly
  ## between exporting and running the bundle:
  migrate_single(path_pack, config)

  withCallingHandlers({
    copy_directory(path_pack, dest)
    report_db_import(name, id, config)
  }, error = function(e) unlink(dest, recursive = TRUE))
}


##' @rdname orderly_bundle_pack
##' @export
orderly_bundle_list <- function(path) {
  assert_file_exists(path, check_case = FALSE)
  f <- function(p) {
    info <- orderly_bundle_info(p)
    status <- orderly_bundle_status(p)
    data_frame(id = info$id,
               name = info$name,
               parameters = I(list(info$parameters)),
               status = status,
               time = info$preflight_info$time)
  }

  files <- orderly_bundle_list_files(path)
  do.call("rbind", lapply(files, f))
}


orderly_bundle_list_files <- function(path) {
  assert_is_directory(path, FALSE)
  sort(dir(path, full.names = TRUE,
           pattern = "[0-9]{8}-[0-9]{6}-[[:xdigit:]]{8}\\.zip$"))
}


orderly_bundle_status <- function(path) {
  if (orderly_bundle_complete(path)) "complete" else "incomplete"
}


orderly_bundle_complete <- function(path) {
  re <- "^[0-9]{8}-[0-9]{6}-[[:xdigit:]]{8}/pack/orderly_run.rds$"
  any(grepl(re, zip_list2(path)$filename))
}


orderly_bundle_info <- function(path) {
  id <- fs::path_split(zip_list2(path)$filename[[1]])[[1]]

  tmp <- tempfile()
  dir_create(tmp)
  on.exit(unlink(tmp))

  tryCatch(
    zip::unzip(path, sprintf("%s/meta/info.rds", id), exdir = tmp,
               junkpaths = TRUE),
    error = function(e)
      stop(sprintf("Failed to extract bundle info from '%s'\n(%s)",
                   path, e$message), call. = FALSE))
  readRDS(file.path(tmp, "info.rds"))
}


## This is a temporary workaround for zip, which has an issue running
## zip_list on large archives. Fix for zip will be submitted in a PR
## soon.
zip_list2 <- function(path) {
  tryCatch(zip::zip_list(path),
           error = function(e)
             tryCatch(zip_list_base(path),
                      error = function(e2) stop(e)))
}


zip_list_base <- function(path) {
  list <- utils::unzip(path, list = TRUE)
  data.frame(filename = list$Name, stringsAsFactors = FALSE)
}
