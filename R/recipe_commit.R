##' Commit a generated report
##' @title Commit a generated report
##'
##' @param id The identifier of the report
##'
##' @param name The name of the report - this can be omitted and the
##'   name will be determined from the \code{id}.
##'
##' @inheritParams orderly_list
##' @export
orderly_commit <- function(id, name = NULL, path = NULL, locate = TRUE) {
  config <- orderly_config_get(path, locate)
  check_orderly_archive_version(config)
  if (is.null(name)) {
    name <- orderly_find_name(id, config, draft = TRUE, must_work = TRUE)
  } else {
    if (!file.exists(file.path(path_draft(config$path), name, id))) {
      stop(sprintf("Did not find draft report %s/%s", name, id))
    }
  }
  workdir <- file.path(path_draft(config$path), name, id)
  recipe_commit(workdir, config$path)
}

recipe_commit <- function(workdir, config) {
  config <- orderly_config_get(config)
  name <- basename(dirname(workdir))
  id <- basename(workdir)
  orderly_log("commit", sprintf("%s/%s", name, id))

  if (!file.exists(path_orderly_run_rds(workdir))) {
    stop(sprintf("Did not find run metadata file for %s/%s", name, id),
         call. = FALSE)
  }

  ## At this point we just won't support migrating drafts because it's
  ## lots easier not to!
  v <- get_version(readRDS(path_orderly_run_rds(workdir))$archive_version)
  if (v < cache$current_archive_version) {
    stop("This report was built with an old version of orderly; please rebuild")
  }

  con <- orderly_db("destination", config)
  on.exit(DBI::dbDisconnect(con))

  ## Ensure that the db is in a reasonable state:
  report_db_init(con, config)
  ## Copy the _files_ over, but we'll roll this back if anything fails
  dest <- copy_report(workdir, name, config)

  DBI::dbBegin(con)
  withCallingHandlers(
    report_data_import(con, workdir, config),
    error = function(e) {
      unlink(dest, recursive = TRUE)
      tryCatch(DBI::dbRollback(con), error = function(e) NULL)
    })
  DBI::dbCommit(con)

  ## After success we can delete the draft directory
  unlink(workdir, recursive = TRUE)

  orderly_log("success", ":)")
  dest
}

## Change this from using an arbitrary workdir to using the draft
## directory perhaps?
copy_report <- function(workdir, name, config) {
  assert_is(config, "orderly_config")
  id <- basename(workdir)
  parent <- path_archive(config$path, name)
  dest <- file.path(parent, id)
  if (file.exists(dest)) {
    ## This situation probably needs help to resolve but I don't know
    ## what conditions might trigger it.  The most obvious one is that
    ## windows file-locking has prevented deletion of a draft report
    stop(sprintf(
      "Report %s/%s appears to have already been copied!", name, id),
      call. = FALSE)
  }
  dir_create(parent)
  orderly_log("copy", "")
  file_copy(workdir, parent, recursive = TRUE)
  dest
}


report_is_published <- function(workdir) {
  published_yml <- path_orderly_published_yml(workdir)
  file.exists(published_yml) && yaml_read(published_yml)$published
}
