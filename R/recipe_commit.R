##' Commit a generated report
##' @title Commit a generated report
##'
##' @param id The identifier of the report
##'
##' @param name The name of the report - this can be ommited and the
##'   name will be determined from the \code{id}.
##'
##' @inheritParams orderly_list
##' @export
orderly_commit <- function(id, name = NULL, config = NULL, locate = TRUE) {
  config <- orderly_config_get(config, locate)
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

  ## TODO: this whole section here comes out when deprecating the old
  ## orderly table
  dat <- report_read_data(workdir, config)
  con <- orderly_db("destination", config)
  on.exit(DBI::dbDisconnect(con))

  ## Ensure that the db is in a reasonable state:
  report_db_init(con, config)
  ## Copy the _files_ over, but we'll roll this back if anything fails
  dest <- copy_report(workdir, dat$name, config)

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

report_read_data <- function(workdir, config) {
  assert_is(config, "orderly_config")
  yml <- path_orderly_run_yml(workdir)

  ## This does *not* go through the read_recipe bits because we want
  ## the unmodified yml contents here.
  info <- modify_list(yaml_read(file.path(workdir, "orderly.yml")),
                      yaml_read(yml))

  artefacts <- info$artefacts
  ## TODO: when dealing with VIMC-506, sort this out; this is due to
  ## the ordered map thing that yaml does.
  if (length(artefacts) == 1L && !is.null(names(artefacts))) {
    artefacts <- list(artefacts)
  }
  for (i in seq_along(artefacts)) {
    artefacts[[i]][[1]]$description <-
                        jsonlite::unbox(artefacts[[i]][[1]]$description)
  }

  published <- report_is_published(workdir)

  if (!is.null(info$depends)) {
    used <- unique(vcapply(info$depends, "[[", "id"))
    msg <- vlapply(used, function(id)
      is.null(orderly_find_name(id, config, draft = FALSE)))
    if (any(msg)) {
      err <- used[msg]
      is_draft <- vlapply(used, function(id)
        !is.null(orderly_find_name(id, config, draft = TRUE)))
      if (any(!is_draft)) {
      stop("Report uses nonexistant id:\n",
           paste(sprintf("\t- %s", err[!is_draft]), collapse = "\n"))
      }
      stop("Report uses draft id - commit first:\n",
           paste(sprintf("\t- %s", err), collapse = "\n"))
    }
  }

  ret <- data.frame(
    id = info$id,
    name = info$name,
    displayname = info$displayname %||% NA_character_,
    description = info$description %||% NA_character_,
    ## Inputs
    views = to_json_string(info$views),
    data = to_json_string(info$data),
    script = info$script,
    artefacts = to_json_string(artefacts, FALSE),
    resources = to_json_string_charvec(info$resources),
    hash_script = info$hash_script,
    ## Outputs
    parameters = to_json_string(info$parameters),
    date = info$date,
    hash_orderly = info$hash_orderly,
    hash_input = info$hash_input,
    hash_resources = to_json_string(info$hash_resources),
    hash_data = to_json_string(info$hash_data),
    hash_artefacts = to_json_string(info$hash_artefacts),
    published = jsonlite::unbox(published),
    depends = to_json_string(info$depends %||% list()),
    stringsAsFactors = FALSE)

  ## If specified, add custom fields.
  if (nrow(config$fields) > 0L) {
    custom <- info[config$fields$name]
    len <- lengths(custom)
    if (any(len == 0)) {
      names(custom) <- config$fields$name
      custom[config$fields$name[len == 0]] <-
        lapply(config$fields$type[len == 0], set_mode, x = NA)
    }
    ret <- cbind(ret, as.data.frame(custom, stringsAsFactors = FALSE))
  }
  ret
}


report_is_published <- function(workdir) {
  published_yml <- path_orderly_published_yml(workdir)
  file.exists(published_yml) && yaml_read(published_yml)$published
}
