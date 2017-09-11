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

  dat <- report_read_data(workdir, config)
  con <- orderly_db("destination", config)
  ## Ensure that the db is in a reasonable state:
  tbl <- report_db_init(con, config)
  ## Copy the _files_ over, but we'll roll this back if anything fails
  success <- FALSE
  dest <- copy_report(workdir, dat$name, config)
  on.exit({
    if (!success) unlink(dest, recursive = TRUE)
    DBI::dbDisconnect(con)
  })

  success <- DBI::dbWriteTable(con, tbl, dat, append = TRUE)
  if (success) {
    ## I should do something here if the unlink fails, though I don't
    ## know exactly what.  But because a *second* round of copy here
    ## would be pretty catastophic we need to flag the draft here as
    ## being stale.  Perhaps write into it a file that indicates that
    ## it is done just before deletion?
    unlink(workdir, recursive = TRUE)
    if (file.exists(workdir)) {
      ## For now we just issue a warning which is pretty lame and
      ## can't really be acted on.  This will at least hopefully let
      ## me know how common an issue this is so that I can work out
      ## how much we should try and anticipate and handle this
      ## situation.
      warning(sprintf("Failed to delete workdir %s!", workdir)) # nocov
    }
  } else {
    ## Really not sure about this, and until the error handling is
    ## done this will be a worry; not sure what dbWriteTable can fail
    ## on.  One option would be a network failure between
    ## report_db_init and this point which is extremely unlikely.
    stop("Failed to add entry to database [orderly bug]") # nocov
  }
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
    stop("Already been copied?")
  }
  dir_create(parent)
  orderly_log("copy", "")
  ok <- file.copy(workdir, parent, recursive = TRUE)
  if (!ok) {
    try(unlink(dest, recursive = TRUE), silent = TRUE)
    stop("Error copying file")
  }
  dest
}

report_read_data <- function(workdir, config) {
  assert_is(config, "orderly_config")
  yml <- path_orderly_run_yml(workdir)
  if (!file.exists(yml)) {
    stop("Did not find run metadata file!")
  }
  ## This does *not* go through the read_recipe bits because we want
  ## the unmodified yml contents here.
  info <- modify_list(yaml_read(file.path(workdir, "orderly.yml")),
                      yaml_read(yml))
  if (info$id != basename(workdir)) {
    stop("Unexpected path") # should never happen
  }

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

  published_yml <- path_orderly_published_yml(workdir)
  if (file.exists(published_yml)) {
    published <- yaml_read(published_yml)$published
  } else {
    published <- FALSE
  }

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
    if (any(len > 1)) {
      ## TODO: Thus currently implies that all custom fields are
      ## scalar; non-scalar fields will require turning into json but
      ## we don't have support for that yet
      stop("FIXME [orderly bug]") # nocov
    }
    ret <- cbind(ret, as.data.frame(custom, stringsAsFactors = FALSE))
  }
  ret
}
