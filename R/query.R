version_id_re <- "^([0-9]{8}-[0-9]{6})-([[:xdigit:]]{4})([[:xdigit:]]{4})$"

## This gives a list of the source report names known to the system.
## This will not include things that have been deleted in source but
## are present in the database, because I want this to be useful for
## getting targets that one can run.

##' List the \emph{names} of reports known to orderly.  These are the
##' \emph{source} names, not the results of running reports.  Note
##' that if a report has been committed from a different branch it
##' will not appear here, as this is simply the set of reports in the
##' \code{src} directory that can be run.
##'
##' @title List orderly reports
##'
##' @param root The path to an orderly root directory, or \code{NULL}
##'   (the default) to search for one from the current working
##'   directory if \code{locate} is \code{TRUE}.
##'
##' @param locate Logical, indicating if the configuration should be
##'   searched for.  If \code{TRUE} and \code{config} is not given,
##'   then orderly looks in the working directory and up through its
##'   parents until it finds an \code{orderly_config.yml} file.
##'
##' @seealso \code{\link{orderly_list_archive}} and
##'   \code{\link{orderly_list_drafts}}, which list archived
##'   (committed) and draft reports and their versions.
##'
##' @export
##' @return A character vector of report names
##' @examples
##' # The orderly demo, with lots of potential reports:
##' path <- orderly::orderly_example("demo")
##'
##' # Reports that _could_ be run:
##' orderly::orderly_list(path)
orderly_list <- function(root = NULL, locate = TRUE) {
  config <- orderly_config(root, locate)
  basename(list_dirs(path_src(config$root)))
}

##' List draft and archived reports.  This returns a data.frame with
##' columns \code{name} (see \code{\link{orderly_list}}) and \code{id}.
##'
##' @title List draft and archived reports
##'
##' @inheritParams orderly_list
##'
##' @param include_failed Logical, indicating if failed drafts should
##'   be listed (only has an effect for \code{orderly_list_drafts} as
##'   no failed run should make it into the archive).  A failed report
##'   is one that lacks an \code{orderly_run.rds} file.
##'
##' @seealso \code{\link{orderly_list}}, which lists the names of
##'   source reports that can be run, and \code{\link{orderly_latest}}
##'   which returns the id of the most recent report.
##'
##' @export
##'
##' @return A \code{data.frame} with columns \code{name} and
##'   \code{id}, containing character vectors of report names and
##'   versions, respectively.
##'
##' @examples
##' # The orderly demo, with lots of potential reports:
##' path <- orderly::orderly_example("demo")
##'
##' # Reports that _could_ be run:
##' orderly::orderly_list(path)
##'
##' # Run a report twice:
##' id1 <- orderly::orderly_run("minimal", root = path)
##' id2 <- orderly::orderly_run("minimal", root = path)
##'
##' # We can see both drafts:
##' orderly::orderly_list_drafts(path)
##'
##' # Nothing is in the archive:
##' orderly::orderly_list_archive(path)
##'
##' # Commit a report:
##' orderly::orderly_commit(id2, root = path)
##'
##' # Only one draft now
##' orderly::orderly_list_drafts(path)
##'
##' # And the second report is in the archive:
##' orderly::orderly_list_archive(path)
orderly_list_drafts <- function(root = NULL, locate = TRUE,
                                include_failed = FALSE) {
  orderly_list2(TRUE, root, locate, include_failed)
}

##' @export
##' @rdname orderly_list_drafts
orderly_list_archive <- function(root = NULL, locate = TRUE) {
  orderly_list2(FALSE, root, locate)
}

##' Find most recent version of an orderly report.  The most recent
##' report is always the most recently run report that has been
##' committed (regardless of the order in which they were committed).
##'
##' @title Find most recent report
##'
##' @param name Name of the report to find; if \code{NULL} returns the
##'   most recent report across all names
##'
##' @param draft Should draft reports be used searched? Valid values
##'   are logical (\code{TRUE}, \code{FALSE}) or use the string
##'   \code{newer} to use draft reports where they are newer than
##'   archive reports. For consistency, \code{always} and \code{never}
##'   are equivalent to \code{TRUE} and \code{FALSE}, respectively.
##'
##' @param must_work Throw an error if no report is found.  If FALSE,
##'   returns \code{NA_character_}.
##'
##' @inheritParams orderly_list
##'
##' @seealso \code{\link{orderly_list}} and
##'   \code{\link{orderly_list_archive}} for listing report names and
##'   versions.
##'
##' @return A character string with the id of the most recent report
##'
##' @export
##' @examples
##' path <- orderly::orderly_example("minimal")
##' id1 <- orderly::orderly_run("example", root = path, echo = FALSE)
##' id2 <- orderly::orderly_run("example", root = path, echo = FALSE)
##'
##' # With no reports committed there is no latest report:
##' orderly::orderly_latest("example", root = path, must_work = FALSE)
##'
##' # Commit the first report and it will be reported as latest:
##' orderly::orderly_commit(id1, root = path)
##' orderly::orderly_latest("example", root = path)
##'
##' # Commit the second report and it will be reported as latest instead:
##' orderly::orderly_commit(id2, root = path)
##' orderly::orderly_latest("example", root = path)
orderly_latest <- function(name = NULL, root = NULL, locate = TRUE,
                           draft = FALSE, must_work = TRUE) {
  config <- orderly_config(root, locate)

  draft <- query_check_draft(draft)
  path_funcs <- switch(draft,
                       always = list(list_draft),
                       never = list(list_archive),
                       newer = list(list_draft, list_archive))
  what <- switch(draft,
                 always = "draft",
                 never = "archive",
                 newer = "draft or archive")

  ids <- character(0)
  for (func in path_funcs) {
    ids <- c(ids, func(name, config))
  }

  if (length(ids) == 0L) {
    if (must_work) {
      name <- name %||% "any report"
      stop(sprintf("Did not find any %s reports for %s", what, name))
    } else {
      return(NA_character_)
    }
  }

  latest_id(ids)
}

list_archive <- function(name, config) {
  if (is.null(name)) {
    d <- orderly_list2(FALSE, config, FALSE)
    d$id
  } else {
    path <- file.path(path_archive(config$root), name)
    orderly_list_dir(path, check_run_rds = FALSE)
  }
}

list_draft <- function(name, config) {
  if (is.null(name)) {
    d <- orderly_list2(TRUE, config, FALSE)
    d$id
  } else {
    path <- file.path(path_draft(config$root), name)
    orderly_list_dir(path, check_run_rds = TRUE)
  }
}


orderly_list2 <- function(draft, root = NULL, locate = TRUE,
                          include_failed = FALSE) {
  config <- orderly_config(root, locate)
  path <- if (draft) path_draft else path_archive
  check <- list_dirs(path(config$root))
  check_run_rds <- draft && !include_failed
  res <- lapply(check, orderly_list_dir, check_run_rds = check_run_rds)
  data.frame(name = rep(basename(check), lengths(res)),
             id = as.character(unlist(res)),
             stringsAsFactors = FALSE)
}

orderly_find_name <- function(id, config, locate = FALSE, draft = TRUE,
                              must_work = FALSE) {
  config <- orderly_config(config, locate)
  path <- (if (draft) path_draft else path_archive)(config$root)
  ## NOTE: listing draft/archive rather than using orderly_list here
  ## because it allows for the existance of an archived report that we
  ## don't have the source for (VIMC-1013 and a related bug when
  ## pulling dependencies).
  for (name in basename(list_dirs(path))) {
    if (file.exists(file.path(path, name, id))) {
      return(name)
    }
  }
  ## Error case:
  if (must_work) {
    stop(sprintf("Did not find %s report %s",
                 if (draft) "draft" else "archive", id))
  } else {
    NULL
  }
}

## This is only used in one place, and so we can be quite flexible in
## how it is used.  It's a bit of a horror show, tbh, given how simple
## what we want to achieve is.  This function is not an orderly API
## function, and is used only in dependency resolution, so we can
## update this later if needed.
orderly_find_report <- function(id, name, config, locate = FALSE,
                                draft = TRUE, must_work = FALSE) {
  config <- orderly_config(config, locate)

  draft <- query_check_draft(draft)
  search_draft <- draft != "never"
  search_archive <- draft != "always"
  what <- switch(draft,
                 always = "draft",
                 never = "archive",
                 newer = "draft or archive")

  base_archive <- file.path(path_archive(config$root), name)
  base_draft <- file.path(path_draft(config$root), name)

  if (id == "latest") {
    path <- NULL
    if (search_archive) {
      found <- orderly_latest(name, config, draft = FALSE, must_work = FALSE)
      if (!is.na(found)) {
        path <- c(path, set_names(file.path(base_archive, found), found))
      }
    }

    if (search_draft) {
      found <- orderly_latest(name, config, draft = TRUE, must_work = FALSE)
      if (!is.na(found)) {
        path <- c(path, set_names(file.path(base_draft, found), found))
      }
    }

    if (length(path) == 1L) {
      return(path[[1L]])
    } else if (length(path) > 1L) {
      return(path[[latest_id(names(path))]])
    }
  } else {
    if (search_archive) {
      path <- file.path(base_archive, id)
      if (file.exists(path)) {
        return(path)
      }
    }

    if (search_draft) {
      path <- file.path(base_draft, id)
      if (file.exists(path)) {
        return(path)
      }
    }
  }

  if (must_work) {
    stop(sprintf("Did not find %s report %s:%s", what, name, id))
  }

  NULL
}

latest_id <- function(ids) {
  if (length(ids) == 0L) {
    return(NA_character_)
  }

  ids <- sort_c(unique(ids))

  err <- !grepl(version_id_re, ids)
  if (any(err)) {
    stop(sprintf("Invalid report id: %s",
                 paste(squote(ids[err]), collapse = ", ")),
         call. = FALSE)
  }

  isodate <- sub(version_id_re, "\\1", ids)
  ids <- ids[isodate == last(isodate)]

  if (length(ids) > 1L) {
    ms <- sub(version_id_re, "\\2", ids)
    ids <- max(ids[ms == last(ms)])
  }

  ids
}


orderly_list_dir <- function(path, check_run_rds = FALSE) {
  files <- dir(path)
  err <- !grepl(version_id_re, files)
  if (any(err)) {
    stop(sprintf("Unexpected files within orderly directory '%s': %s",
                 path, paste(squote(files[err]), collapse = ", ")),
         call. = FALSE)
  }

  if (check_run_rds && length(files) > 0L) {
    keep <- file.exists(path_orderly_run_rds(file.path(path, files)))
    files <- files[keep]
  }

  files
}


id_is_query <- function(id) {
  grepl("^latest\\s*\\(", id)
}
