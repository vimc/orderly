VERSION_ID_RE <- "^([0-9]{8}-[0-9]{6})-([[:xdigit:]]{4})([[:xdigit:]]{4})$"

## This gives a list of the source report names known to the system.
## This will not include things that have been deleted in source but
## are present in the database, because I want this to be useful for
## getting targets that one can run.

##' List the \emph{names} of reports known to orderly.  These are the
##' \emph{source} names, not the results of running reports.
##'
##' @title List orderly reports
##'
##' @param config An orderly configuration, or the path to one (or
##'   \code{NULL} to locate one if \code{locate} is \code{TRUE}).
##'
##' @param locate Logical, indicating if the configuration should be
##'   searched for.  If \code{TRUE} and \code{config} is not given,
##'   then orderly looks in the working directory and up through its
##'   parents until it finds an \code{orderly_config.yml} file.
##'
##' @export
orderly_list <- function(config = NULL, locate = TRUE) {
  config <- orderly_config_get(config, locate)
  basename(list_dirs(path_src(config$path)))
}

##' List draft and archived reports.  This returns a data.frame with
##' columns \code{name} (see \code{\link{orderly_list}} and \code{id}.
##' It will expand in future.
##'
##' @title List draft and archived reports
##' @inheritParams orderly_list
##' @export
orderly_list_drafts <- function(config = NULL, locate = TRUE) {
  orderly_list2(TRUE, config, locate)
}

##' @export
##' @rdname orderly_list_drafts
orderly_list_archive <- function(config = NULL, locate = TRUE) {
  orderly_list2(FALSE, config, locate)
}

##' Find most recent version of an orderly report
##' @title Find most recent report
##'
##' @param name Name of the report to find; if \code{NULL} returns the
##'   most recent report across all names
##'
##' @param draft Find most recent \emph{draft} report
##'
##' @param must_work Throw an error if no report is found.  If FALSE,
##'   returns \code{NA_character_}.
##'
##' @inheritParams orderly_list
##' @export
orderly_latest <- function(name = NULL, config = NULL, locate = TRUE,
                           draft = FALSE, must_work = TRUE) {
  config <- orderly_config_get(config, locate)

  if (is.null(name)) {
    d <- orderly_list2(draft, config, FALSE)
    ids <- d$id
    path <-
      file.path((if (draft) path_draft else path_archive)(config$path), d$name)
  } else {
    path <-
      file.path((if (draft) path_draft else path_archive)(config$path), name)
    ids <- orderly_list_dir(path)
  }

  if (length(ids) == 0L) {
    if (must_work) {
      type <- if (draft) "draft" else "archive"
      name <- name %||% "any report"
      stop(sprintf("Did not find any %s reports for %s", type, name))
    } else {
      return(NA_character_)
    }
  }

  latest_id(ids)
}

##' Open the directory for a completed orderly report
##'
##' @title Open directory of completed report
##'
##' @param id The identifier of the report - can be \code{latest}, in
##'   which case \code{name} and \code{draft} must be specified
##'
##' @param name The name of the report.  Can be omitted if \code{id}
##'   is not \code{latest}
##'
##' @param draft Logical, indicating if a draft report should be
##'   found.  Practically only useful when \code{id = "latest"} but
##'   might be useful to ensure presence of a particular type of
##'   report.
##'
##' @inheritParams orderly_list
##'
##' @export
##' @author Rich FitzJohn
orderly_open <- function(id, name = NULL, config = NULL, locate = TRUE,
                         draft = NULL) {
  path <- orderly_locate(id, name, config, locate, draft, TRUE)
  open_directory(path)
}

##' @export
##' @rdname orderly_open
orderly_open_latest <- function(name = NULL, config = NULL, locate = TRUE,
                                draft = FALSE) {
  id <- orderly_latest(name, config, locate, draft, TRUE)
  path <- orderly_locate(id, name, config, locate, draft, TRUE)
  open_directory(path)
}

##' Find the last id that was run
##' @title Get id of last run report
##' @inheritParams orderly_list
##' @param draft Find draft reports?
##' @export
orderly_last_id <- function(config = NULL, locate = TRUE, draft = TRUE) {
  config <- orderly_config_get(config, locate)
  path <- if (draft) path_draft else path_archive
  check <- list_dirs(path(config$path))

  d <- orderly_list2(draft, config, FALSE)
  latest_id(d$id)
}

orderly_list2 <- function(draft, config = NULL, locate = TRUE) {
  config <- orderly_config_get(config, locate)
  path <- if (draft) path_draft else path_archive
  check <- list_dirs(path(config$path))
  res <- lapply(check, orderly_list_dir)
  data.frame(name = rep(basename(check), lengths(res)),
             id = as.character(unlist(res)),
             stringsAsFactors = FALSE)
}

orderly_find_name <- function(id, config, locate = FALSE, draft = TRUE,
                              must_work = FALSE) {
  config <- orderly_config_get(config, locate)
  path <- (if (draft) path_draft else path_archive)(config$path)
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

orderly_find_report <- function(id, name, config, locate = FALSE,
                                draft = TRUE, must_work = FALSE) {
  config <- orderly_config_get(config, locate)
  ## TODO: I don't think that the treatment of draft is OK here - we
  ## should allow reports to roll over into archive gracefully.
  path <-
    file.path((if (draft) path_draft else path_archive)(config$path), name)
  if (id == "latest") {
    id <- orderly_latest(name, config, FALSE,
                         draft = draft, must_work = must_work)
  }
  path_report <- file.path(path, id)
  if (!is.na(id) && file.exists(path_report)) {
    return(path_report)
  }
  if (must_work) {
    stop(sprintf("Did not find %s report %s:%s",
                 if (draft) "draft" else "archived", name, id))
  } else {
    NULL
  }
}

latest_id <- function(ids) {
  if (length(ids) == 0L) {
    return(NA_character_)
  }

  ids <- sort_c(unique(ids))

  err <- !grepl(VERSION_ID_RE, ids)
  if (any(err)) {
    stop(sprintf("Invalid report id: %s",
                 paste(squote(ids[err]), collapse = ", ")),
         call. = FALSE)
  }

  isodate <- sub(VERSION_ID_RE, "\\1", ids)
  ids <- ids[isodate == last(isodate)]

  if (length(ids) > 1L) {
    ms <- sub(VERSION_ID_RE, "\\2", ids)
    ids <- ids[ms == last(ms)]
  }

  ids
}

## This is annoyingly similar to orderly_find_report, but allows for
## draft and name to be NULL.  It's used only in tests and in the
## orderly_open function
orderly_locate <- function(id, name, config = NULL, locate = TRUE,
                           draft = NULL, must_work = TRUE) {
  config <- orderly_config_get(config, locate)
  if (id == "latest") {
    if (is.null(name)) {
      stop("name must be given for id = 'latest'")
    }
    if (is.null(draft)) {
      stop("draft must be given for id = 'latest'")
    }
    id <- orderly_latest(name, config, locate, draft, must_work)
  } else {
    if (is.null(draft)) {
      for (draft in c(FALSE, TRUE)) {
        name <- orderly_find_name(id, config, locate, draft, FALSE)
        if (!is.null(name)) {
          break
        }
      }
      if (is.null(name) && must_work) {
        stop(sprintf("Did not find report %s (draft or archive)", id))
      }
    } else {
      name <- orderly_find_name(id, config, locate, draft, must_work)
    }
  }
  if (is.null(id) || is.null(name)) {
    NULL
  } else {
    path <- (if (draft) path_draft else path_archive)(config$path)
    file.path(path, name, id)
  }
}


orderly_list_dir <- function(path) {
  files <- dir(path)
  err <- !grepl(VERSION_ID_RE, files)
  if (any(err)) {
    stop(sprintf("Unexpected files within orderly directory '%s': %s",
                 path, paste(squote(files[err]), collapse = ", ")),
         call. = FALSE)
  }
  files
}
