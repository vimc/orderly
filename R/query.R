VERSION_ID_RE <- "^([0-9]{8}-[0-9]{6})-([[:xdigit:]]{4})([[:xdigit:]]{4})$"

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
##'   directory if \code{locate} is \code{TRUE}).
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
##' @examples
##' # The orderly demo, with lots of potential reports:
##' path <- orderly::orderly_example("demo")
##'
##' # Reports that _could_ be run:
##' orderly::orderly_list(path)
orderly_list <- function(root = NULL, locate = TRUE) {
  config <- orderly_config_get(root, locate)
  basename(list_dirs(path_src(config$root)))
}

##' List draft and archived reports.  This returns a data.frame with
##' columns \code{name} (see \code{\link{orderly_list}}) and \code{id}.
##'
##' @title List draft and archived reports
##'
##' @inheritParams orderly_list
##'
##' @seealso \code{\link{orderly_list}}, which lists the names of
##'   source reports that can be run, and \code{\link{orderly_latest}}
##'   which returns the id of the most recent report.
##'
##' @export
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
orderly_list_drafts <- function(root = NULL, locate = TRUE) {
  orderly_list2(TRUE, root, locate)
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
##' @param draft Find most recent \emph{draft} report
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
  config <- orderly_config_get(root, locate)

  if (is.null(name)) {
    d <- orderly_list2(draft, config, FALSE)
    ids <- d$id
    path <-
      file.path((if (draft) path_draft else path_archive)(config$root), d$name)
  } else {
    path <-
      file.path((if (draft) path_draft else path_archive)(config$root), name)
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


orderly_list2 <- function(draft, root = NULL, locate = TRUE) {
  config <- orderly_config_get(root, locate)
  path <- if (draft) path_draft else path_archive
  check <- list_dirs(path(config$root))
  res <- lapply(check, orderly_list_dir)
  data.frame(name = rep(basename(check), lengths(res)),
             id = as.character(unlist(res)),
             stringsAsFactors = FALSE)
}

orderly_find_name <- function(id, config, locate = FALSE, draft = TRUE,
                              must_work = FALSE) {
  config <- orderly_config_get(config, locate)
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

orderly_find_report <- function(id, name, config, locate = FALSE,
                                draft = TRUE, must_work = FALSE) {
  config <- orderly_config_get(config, locate)
  path <-
    file.path((if (draft) path_draft else path_archive)(config$root), name)
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
