##' Clean up orderly draft and data directories.  Deletes all drafts
##' (possibly just for a set of report names) and then deletes
##' dangling data sets that are not pointed to by any draft or
##' committed reports.  Running cleanup does not affect any reports
##' that have been committed with \code{\link{orderly_commit}} (i.e.,
##' the contents of the \code{archive/} directory).
##'
##' @title Orderly cleanup
##' @param name Optional name; in this case only clean up drafts with this name
##'
##' @param draft Logical, indicating if drafts should be removed
##'
##' @param data Logical, indicating if dangling data should be removed
##'   (data not used by any draft or archived report).
##'
##' @param failed_only Delete only failed reports (those without the
##'   end-of-run metadata).  This will also clean up drafts created by
##'   \code{\link{orderly_test_start}}
##' @inheritParams orderly_list
##' @return No return value, this function is called only for its side effects
##' @export
##' @examples
##' # In a new example orderly, run two reports and commit only the
##' # second one:
##' path <- orderly::orderly_example("minimal")
##' id1 <- orderly::orderly_run("example", root = path)
##' id2 <- orderly::orderly_run("example", root = path)
##' orderly::orderly_commit(id2, root = path)
##'
##' # We now have one draft and one archive report:
##' orderly::orderly_list_drafts(root = path)
##' orderly::orderly_list_archive(root = path)
##'
##' # To clean up the drafts:
##' orderly::orderly_cleanup(root = path)
##'
##' # We now have no draft and one archive reports:
##' orderly::orderly_list_drafts(root = path)
##' orderly::orderly_list_archive(root = path)
orderly_cleanup <- function(name = NULL, root = NULL, locate = TRUE,
                            draft = TRUE, data = TRUE, failed_only = FALSE) {
  config <- orderly_config(root, locate)
  if (draft) {
    orderly_cleanup_drafts(config, name, failed_only)
  }
  if (data) {
    orderly_cleanup_data(config)
  }
}

orderly_cleanup_drafts <- function(config, name = NULL, failed_only = FALSE) {
  assert_is(config, "orderly_config")

  d <- orderly_list_drafts(config, FALSE, include_failed = TRUE)
  if (!is.null(name)) {
    assert_character(name)
    d <- d[d$name %in% name, , drop = FALSE]
  }
  p <- file.path(path_draft(config$root), d$name, d$id)
  if (failed_only) {
    p <- p[!file.exists(path_orderly_run_rds(p))]
  }
  msg <- sprintf("Found %s draft %s", length(p),
                 ngettext(length(p), "report", "reports"))
  if (!is.null(name)) {
    msg <- paste(msg, sprintf("for report name '%s'", name))
  }
  orderly_log("clean", msg)
  if (length(p) > 0L) {
    orderly_log(if (failed_only) "prune" else "clean", p)
    unlink(p, recursive = TRUE)
  }
}

orderly_cleanup_data <- function(config) {
  assert_is(config, "orderly_config")
  con <- orderly_db("destination", config, FALSE)
  on.exit(DBI::dbDisconnect(con))
  data <- DBI::dbGetQuery(con, "SELECT hash from report_version_data")[[1]]

  ## Determine all used data sets in *both* draft and committed (archived)
  ## reports
  used_pub <- unique(data)
  dr <- orderly_list_drafts(config, FALSE)
  path_rds <- path_orderly_run_rds(
    file.path(path_draft(config$root), dr$name, dr$id))
  used_draft <-
    unlist(lapply(path_rds, function(x)
      readRDS(x)$meta$data$hash), use.names = FALSE)
  used <- c(used_pub, used_draft)

  csv <- orderly_db("csv", config, FALSE)
  drop_csv <- setdiff(csv$list(), used)
  msg <- sprintf("Found %s csv %s", length(drop_csv),
                 ngettext(length(drop_csv), "file", "files"))
  orderly_log("clean", msg)
  if (length(drop_csv) > 0L) {
    csv$del(drop_csv)
  }

  rds <- orderly_db("rds", config, FALSE)
  drop_rds <- setdiff(rds$list(), used)
  msg <- sprintf("Found %s rds %s", length(drop_rds),
                 ngettext(length(drop_rds), "file", "files"))
  orderly_log("clean", msg)
  if (length(drop_rds) > 0L) {
    rds$del(drop_rds)
  }
}
