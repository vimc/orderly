##' Clean up orderly draft and data directories.  Deletes all drafts
##' (possibly just for a set of report names) and then deletes
##' dangling data sets that are not pointed to by any draft or
##' committed reports
##'
##' @title Orderly cleanup
##' @param name Optional name; in this case only clean up drafts with this name
##'
##' @param draft Remove drafts?
##'
##' @param data Remove dangling data (data not used by any draft or
##'   archived report).
##'
##' @param failed_only Delete only failed reports (those without the
##'   end-of-run metadata).  This will also clean up drafts created by
##'   \code{\link{orderly_test_start}}
##' @inheritParams orderly_list
##' @export
orderly_cleanup <- function(name = NULL, config = NULL, locate = TRUE,
                            draft = TRUE, data = TRUE, failed_only = FALSE) {
  config <- orderly_config_get(config, locate)
  if (draft) {
    orderly_cleanup_drafts(config, name, failed_only)
  }
  if (data) {
    orderly_cleanup_data(config)
  }
}

orderly_cleanup_drafts <- function(config, name = NULL, failed_only = FALSE) {
  assert_is(config, "orderly_config")
  d <- orderly_list_drafts(config, FALSE)
  if (!is.null(name)) {
    assert_character(name)
    d <- d[d$name %in% name, , drop = FALSE]
  }
  p <- file.path(path_draft(config$path), d$name, d$id)
  if (failed_only) {
    p <- p[!file.exists(path_orderly_run_yml(p))]
  }
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

  ## Determine all used data sets in *both* draft and published
  ## reports
  used_pub <- unique(data)
  dr <- orderly_list_drafts(config, FALSE)
  yml <- path_orderly_run_yml(
    file.path(path_draft(config$path), dr$name, dr$id))
  used_draft <-
    unlist(lapply(yml, function(x) yaml_read(x)$hash_data), use.names = FALSE)
  used <- c(used_pub, used_draft)

  csv <- orderly_db("csv", config, FALSE)
  drop_csv <- setdiff(csv$list(), used)
  if (length(drop_csv) > 0L) {
    csv$del(drop_csv)
  }

  rds <- orderly_db("rds", config, FALSE)
  drop_rds <- setdiff(rds$list(), used)
  if (length(drop_rds) > 0L) {
    rds$del(drop_rds)
  }
}
