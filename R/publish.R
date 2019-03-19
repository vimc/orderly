##' Publish a report
##' @title Publish a report
##'
##' @param id The id of the report to publish
##'
##' @param value The value to set the published to; either \code{TRUE}
##'   or \code{FALSE}.
##'
##' @param name Optional report name.  If not given, then the name
##'   will be determined from the \code{id}
##'
##' @inheritParams orderly_list
##' @export
orderly_publish <- function(id, value = TRUE, name = NULL,
                            config = NULL, locate = TRUE) {
  config <- orderly_config_get(config, locate)
  if (is.null(name)) {
    name <- orderly_find_name(id, config, draft = FALSE, must_work = TRUE)
  }
  workdir <- file.path(path_archive(config$path), name, id)
  yml <- path_orderly_published_yml(workdir)

  if (file.exists(yml)) {
    data <- yaml_read(yml)
    if (identical(data$published, value)) {
      message("Report is already ", if (value) "published" else "unpublished")
      return(invisible(NULL))
    }
  } else {
    data <- list()
  }
  data$published <- value
  orderly_log(if (value) "publish" else "unpublish", id)
  yaml_write(data, yml)

  con <- orderly_db("destination", config, FALSE)
  on.exit(DBI::dbDisconnect(con))
  report_db2_publish(con, id, name, value)

  invisible(NULL)
}
