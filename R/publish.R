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
  yaml_write(data, yml)

  con <- orderly_db("destination", config, FALSE)
  on.exit(DBI::dbDisconnect(con))
  orderly_table <- "orderly"
  sql <- sprintf("UPDATE %s SET published = $1 WHERE id = $2", orderly_table)
  DBI::dbExecute(con, sql, list(value, id))
  invisible(NULL)
}
