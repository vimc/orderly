legacy_orderly_publish <- function(name, id, value = TRUE, root = NULL) {
  config <- orderly_config_get(root)
  path <- file.path(config$root, "archive", name, id)
  yaml_write(list(published = value),
             file.path(path, "orderly_published.yml"))
  legacy_report_db_publish(name, id, value, config)
}


legacy_report_db_publish <- function(name, id, value, config) {
  con <- orderly_db("destination", config)
  on.exit(DBI::dbDisconnect(con))

  sql <- "UPDATE report_version SET published = $1 WHERE id = $2"
  DBI::dbExecute(con, sql, list(value, id))

  sql <- "SELECT id FROM report_version WHERE report = $1 and published"
  published <- DBI::dbGetQuery(con, sql, name)$id

  sql <- paste("SELECT",
               "  changelog.id, report_version, report_version_public,",
               "    published",
               "  FROM changelog",
               "  JOIN report_version",
               "    ON report_version.id = changelog.report_version",
               "  JOIN changelog_label",
               "    ON changelog_label.id = changelog.label",
               " WHERE report_version.report = $1",
               "   AND changelog_label.public",
               " ORDER BY report_version")
  dat <- DBI::dbGetQuery(con, sql, list(name))
  dat$published <- dat$published == 1

  p <- rep(NA_character_, nrow(dat))
  for (i in seq_len(nrow(dat))) {
    j <- dat$report_version[[i]] <= published
    if (!any(j)) {
      break
    }
    p[[i]] <- published[[min(which(j))]]
  }

  prev <- dat$report_version_public
  new <- p
  ## Replace NAs with empty strings for ease of the next comparison
  prev[is.na(prev)] <- ""
  new[is.na(new)] <- ""

  sql <- "UPDATE changelog SET report_version_public = $1 WHERE id = $2"
  for (k in which(new != prev)) {
    DBI::dbExecute(con, sql, list(p[[k]], dat$id[[k]]))
  }
}


legacy_report_db_rebuild_published <- function(config) {
  assert_is(config, "orderly_config")
  root <- config$root
  con <- orderly_db("destination", config, validate = FALSE)
  on.exit(DBI::dbDisconnect(con))
  if (!is.null(config$changelog)) {
    reports <- orderly_list_archive(config)
    reports$yml <- file.path(root, "archive", reports$name, reports$id,
                             "orderly_published.yml")
    for (i in which(file.exists(reports$yml))) {
      name <- reports$name[[i]]
      id <- reports$id[[i]]
      value <- yaml_read(reports$yml[[i]])$published
      legacy_report_db_publish(name, id, value, config)
    }
  }
}
