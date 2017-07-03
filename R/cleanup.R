orderly_cleanup <- function(name = NULL, config = NULL, locate = TRUE) {
  config <- orderly_config_get(config, locate)
  orderly_cleanup_drafts(config, name)
  orderly_cleanup_data(config)
}

orderly_cleanup_drafts <- function(config, name = NULL) {
  assert_is(config, "orderly_config")
  d <- orderly_list_drafts(config, FALSE)
  if (!is.null(name)) {
    assert_character(name)
    d <- d[d$name %in% name, , drop = FALSE]
  }
  p <- file.path(path_draft(config$path), d$name, d$id)
  if (length(p) > 0L) {
    orderly_log("clean", p)
    unlink(p, recursive = TRUE)
  }
}

orderly_cleanup_data <- function(config) {
  assert_is(config, "orderly_config")
  con <- orderly_db("destination", config, FALSE)
  data <- DBI::dbGetQuery(con, "SELECT hash_data FROM orderly")[[1]]
  used <- unique(unlist(lapply(data, jsonlite::fromJSON)))

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
