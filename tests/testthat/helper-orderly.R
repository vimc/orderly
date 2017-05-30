fake_db <- function(path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path)

  set.seed(1)

  id <- ids::adjective_animal(20)
  n <- 200

  d <- data.frame(id = seq_along(id),
                  name = id,
                  number = runif(length(id)))
  DBI::dbWriteTable(con, "thing", d, overwrite = TRUE)

  d <- data.frame(id = seq_len(n),
                  thing = sample(length(id), n, replace = TRUE),
                  value = rnorm(n),
                  stringsAsFactors = FALSE)
  DBI::dbWriteTable(con, "data", d, overwrite = TRUE)

  invisible(path)
}

## Via wikimedia:
MAGIC_PNG <- as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a))
