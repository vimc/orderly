other_change_script <- function() {
  txt <- readLines("src/other/script.R")
  writeLines(c("extract$number <- extract$number * 1.2", txt),
             "src/other/script.R")
}

update_db <- function() {
  con <- orderly::orderly_db("source")
  on.exit(DBI::dbDisconnect(con))
  orderly:::fake_db(con)
}
