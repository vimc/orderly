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

add_changelog <- function() {
  path <- "src/changelog/changelog.txt"
  txt <- readLines(path)
  new <- c(
    "[public]",
    "Do you see any Teletubbies in here? Do you see a slender plastic tag",
    "clipped to my shirt with my name printed on it? Do you see a little",
    "Asian child with a blank expression on his face sitting outside on a",
    "mechanical helicopter that shakes when you put quarters in it? No?",
    "Well, that's what you see at a toy store. And you must think you're in",
    "a toy store, because you're here shopping for an infant named Jeb.")
  writeLines(c(new, txt), path)
}
