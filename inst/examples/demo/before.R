other_change_script <- function() {
  txt <- readLines("src/other/script.R")
  writeLines(c("extract$number <- extract$number * 1.2", txt),
             "src/other/script.R")
}

update_db <- function() {
  con <- orderly::orderly_db("source")
  on.exit(DBI::dbDisconnect(con$source))
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
    "a toy store, because you're here shopping for an infant named Jeb.",

    "[public]",
    "You think water moves fast? You should see ice. It moves like it has",
    "a mind. Like it knows it killed the world once and got a taste for",
    "murder. After the avalanche, it took us a week to climb out. Now, I",
    "don't know exactly when we turned on each other, but I know that",
    "seven of us survived the slide... and only five made it out. Now we",
    "took an oath, that I'm breaking now. We said we'd say it was the",
    "snow that killed the other two, but it wasn't. Nature is lethal but",
    "it doesn't hold a candle to man.")
  writeLines(c(new, "", txt), path)
}


write_spacy_filename <- function() {
  path <- "src/spaces/a resource with spaces.csv"
  writeLines(c("a,b", "1,2"), path)
}
