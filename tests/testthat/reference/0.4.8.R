patch_yml <- function(...) {
  txt <- readLines("src/depend/orderly.yml")
  writeLines(txt[!grepl("draft: true", txt)],
             "src/depend/orderly.yml")
}


pin_depends <- function(n = 0) {
  txt <- readLines("src/depend/orderly.yml")
  ids <- sort(dir("archive/example"))
  txt <- sub("latest$", ids[[length(ids) - n]], txt)
  writeLines(txt, "src/depend/orderly.yml")
}


unpin_depends <- function() {
  txt <- readLines("src/depend/orderly.yml")
  i <- grepl("depends:", txt)
  txt[i] <- sub(": .+", ": latest", txt[i])
  writeLines(txt, "src/depend/orderly.yml")
}
