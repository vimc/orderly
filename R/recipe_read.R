## TODO: this whole thing needs thought: this is not really the
## *format* (which we can get from the extension) but an intent of
## use.
valid_formats <- function() {
  c("staticgraph", "interactivegraph", "data", "report", "interactivehtml")
}

string_or_filename <- function(x, path, name) {
  assert_scalar_character(x, name)
  if (grepl("\\.sql$", x)) {
    file <- x
    assert_file_exists(file, workdir = path, name = "SQL file")
    query <- read_lines(file.path(path, file))
  } else {
    file <- NULL
    query <- x
  }
  list(query = query, query_file = file)
}



recipe_read_skip_on_develop <- function(develop, expr) {
  if (develop) {
    tryCatch(expr, error = function(e) orderly_log("warning", e$message))
  } else {
    force(expr)
  }
}
