yaml_load <- function(string) {
  ## More restrictive true/false handling.  Only accept if it maps to
  ## full (true|yes) / (false|no):
  handlers <- list(
    "bool#yes" = function(x)
      if (tolower(x) %in% c("true", "yes")) TRUE else x,
    "bool#no" = function(x)
      if (tolower(x) %in% c("false", "no")) FALSE else x)
  yaml::yaml.load(string, handlers = handlers)
}

yaml_read <- function(filename) {
  catch_yaml <- function(e) {
    stop(sprintf("while reading '%s'\n%s", filename, e$message),
         call. = FALSE)
  }
  tryCatch(yaml_load(read_lines(filename)),
           error = catch_yaml)
}

yaml_write <- function(data, filename) {
  writeLines(yaml::as.yaml(data), filename)
}

read_lines <- function(...) {
  paste(readLines(...), collapse = "\n")
}

vcapply <- function(X, FUN, ...) {
  vapply(X, FUN, character(1), ...)
}
vlapply <- function(X, FUN, ...) {
  vapply(X, FUN, logical(1), ...)
}

check_symbol_from_str <- function(str, name) {
  assert_scalar_character(str)
  dat <- strsplit(str, "::", fixed = TRUE)[[1L]]
  if (length(dat) != 2) {
    stop(sprintf("Expected fully qualified name for %s", name))
  }
  dat
}

## Originally in cyphr:
find_file_descend <- function(target, start = ".", limit = "/") {
  root <- normalizePath(limit, mustWork = TRUE)
  start <- normalizePath(start, mustWork = TRUE)

  f <- function(path) {
    if (file.exists(file.path(path, target))) {
      return(path)
    }
    if (normalizePath(path, mustWork = TRUE) == root) {
      return(NULL)
    }
    parent <- normalizePath(file.path(path, ".."))
    if (parent == path) {
      return(NULL)
    }
    Recall(parent)
  }
  ret <- f(start)
  if (!(is.null(ret))) {
    ret <- normalizePath(ret, mustWork = TRUE)
  }
  ret
}

orderly_file <- function(...) {
  system.file(..., package = "orderly", mustWork = TRUE)
}

`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}

check_fields <- function(x, name, required, optional) {
  msg <- setdiff(required, names(x))
  if (length(msg) > 0L) {
    stop(sprintf("Fields missing from %s: %s",
                 name, paste(msg, collapse = ", ")))
  }
  extra <- setdiff(names(x), c(required, optional))
  if (length(extra) > 0L) {
    stop(sprintf("Unknown fields in %s: %s",
                 name, paste(extra, collapse = ", ")))
  }
}

is_within_dir <- function(files, path = getwd()) {
  files <- normalizePath(files, mustWork = TRUE)
  path <- normalizePath(path, mustWork = TRUE)
  substr(files, 1, nchar(path)) == path
}

is_absolute_path <- function(path) {
  grepl("^(/|[A-Z][a-z]:)", path)
}

is_relative_path <- function(path) {
  !is_absolute_path(path)
}

sql_str_sub <- function(s, data, ...) {
  vcapply(s, function(el) DBI::sqlInterpolate(DBI::ANSI(), el, .dots = data),
          ...)
}

read_csv <- function(filename, ...) {
  utils::read.csv(filename, stringsAsFactors = FALSE)
}

write_csv <- function(data, filename, ...) {
  utils::write.csv(data, filename, ..., row.names = FALSE)
}

set_names <- function(x, nms) {
  names(x) <- nms
  x
}

dir_create <- function(x) {
  for (i in unique(x)) {
    dir.create(i, FALSE, TRUE)
  }

}

hash_files <- function(filenames, named = TRUE) {
  if (is.null(filenames)) {
    set_names(character(0), if (named) character(0) else NULL)
  } else {
    h <- tools::md5sum(filenames)
    if (!named) {
      names(h) <- NULL
    }
    h
  }
}

hash_object <- function(object) {
  digest::digest(object)
}

to_json <- function(x, auto_unbox = TRUE, ...) {
  jsonlite::toJSON(x, auto_unbox = auto_unbox)
}

to_json_string <- function(...) {
  as.character(to_json(...))
}

list_dirs <- function(path) {
  files <- dir(path, full.names = TRUE)
  files[file.info(files, extra_cols = FALSE)$isdir]
}

file_copy <- function(...) {
  ok <- file.copy(...)
  if (any(!ok)) {
    stop("Error copying files")
  }
}

is_directory <- function(x) {
  file.info(x, extra_cols = FALSE)$isdir
}

rbind_df <- function(x) {
  do.call("rbind", x)
}

squote <- function(x) {
  sprintf("'%s'", x)
}

pasteq <- function(x, sep = ", ") {
  paste(squote(x), collapse = ", ")
}

set_mode <- function(x, mode) {
  storage.mode(x) <- mode
  x
}

capture_log <- function(expr, filename, suppress_messages = FALSE) {
  con <- file(filename, "w")
  sink(con, split = FALSE)
  on.exit({
    sink(NULL)
    close(con)
  })
  handle_message <- function(e) cat(e$message, file = stdout())
  if (suppress_messages) {
    suppressMessages(withCallingHandlers(force(expr), message = handle_message))
  } else {
    withCallingHandlers(force(expr), message = handle_message)
  }
}

last <- function(x) {
  x[[length(x)]]
}

modify_list <- function(a, b) {
  a[names(b)] <- b
  a
}

session_info <- function() {
  dat <- utils::sessionInfo()
  dat$time <- Sys.time()
  dat
}

## Because time handling is a total faff:
which_max_time <- function(x) {
  idx <- 1L
  t <- x[[idx]]
  for (i in seq_along(x)[-1]) {
    ti <- x[[i]]
    if (ti > t) {
      t <- ti
      idx <- i
    }
  }
  idx
}
