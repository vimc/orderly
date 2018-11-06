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
  tryCatch(yaml_load(read_lines(filename, warn = FALSE)),
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


viapply <- function(X, FUN, ...) {
  vapply(X, FUN, integer(1), ...)
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

hash_directory <- function(filenames, named = TRUE) {
  f <- function(p) {
    hash_object(hash_files(list_all_files(p), FALSE))
  }
  res <- vcapply(filenames, f)
  names(res) <- if (named) filenames else NULL
  res
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

to_json_string_charvec <- function(x) {
  to_json_string(x %||% character(0), auto_unbox = FALSE)
}

list_dirs <- function(path) {
  files <- dir(path, full.names = TRUE)
  files[file.info(files, extra_cols = FALSE)$isdir]
}

file_copy <- function(..., overwrite = TRUE) {
  ok <- file.copy(..., overwrite = overwrite)
  if (any(!ok)) {
    stop("Error copying files")
  }
  ok
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

dquote <- function(x) {
  sprintf('"%s"', x)
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

orderly_env <- function() {
  env <- Sys.getenv()
  as.list(env[grepl("^ORDERLY_", names(env))])
}

session_info <- function(path = ".") {
  list(session_info = utils::sessionInfo(),
       time = Sys.time(),
       env = orderly_env(),
       git = git_info(path))
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

append_text <- function(filename, txt) {
  orig <- readLines(filename)
  writeLines(c(orig, txt), filename)
}

Sys_getenv <- function(x, error = TRUE, default = NULL) {
  v <- Sys.getenv(x, NA_character_)
  if (is.na(v)) {
    if (error) {
      stop(sprintf("Environment variable '%s' is not set", x))
    } else {
      v <- default
    }
  }
  v
}

val_to_bytes <- function(x, nbytes) {
  n <- round((x %% 1) * 256 ^ nbytes)
  paste(packBits(intToBits(n))[nbytes:1], collapse = "")
}

sort_c <- function(x) {
  withr::with_locale(c(LC_COLLATE = "C"), sort(x))
}

git_info_call <- function(root, args) {
  git <- Sys.which("git")
  if (nzchar(git)) {
    res <- suppressWarnings(system2(git, c("-C", root, args),
                                    stdout = TRUE, stderr = FALSE))
    if (system_success(res)) {
      return(res)
    }
  }
  NULL
}

git_info <- function(root) {
  sha <- git_info_call(root, c("rev-parse", "HEAD"))
  if (is.null(sha)) {
    return(NULL)
  }
  sha_short <- substr(sha, 1, 7)
  branch <- git_info_call(root, c("symbolic-ref", "--short", "HEAD"))

  status <- git_info_call(root, c("status", "--porcelain"))
  if (length(status) == 0L) {
    status <- NULL
  }

  git_url <- git_info_call(root, c("remote", "get-url", "origin"))
  if (!is.null(git_url)) {
    re <- "^git@github.com:"
    if (grepl(re, git_url)) {
      git_url <- sub("\\.git$", "", sub(re, "https://github.com/", git_url))
    } else if (!grepl("^https://github.com/", git_url)) {
      git_url <- NULL
    }
  }

  list(sha_short = sha_short, sha = sha, branch = branch, status = status,
       github_url = git_url)
}

system_success <- function(x) is.null(attr(x, "status", exact = TRUE))

indent <- function(x, n) {
  paste0(strrep(" ", n), strsplit(x, "\n", fixed = TRUE)[[1]])
}

resolve_driver_config <- function(args, config) {
  resolve_secrets(resolve_env(args), config)
}

resolve_env <- function(x, error = TRUE, default = NULL) {
  f <- function(x) {
    if (grepl("^\\$[0-9A-Z_]+$", x)) {
      Sys_getenv(substr(x, 2, nchar(x)), error = error, default = NULL)
    } else {
      x
    }
  }
  lapply(x, f)
}

is_windows <- function() {
  Sys.info()[["sysname"]] == "Windows"
}
is_linux <- function() {
  Sys.info()[["sysname"]] == "Linux"
}

open_directory <- function(path) {
  if (!isTRUE(is_directory(path))) {
    stop("Expected a directory")
  }
  sysname <- Sys.info()[["sysname"]]
  if (sysname == "Windows") {
    system2("cmd", c("/c", "start", "explorer", path))
  } else {
    cmd <- switch(sysname,
                  "Darwin" = "open",
                  "Linux" = "xdg-open",
                  stop("Unsupported system ", sysname))
    system2(cmd, path)
  }
}

## rename is atomic
writelines_atomic <- function(txt, path) {
  tmp <- paste0(path, ".orderly")
  writeLines(txt, tmp)
  file.rename(tmp, path)
}

data_frame <- function(...) {
  data.frame(..., stringsAsFactors = FALSE)
}

as_data_frame <- function(...) {
  as.data.frame(..., stringsAsFactors = FALSE)
}

readlines_if_exists <- function(path, missing = NULL) {
  if (file.exists(path)) {
    readLines(path)
  } else {
    missing
  }
}

system3 <- function(command, args) {
  res <- suppressWarnings(system2(command, args, stdout = TRUE, stderr = TRUE))
  code <- attr(res, "status") %||% 0
  attr(res, "status") <- NULL
  list(success = code == 0,
       code = code,
       output = res)
}

sys_which <- function(name) {
  path <- Sys.which(name)
  if (!nzchar(path)) {
    stop(sprintf("Did not find '%s'", name))
  }
  unname(path)
}

zip_dir <- function(path, dest = paste0(basename(path), ".zip")) {
  owd <- setwd(dirname(path))
  on.exit(setwd(owd))
  code <- utils::zip(dest, basename(path), extras = "-q")
  if (code != 0) {
    stop("error running zip")
  }
  normalizePath(dest)
}

file_exists <- function(..., check_case = FALSE, workdir = NULL) {
  files <- c(...)
  if (!is.null(workdir)) {
    assert_scalar_character(workdir)
    owd <- setwd(workdir)
    on.exit(setwd(owd))
  }
  exists <- file.exists(files)

  if (check_case) {
    incorrect_case <- logical(length(files))
    if (!is_linux()) {
      incorrect_case[exists] <-
        !vlapply(files[exists], file_has_canonical_case)
    }
    attr(exists, "incorrect_case") <- incorrect_case
    exists[incorrect_case] <- FALSE
  }

  exists
}

## These two have quite similar patterns
file_has_canonical_case <- function(filename) {
  path <- strsplit(filename, "[/\\\\]")[[1]]
  if (!nzchar(path[[1]])) {
    base <- "/"
    path <- path[-1L]
  } else if (grepl("^[A-Za-z]:", path[[1]])) {
    base <- paste0(path[[1L]], "/")
    path <- path[-1L]
  } else {
    base <- "."
  }

  for (p in path[nzchar(path)]) {
    if (p %in% dir(base, all.files = TRUE)) {
      base <- paste(base, p, sep = "/")
    } else {
      return(FALSE)
    }
  }
  TRUE
}

file_canonical_case <- function(filename) {
 path <- strsplit(tolower(filename), "[/\\\\]")[[1]]
 if (!nzchar(path[[1]])) {
   base <- "/"
   path <- path[-1]
 } else if (grepl("^[A-Za-z]:", path[[1]])) {
   base <- paste0(path[[1]], "/")
   path <- path[-1]
 } else {
   base <- "."
 }

 for (p in path[nzchar(path)]) {
   pos <- dir(base, all.files = TRUE)
   i <- match(p, tolower(pos))
   if (is.na(i)) {
     return(NA_character_)
   } else {
     base <- paste(base, pos[[i]], sep = "/")
   }
 }

 if (grepl("^\\./", base) && !grepl("^\\./", filename)) {
   base <- sub("^\\./", "", base)
 }
 base
}

copy_directory <- function(src, as) {
  files <- dir(src, all.files = TRUE, no.. = TRUE, full.names = TRUE)
  dir.create(as, FALSE, TRUE)
  res <- file.copy(files, as, recursive = TRUE)
  if (!all(res)) {
    stop("Error copying files")
  }
}

expand_directory_list <- function(files) {
  if (is.null(files)) {
    return(NULL)
  }
  i <- is_directory(files)
  extra <- unlist(lapply(files[i], list_all_files), use.names = FALSE)
  union(files[!i], extra)
}

list_all_files <- function(path) {
  sort_c(dir(path, recursive = TRUE, full.names = TRUE, all.files = TRUE,
             no.. = TRUE))
}


ordered_map_to_list <- function(x) {
  ## This should not happen, but this is what would happen if we had
  ## a corrupted ordered map.  I think that the yaml parrsers will
  ## fix that for us though.  See similar faff in
  ## recipe_read_check_artefacts.
  stopifnot(all(lengths(x) == 1L),
            vlapply(x, function(el) !is.null(names(el))))
  if (!all(lengths(x) == 1L)) {
    stop("I am confused here")
  }
  set_names(lapply(x, function(x) x[[1]]),
            vcapply(x, names))
}


drop_na <- function(x) {
  x[!is.na(x)]
}


drop_null <- function(x) {
  x[!vlapply(x, is.null)]
}


list_to_character <- function(x, named = TRUE) {
  vcapply(x, identity, USE.NAMES = named)
}


list_to_integer <- function(x, named = TRUE) {
  viapply(x, identity, USE.NAMES = named)
}


source_to_function <- function(filename, name, parent) {
  e <- new.env(parent = parent)
  sys.source(filename, e)
  ret <- e[[name]]
  assert_is(ret, "function", sprintf("'%s' within '%s'", name, filename))
  ret
}


get_version <- function(x, numeric = TRUE, default = "0.0.0") {
  v <- x %||% default
  if (numeric) numeric_version(v) else as.character(v)
}


abbreviate <- function(x, len = round(getOption("width", 80) * 0.8)) {
  x <- sub("\n.*", "", x)
  i <- nchar(x) > len
  if (any(i)) {
    x[i] <- paste0(substr(x[i], 1, len - 3L), "...")
  }
  x
}
