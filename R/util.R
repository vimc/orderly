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

to_json_string_charvec <- function(x) {
  to_json_string(x %||% character(0), auto_unbox = FALSE)
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

Sys_getenv <- function(x, default = NULL) {
  v <- Sys.getenv(x, default %||% NA_character_)
  if (is.na(v) && is.null(default)) {
    stop(sprintf("Environment variable '%s' is not set", x))
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

git_call <- function(root, args) {
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
  sha <- git_call(root, c("rev-parse", "HEAD"))
  if (is.null(sha)) {
    return(NULL)
  }
  sha_short <- substr(sha, 1, 7)
  branch <- git_call(root, c("symbolic-ref", "--short", "HEAD"))

  status <- git_call(root, c("status", "-s"))
  if (length(status) == 0L) {
    status <- NULL
  }
  list(sha_short = sha_short, sha = sha, branch = branch, status = status)
}

system_success <- function(x) is.null(attr(x, "status", exact = TRUE))

indent <- function(x, n) {
  paste0(strrep(" ", n), strsplit(x, "\n", fixed = TRUE)[[1]])
}

resolve_driver_config <- function(args) {
  resolve_secrets(resolve_env(args))
}

resolve_env <- function(x) {
  f <- function(x) {
    if (grepl("^\\$[0-9A-Z_]+$", x)) {
      Sys_getenv(substr(x, 2, nchar(x)))
    } else {
      x
    }
  }
  lapply(x, f)
}

resolve_secrets <- function(x) {
  re <- "^VAULT:(.+):(.+)"
  if (is.list(x)) {
    i <- vlapply(x, function(el) is.character(el) && grepl(re, el))
    if (any(i)) {
      x[i] <- resolve_secrets(vcapply(x[i], identity))
    }
  } else {
    i <- grepl(re, x)
    if (any(i)) {
      key <- unname(sub(re, "\\1", x[i]))
      field <- unname(sub(re, "\\2", x[i]))

      x[i] <- unname(Map(vault_read, key, field))
    }
  }
  x
}

vault_connect <- function() {
  loadNamespace("vaultr")
  if (is.null(cache$vault)) {
    cache$vault <- vaultr::vault_client()
  } else if (is.null(cache$vault$token)) {
    cache$vault$auth(NULL)
  }
}

vault_read <- function(key, field) {
  vault_connect()
  cache$vault$read(key, field)
}
