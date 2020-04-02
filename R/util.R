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

vcapply <- function(X, FUN, ...) { # nolint
  vapply(X, FUN, character(1), ...)
}


viapply <- function(X, FUN, ...) { # nolint
  vapply(X, FUN, integer(1), ...)
}


vlapply <- function(X, FUN, ...) { # nolint
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

`%||%` <- function(a, b) { # nolint
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

sql_str_sub <- function(s, data) {
  for (i in seq_along(s)) {
    s[[i]]$query <- DBI::sqlInterpolate(DBI::ANSI(), s[[i]]$query,
                                        .dots = data)
  }
  s
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
  h <- tools::md5sum(filenames)
  if (!named) {
    names(h) <- NULL
  }
  h
}

hash_object <- function(object) {
  digest::digest(object)
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


file_move <- function(from, to) {
  ok <- file.rename(from, to)
  if (any(!ok)) {
    stop("Error moving files")
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


capture_log <- function(expr, filename) {
  ## nolint start
  if (file.exists(filename)) {
    mode = "a"
  } else {
    mode = "w"
  }
  con <- file(filename, mode)
  sink(con, split = FALSE)
  on.exit({
    sink(NULL)
    close(con)
  })
  ## nolint end
  handle_message <- function(e) cat(e$message, file = stdout())
  suppressMessages(withCallingHandlers(force(expr), message = handle_message))
}

conditional_capture_log <- function(capture, filename, expr) {
  if (isTRUE(capture)) {
    capture_log(expr, filename)
  } else {
    force(expr)
  }
}

last <- function(x) {
  x[[length(x)]]
}


orderly_env <- function() {
  env <- Sys.getenv()
  nms <- names(env)
  i <- grepl("^ORDERLY_", nms) & !grepl("(TOKEN|PAT|PASS)", nms)
  as.list(env[i])
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

sys_getenv <- function(x, used_in, error = TRUE, default = NULL) {
  v <- Sys.getenv(x, NA_character_)
  if (is.na(v) || !nzchar(v)) {
    if (error) {
      reason <- if (!nzchar(v)) "empty" else "not set"
      stop(sprintf("Environment variable '%s' is %s\n\t(used in %s)",
                   x, reason, used_in), call. = FALSE)
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
  if (isTRUE(getOption("orderly.nogit", FALSE))) {
    return(NULL)
  }
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

  git_url <- git_clean_url(
    git_info_call(root, c("remote", "get-url", "origin")))

  list(sha_short = sha_short, sha = sha, branch = branch, status = status,
       github_url = git_url)
}


git_clean_url <- function(x) {
  if (!is.null(x)) {
    re <- "^git@github.com:"
    if (grepl(re, x)) {
      x <- sub("\\.git$", "", sub(re, "https://github.com/", x))
    } else if (!grepl("^https://github.com/", x)) {
      x <- NULL
    }
  }
  x
}

system_success <- function(x) is.null(attr(x, "status", exact = TRUE))

indent <- function(x, n) {
  paste0(strrep(" ", n), strsplit(x, "\n", fixed = TRUE)[[1]])
}

resolve_driver_config <- function(args, config, name = NULL) {
  resolve_secrets(resolve_env(args, name), config)
}

resolve_env <- function(x, used_in, error = TRUE, default = NULL) {
  f <- function(nm, x) {
    if (length(x) == 1L && is.character(x) && grepl("^\\$[0-9A-Z_]+$", x)) {
      sys_getenv(substr(x, 2, nchar(x)), sprintf("%s:%s", used_in, nm),
                 error = error, default = NULL)
    } else {
      x
    }
  }
  assert_named(x)
  Map(f, names(x), x)
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

  if (is_windows()) {
    cmd <- "cmd"
    args <- c("/c", "start", "explorer", path)
  } else {
    args <- path
    cmd <- if (is_linux()) "xdg-open" else "open"
  }

  system2(cmd, args)
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
    stop(sprintf("Did not find '%s'", name), call. = FALSE)
  }
  unname(path)
}

zip_dir <- function(path, dest = paste0(basename(path), ".zip")) {
  withr::with_dir(dirname(path), {
    zip::zipr(dest, basename(path))
    normalizePath(dest)
  })
}

file_exists <- function(..., check_case = FALSE, workdir = NULL,
                        force_case_check = FALSE) {
  files <- c(...)
  if (!is.null(workdir)) {
    assert_scalar_character(workdir)
    owd <- setwd(workdir) # nolint
    on.exit(setwd(owd)) # nolint
  }
  exists <- file.exists(files)

  if (check_case) {
    incorrect_case <- logical(length(files))
    if (!is_linux() || force_case_check) {
      incorrect_case[exists] <-
        !vlapply(files[exists], file_has_canonical_case)
      if (any(incorrect_case)) {
        correct <- vcapply(files[incorrect_case], file_canonical_case)
        names(correct) <- files[incorrect_case]
        attr(exists, "incorrect_case") <- incorrect_case
        attr(exists, "correct_case") <- correct
        exists[incorrect_case] <- FALSE
      }
    }
  }

  exists
}


file_split_base <- function(filename, lowercase = FALSE) {
  path <- strsplit(filename, "[/\\\\]")[[1L]]
  if (!nzchar(path[[1]])) {
    base <- "/"
    path <- path[-1L]
    absolute <- TRUE
  } else if (grepl("^[A-Za-z]:", path[[1]])) {
    base <- paste0(path[[1L]], "/")
    path <- path[-1L]
    absolute <- TRUE
  } else {
    base <- "."
    absolute <- FALSE
  }
  if (lowercase) {
    path <- tolower(path)
  }
  list(path = path[nzchar(path)], base = base, absolute = absolute)
}


file_has_canonical_case <- function(filename) {
  dat <- file_split_base(filename)
  base <- dat$base
  absolute <- dat$absolute

  for (p in dat$path) {
    if (p %in% dir(base, all.files = TRUE)) {
      base <- paste(base, p, sep = if (absolute) "" else "/")
      absolute <- FALSE
    } else {
      return(FALSE)
    }
  }
  TRUE
}

## This one here behaves differently on unix because we could have
## files called Foo and foo next to each other (but not on
## windows/mac)
file_canonical_case <- function(filename) {
  dat <- file_split_base(filename, TRUE)
  base <- dat$base
  path <- dat$path
  absolute <- dat$absolute

  for (p in dat$path) {
    pos <- dir(base, all.files = TRUE)
    i <- match(p, tolower(pos))
    if (is.na(i)) {
      return(NA_character_)
    } else {
      base <- paste(base, pos[[i]], sep = if (absolute) "" else "/")
      absolute <- FALSE
    }
  }

  if (grepl("^\\./", base) && !grepl("^\\./", filename)) {
    base <- sub("^\\./", "", base)
  }
  base
}

copy_directory <- function(src, as, rollback_on_error = FALSE) {
  assert_is_directory(src, FALSE)
  files <- dir(src, all.files = TRUE, no.. = TRUE, full.names = TRUE)
  if (rollback_on_error) {
    if (file.exists(as)) {
      stop("Destination cannot already exist")
    }
    on.exit(unlink(as, recursive = TRUE))
  }
  dir.create(as, FALSE, TRUE)
  res <- file.copy(files, as, recursive = TRUE)
  if (!all(res)) {
    stop("Error copying files")
  }
  if (rollback_on_error) {
    on.exit()
  }
}


ordered_map_to_list <- function(x) {
  ## This should not happen, but this is what would happen if we had
  ## a corrupted ordered map.  I think that the yaml parsers will
  ## fix that for us though.  See similar faff in
  ## recipe_read_check_artefacts.
  if (!all(lengths(x) == 1L)) {
    stop("Corrupt ordered map (this should never happen)")
  }
  stopifnot(vlapply(x, function(el) !is.null(names(el))))
  set_names(lapply(x, function(x) x[[1]]),
            vcapply(x, names))
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


list_to_logical <- function(x, named = TRUE) {
  vlapply(x, identity, USE.NAMES = named)
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

handle_missing_packages <- function(missing_packages, force = FALSE) {
  ## check if we are interactive and logging is active...
  if (show_question() || force) {
    install_missing_packages(missing_packages)
  } else {
    ## ...we're not in interactive environment so just print out the command
    stop_missing_packages(missing_packages)
  }
}

install_missing_packages <- function(missing_packages) {
  ## collapse vector to packages to string "c('pckg_1','pckg_2')"
  vector_packages <- sprintf("install.packages(c(%s))",
                             paste(squote(missing_packages), collapse = ", "))

  ## ...if so ask if Orderly should try to install the pacakges
  question <- "Should I try to install missing packages by running:"
  install_command <- sprintf("\n%s\n\n    %s", question, vector_packages)

  if (prompt_ask_yes_no(install_command)) {
    install_packages(missing_packages)
  } else {
    stop_missing_packages(missing_packages)
  }
}

stop_missing_packages <- function(missing_packages) {
  vector_packages <- sprintf("install.packages(c(%s))",
                             paste(squote(missing_packages), collapse = ", "))
  question <- "To install the missing packages run:"
  install_command <- sprintf("\n%s\n\n    %s", question, vector_packages)
  msg <- sprintf("Missing packages: %s\n%s",
                 paste(squote(missing_packages), collapse = ", "),
                 install_command)
  stop(msg)
}

prompt_ask_yes_no <- function(prompt) {
  utils::menu(c("no", "yes"), FALSE, title = prompt) == 2 # nocov
}

show_question <- function() {
  (interactive() && !isTRUE(getOption("orderly.nolog")))
}

install_packages <- function(missing_packages) {
  ## try to install missing packages...
  utils::install.packages(missing_packages, quiet = TRUE)
  ## ...then check that they have been sucessful
  msg <- setdiff(missing_packages, .packages(TRUE))
  if (length(msg) > 0) {
    stop(sprintf("Could not install these packages: %s",
                 paste(squote(msg), collapse = ", ")))
  }
}


flow_text <- function(x) {
  paste(strwrap(paste(x, collapse = " ")), collapse = "\n")
}


sqlite_backup <- function(src, dest) {
  if (file.exists(dest)) {
    file.rename(dest, paste0(dest, ".prev"))
  }

  dest_con <- DBI::dbConnect(RSQLite::SQLite(), dest)
  on.exit(DBI::dbDisconnect(dest_con))

  src_con <- DBI::dbConnect(RSQLite::SQLite(), src)
  on.exit(DBI::dbDisconnect(src_con), add = TRUE)

  RSQLite::sqliteCopyDatabase(src_con, dest_con)
  invisible(dest)
}


periodic <- function(fun, period) {
  fun <- match.fun(fun)
  force(period)
  env <- new.env(parent = emptyenv())
  env$last <- Sys.time()
  function() {
    now <- Sys.time()
    if (now > env$last + period) {
      fun()
      env$last <- now
    }
  }
}


protect <- function(fun) {
  fun <- match.fun(fun)
  function() {
    tryCatch(fun(), error = function(e) NULL)
  }
}


## Does not exist in older R (< 3.3.0 I think)
file_size <- function(path) {
  file.info(path, extra_cols = FALSE)$size
}


file_info <- function(path, workdir = NULL) {
  if (is.null(path)) {
    return(NULL)
  }
  if (!is.null(workdir)) {
    return(withr::with_dir(workdir, file_info(path)))
  }
  data_frame(filename = path,
             file_hash = hash_files(path, FALSE),
             file_size = file_size(path))
}


file_in_data <- function(...) {
  d <- list(...)
  n <- viapply(d, NROW, USE.NAMES = FALSE)
  ret <- cbind(file_purpose = rep(names(d), n),
               do.call("rbind", d),
               stringsAsFactors = FALSE)
  rownames(ret) <- NULL
  ret
}


pretty_bytes <- function(bytes) {
  unit <- c("", "k", "M", "G")
  exponent <- max(0, min(floor(log(bytes, 1000)), length(unit) - 1))
  sprintf("%s %sB", round(bytes / 1000^exponent, 2), unit[exponent + 1])
}


clean_path <- function(path) {
  gsub("\\", "/", path, fixed = TRUE)
}


random_seed <- function(envir = globalenv()) {
  envir$.Random.seed
}


same_path <- function(a, b) {
  normalizePath(a, "/", TRUE) == normalizePath(b, "/", TRUE)
}


## NOTE: this will not cope for things like a block string that runs
## mutiple line in any child element.  So we cannot use this to edit
## sections where that is possible.
yaml_block_info <- function(name, text) {
  ## TODO: Explicitly check for a null section - that could be
  ## replaced in a 3rd option.
  ##
  ## TODO: Better behaviour for the key: value pair where value could
  ## have been a list.
  re <- sprintf("^%s\\s*:", name)
  start <- grep(re, text)
  if (length(start) == 0L) {
    return(list(name = name, exists = FALSE, block = FALSE))
  }

  re <- sprintf("^%s\\s*:", name)
  if (length(start) > 1L) {
    stop("Failed to process yaml")
  }

  ## Find end of the block - that will be the next zero-indented line
  ## or the EOF:
  end <- grep("^[^#[:space:]]", text)
  end <- c(end[end > start], length(text) + 1L)[[1L]] - 1L

  if (start == end) {
    return(list(name = name, exists = TRUE, block = FALSE,
                start = start, end = start))
  }

  tmp <- text[start:end][-1L]
  tmp <- tmp[!grepl("^\\s*(#.*)?$", tmp)][[1L]]
  indent <- sub("[^[:space:]].*$", "", tmp)

  list(name = name, exists = TRUE, block = TRUE,
       start = start, end = end, indent = indent)
}


insert_into_file <- function(text, where, value, path, show, edit, prompt) {
  x <- filediff(text, where, value)

  if (length(x$changed) == 0L) {
    message(sprintf("No changes to make to '%s'", path))
    return(invisible(x))
  }

  if (show) {
    message(sprintf("Changes to '%s'", path))
    cat(format_filediff(x))
  }

  if (edit && prompt && !prompt_ask_yes_no("Write to file? ")) {
    edit <- FALSE
    message("Not modifying file")
  }

  if (edit) {
    message(sprintf("Writing to '%s'", path))
    writeLines(x$result, path)
  }

  invisible(x)
}


filediff <- function(text, where, value) {
  if (is.null(text)) {
    result <- value
    changed <- seq_along(value)
    create <- TRUE
  } else {
    i <- seq_len(where)
    result <- c(text[i], value, text[-i])
    changed <- seq_along(value) + where
    create <- FALSE
  }
  ret <- list(text = text,
              where = where,
              value = value,
              result = result,
              changed = changed,
              create = create)
  class(ret) <- "filediff"
  ret
}


format_filediff <- function(x, ..., context = 2L, colour = NULL) {
  colour <- colour %||% crayon::has_color()
  if (length(x$changed) == 0) {
    return(character(0))
  }
  i <- seq_along(x$result)
  focus <- range(x$changed)
  i <- i[i >= focus[[1L]] - context & i <= focus[[2L]] + context]
  line <- format(i)
  text <- x$result[i]
  changed <- i %in% x$changed
  grey <- crayon::make_style("grey")
  if (colour) {
    line[changed] <- crayon::bold(grey(line[changed]))
    text[changed] <- crayon::bold(text[changed])
  } else {
    line[changed] <- paste0("+ ", line[changed])
    line[!changed] <- paste0("  ", line[!changed])
  }
  paste(sprintf("%s | %s\n", line, text), collapse = "")
}


sys_setenv <- function(env) {
  if (length(env) > 0L) {
    do.call("Sys.setenv", as.list(env))
  }
}


orderly_style <- function(name) {
  switch(name,
         highlight = crayon::combine_styles(
           crayon::bold, crayon::make_style("steelblue3")),
         alert = crayon::combine_styles(
           crayon::bold, crayon::make_style("hotpink")),
         fade = crayon::make_style("grey"),
         identity)
}


is_call <- function(expr, valid) {
  is.recursive(expr) && as.character(expr[[1]]) %in% valid
}


deparse_str <- function(x) {
  paste(deparse(x), collapse = "\n")
}

df2list <- function(df) {
  unname(split(df, seq_len(nrow(df))))
}
