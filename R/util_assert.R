assert_scalar <- function(x, name = deparse(substitute(x))) {
  if (length(x) != 1) {
    stop(sprintf("'%s' must be a scalar", name), call. = FALSE)
  }
}

assert_character <- function(x, name = deparse(substitute(x))) {
  if (!is.character(x)) {
    stop(sprintf("'%s' must be character", name), call. = FALSE)
  }
}

assert_numeric <- function(x, name = deparse(substitute(x))) {
  if (!is.numeric(x)) {
    stop(sprintf("'%s' must be numeric", name), call. = FALSE)
  }
}

assert_logical <- function(x, name = deparse(substitute(x))) {
  if (!is.logical(x)) {
    stop(sprintf("'%s' must be logical", name), call. = FALSE)
  }
}

assert_hash <- function(x, name = deparse(substitute(x))) {
  if (!all(grepl("^[[:xdigit:]]{32}$", x))) {
    stop(sprintf("'%s' must be a hash", name), call. = FALSE)
  }
}

assert_scalar_logical <- function(x, name = deparse(substitute(x))) {
  assert_scalar(x, name)
  assert_logical(x, name)
}

assert_scalar_numeric <- function(x, name = deparse(substitute(x))) {
  assert_scalar(x, name)
  assert_numeric(x, name)
}

assert_scalar_character <- function(x, name = deparse(substitute(x))) {
  assert_scalar(x, name)
  assert_character(x, name)
}

assert_named <- function(x, unique = FALSE, name = deparse(substitute(x))) {
  if (is.null(names(x))) {
    stop(sprintf("'%s' must be named", name), call. = FALSE)
  }
  if (unique && any(duplicated(names(x)))) {
    stop(sprintf("'%s' must have unique names", name), call. = FALSE)
  }
}

assert_is <- function(x, what, name = deparse(substitute(x))) {
  if (!inherits(x, what)) {
    stop(sprintf("'%s' must be a %s", name,
                 paste(what, collapse = " / ")), call. = FALSE)
  }
}

assert_file_exists <- function(x, check_case = TRUE, workdir = NULL,
                               name = "File") {
  err <- !file_exists(x, check_case = check_case, workdir = workdir)
  if (any(err)) {
    if (check_case) {
      i <- attr(err, "incorrect_case")
      msg_case <- x[i]
      msg_totally <- x[err & !i]

      if (length(msg_case) > 0L) {
        msg_case_correct <- vcapply(msg_case, file_canonical_case)
        msg_case <- sprintf("'%s' (should be '%s')", msg_case, msg_case_correct)
      }
      msg <- c(msg_case, squote(msg_totally))
    } else {
      msg <- squote(x[err])
    }
    stop(sprintf("%s does not exist: %s", name, paste(msg, collapse = ", ")),
         call. = FALSE)
  }
}

assert_is_directory <- function(x, check_case = TRUE, workdir = NULL,
                                name = "File") {
  assert_file_exists(x, check_case, workdir, name)
  path <- if (is.null(workdir)) x else file.path(workdir, x)
  if (!is_directory(path)) {
    stop(sprintf("%s exists but is not a directory: %s",
                 name, paste(x, collapse = ", ")),
         call. = FALSE)
  }
}

match_value <- function(arg, choices, name = deparse(substitute(arg))) {
  assert_scalar_character(arg)
  if (!(arg %in% choices)) {
    stop(sprintf("%s must be one of %s",
                 name, paste(squote(choices), collapse = ", ")))
  }
  arg
}

assert_type <- function(x, type, name = deparse(substitute(x))) {
  switch(type,
         logical = assert_scalar_logical(x, name),
         numeric = assert_scalar_numeric(x, name),
         character = assert_scalar_character(x, name))
}
