changelog_load <- function(path, info, config) {
  changelog <- changelog_read(path)
  if (!is.null(info$message)) {
    changelog <- rbind(
      data_frame(label = "[message]",
                 value = info$message,
                 from_file = FALSE),
      changelog)
  }
  prev <- changelog_read_previous(info$name, config)
  changelog_update(info$id, changelog, prev)
}


changelog_compare <- function(new, old) {
  old_str <- paste(old$label, old$value, sep = "\r")[old$from_file]
  new_str <- paste(new$label, new$value, sep = "\r")

  i <- which(!new_str %in% old_str)
  j <- seq_len(length(i))
  ok <- length(i) == length(j) && all(i == j)

  if (!ok) {
    ## TODO: better errors here
    stop("changelog is not consistent with previous")
  }

  new[i, , drop = FALSE]
}


changelog_read_previous <- function(name, config) {
  ## This behaviour needs to _optionally_ call out to use the API so
  ## that we can compare against a remote version.  The switch for
  ## that is going to end up in the configuration.
  prev <- orderly_latest(name, config, locate = FALSE,
                         draft = FALSE, must_work = FALSE)
  changelog_read_json(file.path(config$path, "archive", name, prev))
}


changelog_update <- function(id, new, old) {
  if (is.null(new) && is.null(old)) {
    return(NULL)
  }

  new <- changelog_compare(new, old)
  if (nrow(new) > 0L) {
    new$id <- id
    ret <- rbind(new, old)
  } else {
    ret <- old
  }
  ret
}


changelog_read <- function(path) {
  filename <- path_changelog_txt(path)
  if (!file_exists(filename)) {
    return(NULL)
  }
  ## This takes care of the canonical casing for us, as people might
  ## be tempted to use something like ChangeLog.txt, as capital 'L' is
  ## canonical: https://en.wikipedia.org/wiki/Changelog
  assert_file_exists(basename(filename), workdir = path, check_case = TRUE)
  changelog_parse(readLines(filename))
}


changelog_parse <- function(txt) {
  if (length(txt) == 0L) {
    return(data_frame(label = character(0), value = character(0)))
  }
  re_header <- "^\\[(.+?)\\]\\s*$"
  i <- grep(re_header, txt)
  if (length(i) == 0L || i[[1L]] != 1L) {
    stop("Invalid changelog - first line is not a label", call. = FALSE)
  }
  label <- sub(re_header, "\\1", txt[i])

  n <- diff(c(i, length(txt) + 1L)) - 1L
  err <- i[n == 0L]
  if (length(err) == 1L) {
    stop("Invalid changelog - empty entry on line ", err)
  } else if (length(err) > 0L) {
    stop("Invalid changelog - empty entries on lines ",
         paste(err, collapse = ", "))
  }

  j <- c(i[-1] - 1, length(txt))
  value <- Map(function(i, j) paste(txt[i:j], collapse = "\n"), i + 1L, j)

  ## The 'from_file' label here is to distinguish between file based
  ## changelog entries and ones that come from elsewhere (like the
  ## message).
  data_frame(label = label,
             value = list_to_character(value),
             from_file = TRUE)
}


changelog_save_json <- function(dat, path) {
  if (!is.null(dat)) {
    writeLines(jsonlite::toJSON(dat), path_changelog_json(path))
  }
}


changelog_read_json <- function(path) {
  filename <- path_changelog_json(path)
  if (!file.exists(filename)) {
    return(NULL)
  }
  jsonlite::fromJSON(filename)
}
