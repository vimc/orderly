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

  data_frame(label = label, value = list_to_character(value))
}


changelog_read <- function(path) {
  ## Canonical spelling has a capital 'L'; see
  ## https://en.wikipedia.org/wiki/Changelog
  path_changelog <- file.path(path, "ChangeLog")
  if (!file_exists(path_changelog)) {
    return(NULL)
  }
  ## This takes care of the canonical casing for us:
  assert_file_exists("ChangeLog", workdir = path, check_case = TRUE)
  changelog_parse(readLines(path_changelog))
}


changelog_compare <- function(new, old) {
  old_str <- paste(old$label, old$value, sep = "\r")[old$from_file]
  new_str <- paste(new$label, new$value, sep = "\r")

  i <- which(!new_str %in% old_str)
  j <- seq_len(length(i))
  ok <- length(i) == length(j) && all(i == j)

  if (!ok) {
    stop("ChangeLog is not consistent with previous")
  }
}
