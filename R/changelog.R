changelog_load <- function(name, id, changelog, message, config) {
  if (!is.null(message)) {
    changelog <- rbind(
      changelog_message_parse(message),
      changelog)
  }
  if (!is.null(changelog) && is.null(config$changelog)) {
    stop(sprintf("report '%s' uses changelog, ", name),
         "but this is not enabled in orderly_config.yml",
         call. = FALSE)
  }
  unk <- setdiff(changelog$label, config$changelog$id)
  if (length(unk)) {
    stop(sprintf("Unknown changelog %s: %s. Use one of %s",
                 ngettext(length(unk), "label", "labels"),
                 paste(squote(unk), collapse = ", "),
                 paste(squote(config$changelog$id), collapse = ", ")),
         call. = FALSE)
  }
  prev <- changelog_read_previous(name, config)
  changelog_update(id, changelog, prev)
}


changelog_compare <- function(new, old) {
  old <- old[old$from_file, ]
  old_str <- paste(old$label, old$value, sep = "\r")
  new_str <- paste(new$label, new$value, sep = "\r")

  i <- which(!new_str %in% old_str)
  msg <- old_str %in% new_str

  if (!all(msg)) {
    str <- paste(sprintf("[%s]: %s", old$label[!msg],
                         abbreviate(old$value[!msg])),
                 collapse = "\n")
    stop("Missing previously existing changelog entries:\n", str,
         call. = FALSE)
  }

  add <- i[i > length(i)]
  if (length(add) > 0L) {
    str <- paste(sprintf("[%s]: %s", new$label[add],
                         abbreviate(new$value[add])),
                 collapse = "\n")
    stop("Invalidly added historical changelog entries:\n", str,
         call. = FALSE)
  }

  ret <- new[i, , drop = FALSE]
  if (length(i) > 0L) {
    ret <- cbind(id = ids::random_id(length(i)), ret,
                 stringsAsFactors = FALSE)
  }
  ret
}


changelog_read_previous <- function(name, config) {
  ## This behaviour needs to _optionally_ call out to use the API so
  ## that we can compare against a remote version.  The switch for
  ## that is going to end up in the configuration.
  prev <- orderly_latest(name, config, locate = FALSE,
                         draft = FALSE, must_work = FALSE)
  if (is.na(prev)) {
    return(NULL)
  }
  path <- file.path(config$root, "archive", name, prev)
  readRDS(path_orderly_run_rds(path))$meta$changelog
}


changelog_update <- function(id, new, old) {
  if (is.null(new) && is.null(old)) {
    return(NULL)
  }

  new <- changelog_compare(new, old)
  if (!is.null(new) && nrow(new) > 0L) {
    new$report_version <- id
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
  list(filename = basename(filename),
       contents = changelog_parse(readLines(filename)))
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
  value <- Map(function(i, j) trimws(paste(txt[i:j], collapse = "\n")),
               i + 1L, j)

  ## The 'from_file' label here is to distinguish between file based
  ## changelog entries and ones that come from elsewhere (like the
  ## message).
  data_frame(label = label,
             value = list_to_character(value),
             from_file = TRUE)
}


changelog_message_parse <- function(txt) {
  re <- "^\\[(.+?)\\]\\s+(.+)$"
  i <- grepl(re, txt)
  if (any(!i)) {
    stop("message must be of the form '[<label>] <message>' failed on:\n",
         paste(squote(txt[!i]), collapse = "\n"),
         call. = FALSE)
  }
  label <- trimws(sub(re, "\\1", txt))
  value <- trimws(sub(re, "\\2", txt))
  data_frame(label = label,
             value = value,
             from_file = FALSE)
}
