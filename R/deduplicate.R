##' Deduplicate an orderly archive.  Deduplicating an orderly archive
##' will replace all files that have the same content with "hard
##' links".  This is reliable only on Unix-like systems.  If you use
##' this feature, it is \emph{very important} that you treat your
##' orderly archive as read-only (though you should be anyway) as
##' changing one copy of a linked file changes all the other instances
##' of it - the files are literally the same file.
##'
##' @title Deduplicate an orderly archive
##'
##' @inheritParams orderly_list
##'
##' @param dry_run Logical, indicating if the deduplication
##'
##' @return Invisibly, information about the duplication status of the
##'   archive before deduplication was run.
##' @export
##' @examples
##'
##' path <- orderly::orderly_example("demo")
##' id1 <- orderly::orderly_run("minimal", root = path)
##' id2 <- orderly::orderly_run("minimal", root = path)
##' orderly_commit(id1, root = path)
##' orderly_commit(id2, root = path)
##' tryCatch(
##'   orderly::orderly_deduplicate(path, dry_run = TRUE),
##'   error = function(e) NULL)
orderly_deduplicate <- function(root = NULL, locate = TRUE, dry_run = TRUE, quiet = FALSE) {
  config <- orderly_config_get(root, locate)
  info <- orderly_deduplicate_info(config)
  if (!quiet) {
    print(info)
  }
  plan <- orderly_deduplicate_prepare(info)
  if (!dry_run) {
    orderly_deduplicate_run(plan)
  }
  invisible(info)
}


orderly_deduplicate_info <- function(config) {
  con <- orderly_db("destination", config)
  on.exit(DBI::dbDisconnect(con))

  sql_file_info <- read_lines(orderly_file("database/file_info.sql"))
  files <- DBI::dbGetQuery(con, sql_file_info)
  files$path <- file.path(files$report,
                          files$report_version,
                          files$filename,
                          fsep = "/")

  paths <- file.path(config$root, "archive", files$path)
  ## Information about the physical files, so we can work out which
  ## files are already hardlinked
  files <- cbind(
    files,
    fs::file_info(paths)[c("device_id", "inode", "hard_links")])
  ## Information about if the files are unchanged
  ## TODO: this step is slow for real files unfortunately
  files$unchanged <- hash_files(paths, FALSE) == files$hash

  ## Identify which files are duplicated
  files$duplicated <-
    files$hash %in% files$hash[duplicated(files$hash)]

  ## Identify the first inode (in report id order)
  i <- tapply(files$hash, files$hash)
  files$inode_first <-
    unname(tapply(files$inode, files$hash, function(x) x[[1L]]))[i]

  ## Quick check:
  stopifnot(all(vlapply(split(files, files$hash), function(x)
    all(x$inode_first == x$inode[[1]]))))

  ## Classify the files into different states
  files$state <- rep("distinct", nrow(files))
  files$state[files$duplicated] <- "duplicated"
  files$state[files$duplicated & files$inode == files$inode_first] <- "linked"
  files$state[files$duplicated & !duplicated(files$hash)] <- "distinct"

  ## There is a corner case where if an input is an output (seen in
  ## interactive in the demo) then we get into a right tangle.  So we
  ## exclude the entry here for the artefact version of this.
  files <- files[!duplicated(files$path), , drop = FALSE]
  rownames(files) <- NULL

  path_archive <- file.path(config$root, "archive")
  all_files <- dir(path_archive, recursive = TRUE, all.files = TRUE,
                   no.. = TRUE)
  untracked <- setdiff(all_files, files$path)

  exclude <- c("orderly_published.yml", ".orderly_archive_version", "README.md")
  internal <- grepl("^orderly_run.*\\.(rds|yml)$", basename(untracked)) |
    basename(untracked) %in% exclude

  untracked <- data_frame(
    path = untracked,
    size = file_size(file.path(path_archive, untracked)),
    internal = internal)

  ret <- list(config = config,
              files = files,
              path = path_archive,
              untracked = untracked)
  class(ret) <- "orderly_deduplicate_info"
  ret
}


orderly_deduplicate_prepare <- function(x) {
  if (is_windows()) {
    stop("Deduplication is not supported on windows")
  }

  assert_is(x, "orderly_deduplicate_info")

  ## This is the only safe thing to do here:
  if (length(unique(x$files$device_id)) > 1L) {
    stop("Can't deduplicate as your orderly archive spans multiple devices")
  }

  ## Likely too strict as we can just skip these
  if (!all(x$files$unchanged)) {
    stop("Can't deduplicate files that have been modified")
  }

  ## Possibly too strict:
  i <- x$files$state == "duplicated"
  if (any(x$files$hard_links[i] > 1)) {
    stop("Can't deduplicate files that have been hardlinked elsewhere")
  }

  from <- file.path(x$path, x$files$path[i])
  to <- file.path(x$path,
                  x$files$path[match(x$files$inode_first[i], x$files$inode)])
  data_frame(from = from, to = to)
}


orderly_deduplicate_run <- function(x) {
  for (i in seq_len(nrow(x))) {
    relink(x$from[i], x$to[i])
  }
}


##' @export
format.orderly_deduplicate_info <- function(x, ...) {
  dup <- x$files[x$files$state == "duplicated", ]
  linked <- x$files[x$files$state == "linked", ]
  untracked <- x$untracked[!x$untracked$internal, ]
  c("Deduplication information for",
    sprintf("  %s/archive", x$config$root),
    sprintf("  - %d tracked files", nrow(x$files)),
    sprintf("  - %s total size", pretty_bytes(sum(x$files$size))),
    sprintf("  - %d duplicate files", nrow(dup)),
    sprintf("  - %s duplicated size", pretty_bytes(sum(dup$size))),
    sprintf("  - %d deduplicated files", nrow(linked)),
    sprintf("  - %s deduplicated size", pretty_bytes(sum(linked$size))),
    sprintf("  - %s untracked files", nrow(untracked)),
    sprintf("  - %s untracked size", pretty_bytes(sum(untracked$size))))
}


##' @export
print.orderly_deduplicate_info <- function(x, ...) {
  cat(paste0(format(x), "\n", collapse = ""))
  invisible(x)
}


relink <- function(from, to) {
  backup <- paste0(from, ".bak")
  fs::file_move(from, backup)
  withCallingHandlers(
    fs::link_create(to, from),
    error = function(e) fs::file_move(backup, from))
  fs::file_delete(backup)
}
