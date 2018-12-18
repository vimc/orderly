##' Unzip an orderly archive into place.  This exists to work around
##' an alarming number of failure modes that
##' \code{\link{utils::unzip}} can suffer.  This should rarely be
##' needed to be used from user code and exists to support remotes.
##'
##' @title Unzip orderly archive into place
##' @param zip Path to a zip file
##' @param root Orderly root
##' @param name Name of the expected report
##' @param id Id of the expected report
##' @export
unzip_archive <- function(zip, root, name, id) {
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE))
  res <- utils::unzip(zip, exdir = tmp)

  files <- dir(tmp, all.files = TRUE, no.. = TRUE)
  if (length(files) == 0L) {
    stop("Corrupt zip file? No files extracted")
  } else if (length(files) > 1L) {
    stop("Invalid orderly archive", call. = FALSE)
  }
  if (files != id) {
    stop(sprintf("This is archive '%s' but expected '%s'",
                 files, id), call. = FALSE)
  }

  expected <- c("orderly.yml", "orderly_run.yml", "orderly_run.rds")
  msg <- !file.exists(file.path(tmp, id, expected))
  if (any(msg)) {
    stop(sprintf("Invalid orderly archive: missing files %s",
                 paste(expected[msg], collapse = ", ")), call. = FALSE)
  }

  ## R's file.copy is exceedingly rubbish
  dest <- file.path(root, "archive", name)
  dir.create(dest, FALSE, TRUE)
  file_copy(file.path(tmp, id), dest, recursive = TRUE)
}
