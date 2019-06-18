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

  expected <- c("orderly.yml", "orderly_run.rds")
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
