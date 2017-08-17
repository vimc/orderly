IMAGE_EXTS <- c("png", "pdf", "jpeg", "jpg")
WATERMARK_LENGTH <- 32
WATERMARK_PREFIX_LENGTH <- 8

watermark_write <- function(filename, id) {
  ext <- tolower(tools::file_ext(filename))
  watermark <- sprintf("orderly:%s", id)

  switch(ext,
         rds = watermark_write_rds(filename, watermark),
         csv = watermark_write_csv(filename, watermark),
         pdf = watermark_write_pdf(filename, watermark),
         png = watermark_write_png(filename, watermark),
         message(sprintf("Can't watermark files of type '%s' (%s)",
                         ext, filename)))
}

watermark_read <- function(filename, error = TRUE) {
  ext <- tolower(tools::file_ext(filename))
  switch(ext,
         rds = watermark_read_rds(filename, error),
         csv = watermark_read_csv(filename, error),
         pdf = watermark_read_pdf(filename, error),
         png = watermark_read_png(filename, error),
         stop(sprintf("Can't watermark files of type '%s' (%s)",
                      ext, filename)))
}

watermark_exists <- function(filename) {
  !is.na(watermark_read(filename, FALSE))
}

## This is a bit cheeky: it turns out that we can write junk to
## the end of an rds file, even when compressed, and it still
## loads OK.  I don't know that this will always hold, but it
## seems like a reasonable thing to try
watermark_write_rds <- function(filename, watermark) {
  dat <- readBin(filename, raw(), file.size(filename))
  writeBin(c(dat, charToRaw(watermark)), filename)
}

watermark_write_csv <- function(filename, watermark) {
  dat <- readLines(filename)
  writeLines(c(paste("#", watermark), dat), filename)
}

watermark_write_png <- function(filename, watermark) {
  exiftool_process_write_field(filename, "comment", watermark)
  invisible()
}

watermark_write_pdf <- function(filename, watermark) {
  exiftool_process_write_field(filename, "subject", watermark)
  invisible()
}

watermark_read_rds <- function(filename, error) {
  dat <- readBin(filename, raw(), file.size(filename))
  watermark_check(utils::tail(dat, WATERMARK_LENGTH), error)
}

watermark_read_csv <- function(filename, error) {
  x <- readLines(filename, 1)
  watermark_check(sub("# ", "", x), error)
}

watermark_read_png <- function(filename, error) {
  x <- exiftool_process_read_field(filename, "comment")
  watermark_check(x, error)
}

watermark_read_pdf <- function(filename, error) {
  x <- exiftool_process_read_field(filename, "subject")
  watermark_check(x, error)
}

watermark_check <- function(x, error) {
  if (is.character(x)) {
    if (nchar(x) == WATERMARK_LENGTH &&
        grepl("^orderly:", x)) {
      return(substr(x, 9, WATERMARK_LENGTH))
    }
  } else { # raw
    if (length(x) == WATERMARK_LENGTH &&
        all(x[seq_len(WATERMARK_PREFIX_LENGTH)] == charToRaw("orderly:"))) {
      return(rawToChar(x[-seq_len(WATERMARK_PREFIX_LENGTH)]))
    }
  }
  if (error) {
    stop("Did not find orderly watermark")
  } else {
    NA_character_
  }
}
