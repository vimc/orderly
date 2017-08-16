IMAGE_EXTS <- c("png", "pdf", "jpeg", "jpg")
WATERMARK_LENGTH <- 32
WATERMARK_PREFIX_LENGTH <- 8

watermark_write <- function(filename, id) {
  ext <- tolower(tools::file_ext(filename))
  watermark <- sprintf("orderly:%s", id)

  if (ext == "rds") {
    watermark_write_rds(filename, watermark)
  } else if (ext == "csv") {
    watermark_write_csv(filename, watermark)
  } else if (ext %in% IMAGE_EXTS) {
    watermark_write_image(filename, watermark)
  } else {
    message(sprintf("Can't watermark files of type '%s' (%s)",
                    ext, filename))
  }
}

watermark_read <- function(filename, error = TRUE) {
  ext <- tolower(tools::file_ext(filename))

  if (ext == "rds") {
    watermark_read_rds(filename, error)
  } else if (ext == "csv") {
    watermark_read_csv(filename, error)
  } else if (ext %in% IMAGE_EXTS) {
    watermark_read_image(filename, error)
  } else {
    stop(sprintf("Can't watermark files of type '%s' (%s)",
                 ext, filename))
  }
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
  writeLines(c(watermark, dat), filename)
}

watermark_write_image <- function(filename, watermark) {
  img <- magick::image_read(filename)
  magick::image_write(img, filename, comment = watermark)
}

watermark_read_rds <- function(filename, error) {
  dat <- readBin(filename, raw(), file.size(filename))
  watermark_check(utils::tail(dat, WATERMARK_LENGTH), error)
}

watermark_read_csv <- function(filename, error) {
  watermark_check(readLines(filename, 1), error)
}

watermark_read_image <- function(filename, error) {
  img <- magick::image_read(filename)
  watermark_check(magick::image_comment(img), error)
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
