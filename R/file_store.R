## this could go into storr actually:
##
## * use storr assertions
## * use storr hash
## * use storr errors
## * use storr mget semantics
## * check against remake use
file_store <- function(path, read, write, ext) {
  R6_file_store$new(path, read, write, ext)
}

## Alternatively, hash files based on *disk content*.  Should make
## that an option.  One slight downside is that unless some serious
## work is done, then different versions of R will write out different
## versions of the file based on the first few bits.
R6_file_store <- R6::R6Class(
  "file_store",
  public = list(
    path = NULL,
    read = NULL,
    write = NULL,
    ext = NULL,

    initialize = function(path, read, write, ext) {
      dir.create(path, FALSE, TRUE)
      ## Ensures safety with setwd():
      self$path <- normalizePath(path, mustWork = TRUE)
      self$read <- read
      self$write <- write
      self$ext <- ext
    },

    destroy = function() {
      unlink(self$path, TRUE)
    },

    exists = function(hash) {
      file.exists(self$filename(hash))
    },

    del = function(hash) {
      invisible(file.remove(self$filename(hash)))
    },

    set = function(data) {
      hash <- hash_object(data) ## self$hash(data)
      dest <- self$filename(hash)
      if (!file.exists(dest)) {
        self$write(data, dest)
      }
      invisible(hash)
    },
    mset = function(data) {
      vcapply(data, self$set)
    },

    get = function(hash) {
      assert_scalar(hash)
      filename <- self$filename(hash, TRUE)
      if (is.na(filename)) {
        stop(HashError(hash))
      }
      self$read(filename)
    },

    mget = function(hash, missing = NULL) {
      filename <- self$filename(hash, TRUE)
      if (anyNA(filename)) {
        ret <- vector("list", length(hash))
        is_missing <- is.na(filename)
        ret[is_missing] <- list(missing)
        ret[!is_missing] <- lapply(filename[!is_missing], self$read)
        attr(ret, "missing") <- which(is_missing)
      } else {
        ret <- lapply(filename, self$read)
      }
      ret
    },

    list = function() {
      sub(paste0(self$ext, "$"), "",
          dir(self$path, pattern = paste0("^[[:xdigit:]]{32}", self$ext, "$")))
    },

    filename = function(hash, existing_only = FALSE) {
      assert_hash(hash)
      if (length(hash) > 0L) {
        ret <- file.path(self$path, paste0(hash, self$ext))
        if (existing_only) {
          ret[!file.exists(ret)] <- NA_character_
        }
        ret
      } else {
        character(0)
      }
    },

    hash_object = function(object) {
      hash_object(object)
    }
  ))

## This is directly from storr
HashError <- function(hash) {
  structure(list(hash = hash,
                 message = sprintf("hash '%s' not found", hash),
                 call = NULL),
            class = c("HashError", "error", "condition"))
}
