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
      hash <- digest::digest(data)
      dest <- self$filename(hash)
      if (!file.exists(dest)) {
        self$write(data, dest)
      }
      invisible(hash)
    },
    mset = function(data) {
      ## It's not clear what we do here where 'data' is an environment
      ## rather than a list.
      vcapply(data, self$set)
    },

    get = function(hash) {
      ## Scalar name
      self$read(self$filename(hash))
    },

    mget = function(hash) {
      lapply(hash, self$get)
    },

    list = function() {
      sub(paste0(self$ext, "$"), "",
          dir(self$path, pattern = paste0("^[[:xdigit:]]{32}", self$ext, "$")))
    },

    filename = function(hash) {
      if (!all(grepl("^[[:xdigit:]]+$", hash))) {
        stop("Invalid name")
      }
      if (length(hash) > 0L) {
        file.path(self$path, paste0(hash, self$ext))
      } else {
        character(0)
      }
    }
  ))
