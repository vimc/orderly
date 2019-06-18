##' Create a "handle" for interacting with orderly repositories that
##' are hosted at a different path.  This might be useful in cases
##' where you have access to an orderly repository via a network mount
##' or a synchronised folder (e.g., Dropbox, Box, etc).  More
##' generally, \code{orderly_remote_path} implements an interface
##' used by orderly to abstract over different ways that orderly
##' repositories might be hosted remotely, including over HTTP APIs.
##'
##' @title Orderly remote at a different path
##'
##' @param path Path to the orderly store
##'
##' @param name Name of the remote
##'
##' @export
##' @seealso \code{\link{orderly_pull_dependencies}} and
##'   \code{\link{orderly_pull_archive}}, which are the primary ways
##'   these remote objects are used.  See also
##'   \href{https://github.com/vimc/orderly-web}{OrderlyWeb} for a
##'   system for hosting orderly repositories over an HTTP API.
##'
##' @example man-roxygen/example-remote.R
orderly_remote_path <- function(path, name = NULL) {
  R6_orderly_remote_path$new(path, name)
}


R6_orderly_remote_path <- R6::R6Class(
  "orderly_remote_path",

  public = list(
    config = NULL,
    name = NULL,

    initialize = function(path, name) {
      assert_file_exists(path)
      path <- normalizePath(path, "/", mustWork = TRUE)
      if (!file.exists(path_orderly_config_yml(path))) {
        stop("Does not look like an orderly repository: ", squote(path))
      }
      self$config <- orderly_config(path)
      self$name <- name %||% self$config$root
      lockBinding(quote(config), self)
      lockBinding(quote(name), self)
    },

    list_reports = function() {
      orderly_list(self$config)
    },

    list_versions = function(name) {
      d <- orderly_list_archive(self$config)
      d$id[d$name == name]
    },

    pull = function(name, id, root) {
      src <- file.path(path_archive(self$config$root), name, id)
      dest <- file.path(path_archive(root), name, id)
      copy_directory(src, dest, TRUE)
    },

    run = function(...) {
      stop("'orderly_remote_path' remotes do not run")
    }
  ))
