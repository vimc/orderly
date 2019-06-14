##' Create a "handle" for interacting with orderly repositories that
##' are hosted at a different path.  This might be useful in cases
##' where you have access to an orderly repository via a network mount
##' or a syncronised folder (e.g., Dropbox, Box, etc).  More
##' generally, \code{orderly_remote_path} is implements an interface
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
##' @examples
##' # Suppose we have a "remote" orderly repository at some path.
##' # This might be read-only for you in practice and available via a
##' # network filesystem or a dropbox folder synced to your computer.
##' # We'll populate this with a pair of reports:
##' path_remote <- orderly::orderly_example("demo")
##' id <- orderly::orderly_run("other", list(nmin = 0),
##'                            root = path_remote, echo = FALSE)
##' orderly::orderly_commit(id, root = path_remote)
##' id <- orderly::orderly_run("use_dependency",
##'                            root = path_remote, echo = FALSE)
##' orderly::orderly_commit(id, root = path_remote)
##'
##' # We'll create a an object to interact with this remote using
##' # orderly_remote_path.
##' remote <- orderly::orderly_remote_path(path_remote)
##'
##' # We can use this object directly
##' remote$list_reports()
##' remote$list_versions("other")
##'
##' # More typically one will interact with the functions
##' # orderly_pull_archive and orderly_pull_dependencies.
##'
##' # Now, suppose that you have your "local" copy of this; it shares
##' # the same source (ordinarily these would both be under version
##' # control with git):
##' path_local <- orderly::orderly_example("demo")
##'
##' # If we wanted to run the report "use_dependency" we need to have
##' # a copy of the report "other", on which it depends:
##' try(orderly::orderly_run("use_dependency", root = path_local))
##'
##' # We can "pull" depenencies of a report before running
##' orderly::orderly_pull_dependencies("use_dependency", remote = remote,
##'                                    root = path_local)
##'
##' # Now we can run the report because we have a local copy of the
##' # dependency:
##' orderly::orderly_run("use_dependency", root = path_local)
##'
##' # We can also directly pull previously run reports:
##' orderly::orderly_pull_archive("use_dependency", id, remote = remote,
##'                               root = path_local)
##' orderly::orderly_list_archive(root = path_local)
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

    push = function(name, id, root) {
      src <- file.path(path_archive(root), name, id)
      dest <- file.path(path_archive(self$config$root), name, id)
      copy_directory(src, dest, rollback_on_error = TRUE)
    },

    run = function(...) {
      stop("'orderly_remote_path' remotes do not run")
    }
  ))
