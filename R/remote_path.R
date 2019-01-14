##' @title Orderly remote at a different path
##' @param path Path to the orderly store
##' @param name Name of the remote
##' @export
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
      self$name <- name %||% self$config$path
      lockBinding(quote(config), self)
      lockBinding(quote(name), self)
    },

    list = function() {
      orderly_list(self$config)
    },

    list_versions = function(name) {
      d <- orderly_list_archive(self$config)
      d$id[d$name == name]
    },

    pull = function(name, id, root) {
      src <- file.path(path_archive(self$config$path), name, id)
      dest <- file.path(path_archive(root), name, id)
      copy_directory(src, dest, TRUE)
    },

    push = function(name, id, root) {
      src <- file.path(path_archive(root), name, id)
      dest <- file.path(path_archive(self$config$path), name, id)
      copy_directory(src, dest, rollback_on_error = TRUE)
    },

    publish = function(name, id, value = TRUE) {
      orderly_publish(id, value = value, name = name, config = self$config)
    },

    run = function(...) {
      stop("'orderly_remote_path' remotes do not run")
    }
  ))
