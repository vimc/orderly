##' @importFrom R6 R6Class
cache <- new.env(parent = emptyenv())

.onLoad <- function(...) {
  migrations <- package_version(names(available_migrations()))
  cache$current_archive_version <-
    utils::tail(migrations[migrations <= utils::packageVersion("orderly")], 1L)
  cache$default_remote <- list()
  cache$remotes <- list()
  cache$schema <- list()
}
