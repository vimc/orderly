##' Deprecated functions that will be removed from \code{orderly} in
##' the near future.  Please update to use the new version.
##'
##' Use of any of these functions will produce a warning that will
##' indicate the new version of the function to use.
##'
##' @title Deprecated functions
##'
##' @param ... Arguments that will be passed through to the current
##'   function; see the appropriate help there, and ideally update
##'   your code to use the new function.
##'
##' @name orderly-deprecated
##' @rdname orderly-deprecated
##' @keywords internal
NULL

##' @details For \code{pull_archive} use
##' \code{\link{orderly_pull_archive}}
##' @rdname orderly-deprecated
##' @export
pull_archive <- function(...) {
  ## This is possibly used by science and ebola and should be removed
  ## at release.
  .Deprecated("orderly_pull_archive")
  orderly_pull_archive(...)
}


##' @details For \code{pull_dependencies} use
##' \code{\link{orderly_pull_dependencies}}
##' @rdname orderly-deprecated
##' @export
pull_dependencies <- function(...) {
  ## This is certainly used by science and ebola and should stay up
  ## until release (but ideally be removed by then)
  .Deprecated("orderly_pull_dependencies")
  orderly_pull_dependencies(...)
}


##' @details For \code{set_default_remote} use
##' \code{\link{orderly_default_remote_set}}
##' @rdname orderly-deprecated
##' @export
set_default_remote <- function(...) {
  ## this is possibly used by science but it's not totally clear - we
  ## can rip this out just before release
  .Deprecated("orderly_default_remote_set")
  orderly_default_remote_set(...)
}
