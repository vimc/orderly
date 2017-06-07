##' Start and stop the orderly log.  Soon this might swap out for
##' \code{loggr}, but for now this should do.  When active, some
##' actions will print diagnostic information to the message stream.
##'
##' The interface here will change by adding arguments.  Future versions
##' may support logging to a file.
##' @title Start and stop log
##' @export
##' @rdname orderly_log
##'
##' @return \code{orderly_log_start} invisibly returns a logical
##'   indicating if logging was previously enabled.  This allows
##'   patterns like:
##' \preformatted{if (!orderly::orderly_log_start()) {
##'   orderly::orderly_log_stop()
##' }
##' }
##' to have a scoped log (i.e., log for the duration of a function).
orderly_log_start <- function() {
  invisible(isTRUE(options(orderly.log = TRUE)$orderly.log))
}
##' @export
##' @rdname orderly_log
orderly_log_stop <- function() {
  invisible(isTRUE(options(orderly.log = NULL)$orderly.log))
}

##' Send an entry to the orderly log.  This is designed primarily for
##' use with pacakges that build off of orderly, so that they can log
##' in a consistent way.
##' @title Send entry to orderly log
##' @param topic Up to 9 character text string with the log topic
##' @param value Character string with the log entry
##' @export
orderly_log <- function(topic, value) {
  if (isTRUE(getOption("orderly.log"))) {
    n <- length(value) - 1L
    if (n > 0L) {
      topic <- c(topic, rep_len("...", n))
    }
    str <- trimws(sprintf("[ %-9s ]  %s", topic, value))
    if (n > 0L) {
      str <- paste(str, collapse = "\n")
    }
    message(str)
  }
}
