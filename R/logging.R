##' Start and stop the orderly log.  When active, some actions will
##' print diagnostic information to the message stream.  This is set
##' to be on by default.
##'
##' The interface here will change by adding arguments.  Future versions
##' may support logging to a file.
##' @title Start and stop log
##' @export
##' @rdname orderly_log
##'
##' @return \code{orderly_log_on} and \code{orderly_log_off} invisibly
##'   returns a logical indicating if logging was previously enabled.
##'   This allows patterns like:
##'
##' \preformatted{if (!orderly::orderly_log_off()) {
##'   orderly::orderly_log_on()
##' }
##' }
##' to disable logging within a function
orderly_log_on <- function() {
  invisible(!isTRUE(options(orderly.nolog = NULL)$orderly.nolog))
}
##' @export
##' @rdname orderly_log
orderly_log_off <- function() {
  invisible(!isTRUE(options(orderly.nolog = TRUE)$orderly.nolog))
}

##' Send an entry to the orderly log.  This is designed primarily for
##' use with pacakges that build off of orderly, so that they can log
##' in a consistent way.
##' @title Send entry to orderly log
##' @param topic Up to 9 character text string with the log topic
##' @param value Character string with the log entry
##' @export
orderly_log <- function(topic, value) {
  if (!isTRUE(getOption("orderly.nolog"))) {
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

orderly_log_break <- function() {
  orderly_log(strrep("-", 9),
              strrep("-", max(getOption("width") - 18, 0)))
}
