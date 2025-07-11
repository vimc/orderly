##' Start and stop the orderly log.  When active, some actions will
##' print diagnostic information to the message stream.  This is set
##' to be on by default.
##'
##' The function `orderly_log` is designed to be used from
##' applications that extend orderly, while the functions
##' `orderly_log_on` and `orderly_log_off` can be used by
##' applications or users to enable and disable log messages.
##'
##' The interface here may expand by adding arguments or change
##' behaviour based on global options. Future versions may support
##' logging to a file, or adding timestamps, or logging in json
##' format, etc.
##'
##' @title Orderly logging and diagnostic messages
##' @export
##' @rdname orderly_log
##'
##' @return `orderly_log_on` and `orderly_log_off` invisibly
##'   returns a logical indicating if logging was previously enabled.
##'   This allows patterns like:
##'
##' ```
##' if (!orderly1::orderly_log_off()) {
##'   on.exit(orderly1::orderly_log_on())
##' }
##' ```
##'
##' to disable logging within a function (the `on.exit` block
##'   will be run when the function exits).
##'
##' @seealso [orderly1::orderly_run()], which makes use of these log
##'   messages
##'
##' @examples
##' # We are going to log things below
##' logging_was_enabled <- orderly1::orderly_log_on()
##'
##' # About orderly log messages:
##' # Orderly log messages have the form "[title] message"
##' orderly1::orderly_log_on()
##' orderly1::orderly_log("title", "message")
##'
##' # If logging is disabled they are not printed:
##' orderly1::orderly_log_off()
##' orderly1::orderly_log("title", "message")
##'
##' # Restore to previous settings:
##' if (logging_was_enabled) {
##'   orderly1::orderly_log_on()
##' }
orderly_log_on <- function() {
  invisible(!isTRUE(options(orderly.nolog = NULL)$orderly.nolog))
}


##' @export
##' @rdname orderly_log
orderly_log_off <- function() {
  invisible(!isTRUE(options(orderly.nolog = TRUE)$orderly.nolog))
}


##' @param topic Up to 9 character text string with the log topic
##' @param value Character string with the log entry
##' @rdname orderly_log
##' @export
orderly_log <- function(topic, value) {
  if (!isTRUE(getOption("orderly.nolog"))) {
    style <- orderly_style(orderly_log_style(topic))
    n <- length(value) - 1L
    if (n > 0L) {
      topic <- c(topic, rep_len("...", n))
    }
    str <- trimws(sprintf("[ %s ]  %s",
                          style(format(topic, width = 10)), value))
    if (n > 0L) {
      str <- paste(str, collapse = "\n")
    }
    message(str)
  }
}

orderly_log_break <- function() {
  orderly_log(strrep("-", 10),
              strrep("-", max(getOption("width") - 18, 0)))
}


orderly_warning <- function(msg) {
  if (getOption("orderly.nowarnings", FALSE)) {
    message(msg)
  } else {
    warning(msg, immediate. = TRUE, call. = FALSE)
  }
}


orderly_log_style <- function(topic) {
  switch(tolower(topic),
         warning = "alert",
         error = "alert",
         unexpected = "alert",
         rollback = "alert",
         workflow = "workflow",
         "highlight")
}
