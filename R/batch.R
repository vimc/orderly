##' Run a batch of reports.
##'
##' Run one report multiple times with different sets of parameters.
##'
##' @param name Name of the report to run (see
##'   \code{\link{orderly_list}}).  A leading \code{src/} will be
##'   removed if provided, allowing easier use of autocomplete.
##'   Alternatively, the default of \code{NULL} is useful if you have
##'   already set the working directory to be the source directory.
##'
##' @param parameters Data frame of parameters passed to report. Each row
##'   represents a parameter set to be passed to one report run.
##'
##' @param log_file File to log to
##'
##' @param ... Additional args passed to \code{\link{orderly_run}}
##'
##' @seealso \code{\link{orderly_run}} for details of report running
##'
##' @export
##' @return List of ids of newly created reports
##'
##' @examples
##'
##' path <- orderly::orderly_example("demo")
##' params <- data.frame(nmin = c(0.2, 0.25))
##' ids <- orderly::orderly_batch("other", params, root = path)
orderly_batch <- function(name = NULL, parameters = NULL, ...) {
  batch_id <- ids::random_id()
  vcapply(df2list(parameters), function(parameter_set) {
    ## Capture log if some file path is passed in (as the root)
    # log <- tempfile()
    # capture_log(main_batch(), log)
    orderly_run(name, parameters = parameter_set, ..., batch_id = batch_id)
  })
}
