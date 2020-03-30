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
##' ids <- orderly::orderly_run("other", params, root = path)
orderly_batch <- function(name = NULL, parameters = NULL, ...) {
  ## Assert each column named? or a test at least if not to see what happens
  batch_id <- ids::random_id()
  ids <- lapply(seq_len(nrow(params)), function(row) {
    orderly_run(name, parameters = params[row, ], ..., batch_id = batch_id)
  })
  ids
}
