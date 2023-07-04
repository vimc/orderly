##' Run a batch of reports.
##'
##' Run one report multiple times with different sets of parameters.
##'
##' @param name Name of the report to run (see
##'   [orderly1::orderly_list()]).  A leading `src/` will be
##'   removed if provided, allowing easier use of autocomplete.
##'   Alternatively, the default of `NULL` is useful if you have
##'   already set the working directory to be the source directory.
##'
##' @param parameters Data frame of parameters passed to report. Each row
##'   represents a parameter set to be passed to one report run.
##'
##' @param continue_on_error If FALSE then if one report run fails the
##'   function will not attempt to run subsequent reports in the batch. If
##'   TRUE subsequent parameter sets will be run. If the report run fails
##'   during preparation e.g. because of missing parameters this will
##'   error and stop all subsequent parameter sets.
##'
##' @param ... Additional args passed to [orderly1::orderly_run()]
##'
##' @seealso [orderly1::orderly_run()] for details of report running
##'
##' @export
##' @return List of ids of newly created reports
##'
##' @examples
##'
##' path <- orderly1::orderly_example("demo")
##' params <- data.frame(nmin = c(0.2, 0.25))
##' ids <- orderly1::orderly_batch("other", params, root = path)
orderly_batch <- function(name = NULL, parameters = NULL,
                          continue_on_error = TRUE, ...) {
  if (NROW(parameters) < 1) {
    stop("Parameters for a batch must be a data frame with at least one row")
  }
  batch_id <- ids::random_id()
  run_report <- function(parameter_set) {
    id_file <- tempfile()
    tryCatch({
      id <- orderly_run_internal(name, parameters = parameter_set, ...,
                                 batch_id = batch_id, id_file = id_file)
      c(id = id,
        success = TRUE,
        as.list(parameter_set))
    }, error = function(e) {
      if (!file.exists(id_file) || !continue_on_error) {
        ## In this case that id_file doesn't exist it means run has
        ## failed during run_read so some issue with args to
        ## orderly_run_internal which have caused error before
        ## ID can even be generated. Error early so  user can fix
        ## issue with inputs.
        stop(e)
      }
      message(sprintf("Report run failed: %s\n", e$message))
      id <- readLines(id_file)
      c(id = id,
        success = FALSE,
        as.list(parameter_set))
    })
  }
  reports <- lapply(df2list(parameters), run_report)
  bind_df <- function(...) rbind.data.frame(..., make.row.names = FALSE,
                                            stringsAsFactors = FALSE)
  do.call(bind_df, reports)
}
