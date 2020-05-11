##' Run a workflow.
##'
##' WIP - skeleton implementation
##'
##' @param name Name of workflow to run
##'
##' @inheritParams orderly_list
##'
##' @return ID of workflow run
##' @export
##'
##' @examples
##' path <- orderly::orderly_example("demo")
##'
##' # To run most reports, provide the report name (and the path if
##' # not running in the working directory, as is the case here):
##' id <- orderly::orderly_workflow("my_workflow", root = path)
orderly_workflow <- function(name, root = NULL, locate = TRUE) {
  ## Locate file
  config <- orderly_config_get(root, locate)
  workflow <- orderly_workflow_get(name, config)

  ids <- workflow$run()

  ## TODO: Write to DB after run

  ids
}

orderly_workflow_get <- function(name, config) {
  filename <- path_orderly_workflow_dir(config$root, name)
  assert_file_exists(basename(filename), workdir = dirname(filename),
                     name = "workflow configuration")
  workflow$new(name, filename, config)
}

workflow <- R6::R6Class(
  "workflow",

  public = list(
    workflow_name = NULL,
    steps = NULL,
    workflow_id = NULL,
    config = NULL,

    initialize = function(workflow_name, workflow_path, config) {
      self$workflow_name <- workflow_name
      self$workflow_id <- ids::random_id()
      self$config <- config
      raw <- yaml_read(workflow_path)
      self$steps <- parse_steps(raw)
      validate_steps(self$steps, self$config, workflow_name)
    },

    run = function() {
      ## TODO: Implement run
      orderly_log("workflow", paste0("Running workflow ", self$workflow_id))
      ids::random_id()
    }
  )
)

parse_steps <- function(yml) {
  unname(unlist(yml$steps))
}

validate_steps <- function(steps, config, workflow_name) {
  reports <- basename(list_dirs(path_src(config$root)))
  for (step in steps) {
    if (!(step %in% reports)) {
      stop(sprintf("Cannot run workflow '%s' as report '%s' does not exist.",
                   workflow_name, step))
    }
  }
  invisible(TRUE)
}
