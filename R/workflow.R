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
  workflow$new(filename)
}

workflow <- R6::R6Class(
  "workflow",

  public = list(
    steps = NULL,
    workflow_id = NULL,

    initialize = function(workflow_path) {
      self$workflow_id <- ids::random_id()
      raw <- yaml_read(workflow_path)
      self$steps <- parse_steps(raw)
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
