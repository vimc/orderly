##' Run a workflow.
##'
##' This runs & commits each of the reports configured in the workflow in turn.
##' Note that if one report fails to be run or to be commited this will
##' continue and attempt to run the remaining reports.
##'
##' @param name Name of workflow to run
##'
##' @inheritParams orderly_list
##' @inheritParams orderly_run
##'
##' @return ID of workflow run
##' @export
##'
##' @examples
##' path <- orderly::orderly_example("demo")
##'
##' # To run most reports, provide the report name (and the path if
##' # not running in the working directory, as is the case here):
##' ids <- orderly::orderly_workflow("my_workflow", root = path)
orderly_workflow <- function(name, envir = NULL, root = NULL, locate = TRUE,
                             message = NULL, instance = NULL, remote = NULL) {
  ## Locate file
  config <- orderly_config_get(root, locate)
  workflow <- orderly_workflow_get(name, config)

  workflow$run(envir, message, instance, remote)

  ## TODO: Write to DB after run
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
      self$steps <- validate_workflow(raw, self$config, self$workflow_name,
                                      workflow_path)
    },

    run = function(envir = NULL, message = NULL, instance = NULL,
                   remote = NULL) {
      orderly_log("workflow", sprintf("Running workflow '%s' with ID '%s'",
                                      self$workflow_name, self$workflow_id))
      output <- lapply(self$steps, run_single, envir, self$config, message,
                       instance, remote)
      run_ids <- vcapply(output, "[[", "value")
      out_message <- sprintf("Completed running workflow '%s' with ID '%s'\n",
                             self$workflow_name, self$workflow_id)
      no_failed <- length(self$steps) - length(run_ids)
      if (no_failed > 0) {
        out_message <- sprintf("%s with %s failure(s)", out_message, no_failed)
      }
      orderly_log("workflow", out_message)
      run_ids
    }
  )
)

parse_steps <- function(yml) {
  unname(unlist(yml$steps))
}

validate_workflow <- function(raw_yml, config, workflow_name, workflow_path) {
  check_fields(raw_yml, workflow_path, "steps", NULL)
  for (step in raw_yml$steps) {
    check_fields(step, sprintf("%s:steps", workflow_path), "name", NULL)
  }
  reports <- orderly_list(config)
  steps <- parse_steps(raw_yml)
  msg <- setdiff(steps, reports)
  if (length(msg)) {
    stop(sprintf("Cannot run workflow '%s' as reports missing: %s",
                 workflow_name, paste(squote(msg), collapse = ", ")))
  }
  steps
}

run_single <- function(report, envir, root, message, instance, remote) {
  tryCatch({
    orderly_log("workflow", sprintf("Running report '%s'", report))
    id <- orderly_run(report, envir = envir, root = root, message = message,
                      instance = instance, remote = remote)
    orderly_log("workflow",
                sprintf("Finished running report '%s'", report))
    orderly_log("workflow",
                sprintf("Committing report '%s'", report))
    orderly_commit(id, root = root)
    orderly_log("workflow",
                sprintf("Finished committing report '%s'", report))
    list(value = id, success = TRUE, error = NULL)
  },
  error = function(e) {
    orderly_log("error",
                sprintf("Running report '%s' failed with message \n%s",
                        report, e$message))
    list(value = NULL, success = FALSE, error = e)
  })
}
