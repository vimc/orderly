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
  orderly_workflow_internal(name, envir, root, locate, message, instance,
                            remote, ref = NULL)
}

orderly_workflow_internal <- function(name, envir = NULL, root = NULL,
                                    locate = TRUE, message = NULL,
                                    instance = NULL, remote = NULL,
                                    ref = NULL) {
  config <- orderly_config(root, locate)
  workflow <- workflow$new(name, config, ref)
  workflow$run(envir, message, instance, remote)
}

workflow <- R6::R6Class(
  "workflow",

  public = list(
    workflow_name = NULL,
    steps = NULL,
    workflow_id = NULL,
    config = NULL,
    ref = NULL,

    initialize = function(workflow_name, config, ref = NULL) {
      self$workflow_name <- workflow_name
      self$workflow_id <- ids::random_id()
      self$config <- config
      self$ref <- ref
      git_restore <- private$git_checkout(self$ref)
      tryCatch({
        workflow_path <- path_orderly_workflow_dir(config$root, workflow_name)
        assert_file_exists(basename(workflow_path),
                           workdir = dirname(workflow_path),
                           name = "workflow configuration")
        raw <- yaml_read(workflow_path)
        self$steps <- validate_workflow(raw, self$config, self$workflow_name,
                                        workflow_path)
      }, finally = git_restore())
    },

    run = function(envir = NULL, message = NULL, instance = NULL,
                   remote = NULL) {
      orderly_log("workflow", sprintf("Running workflow '%s' with ID '%s'",
                                      self$workflow_name, self$workflow_id))
      run_ids <- rep(NA, length(self$steps))
      for (i in seq_along(run_ids)) {
        report <- self$steps[i]
        orderly_log("workflow", sprintf("Running report '%s'", report))
        tryCatch({
          run_ids[i] <- orderly_run_internal(
            report, envir = envir, root = self$config, message = message,
            instance = instance, remote = remote, commit = TRUE, echo = FALSE,
            workflow_info = list(id = self$workflow_id,
                                 name = self$workflow_name),
            ref = self$ref)
          orderly_log("workflow",
                      sprintf("Completed running & committing report '%s'",
                              report))
        },
        error = function(e) {
          orderly_log("error", sprintf("Running report '%s' failed", report))
          stop(e)
        })
      }
      orderly_log("workflow",
                  sprintf("Completed running workflow '%s' with ID '%s'",
                          self$workflow_name, self$workflow_id))
      run_ids
    }
  ),

  private = list(
    git_checkout = function(ref) {
      if (is.null(ref)) {
        return(function() NULL)
      }
      prev <- git_detach_head_at_ref(ref, self$config$root)
      function() git_checkout_branch(prev, TRUE, self$config$root)
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
