##' Return info about a report which has been run
##'
##' This will return info from either successful or failed reports. It will
##' look for the report with `id` in archive first and then look in drafts
##' if it can't be found from archive.
##'
##' @param id The report ID
##' @param name The name of the report
##' @inheritParams orderly_list
##'
##' @return Info from report run - this is subject to change. Returns a list
##' which includes report id, name, indication of success, run date and
##' elapsed time, parameters, git info (if available), path to logfile
##' (if exists) and details of error if the run failed
##' @export
##'
##' @examples
##' path <- orderly::orderly_example("demo")
##' id <- orderly::orderly_run("minimal", root = path)
##' orderly::orderly_info(id, "minimal", root = path)
orderly_info <- function(id, name, root = NULL, locate = TRUE) {
  report <- orderly_find_report(id, name, root, locate, draft = "newer",
                                must_work = TRUE)
  success_rds <- path_orderly_run_rds(report)
  if (file.exists(success_rds)) {
    info <- readRDS(success_rds)
    success <- TRUE
  } else {
    fail_rds <- path_orderly_fail_rds(report)
    if (file.exists(fail_rds)) {
      info <- readRDS(fail_rds)
      success <- FALSE
    } else {
      stop(sprintf(
        "Failed to retrieve info for report %s:%s, rds does not exist",
        name, id))
    }
  }

  logfile <- NULL
  if (file.exists(path_orderly_log(report))) {
    logfile <- path_orderly_log(report)
  }

  ## Find rds from name & id in draft & archive (maybe allow switch?)
  ## Read rds
  ## Parse info out into nice format
  git <- NULL
  if (!is.null(info$git)) {
    git <- list(
      branch = info$git$branch,
      ref = info$git$sha_short
    )
  }
  error <- NULL
  if (!is.null(info$error)) {
    error <- list(
      message = info$error$error$message,
      trace = info$error$trace
    )
  }
  list(
    id = info$meta$id,
    name = info$meta$name,
    success = success,
    date = info$meta$date,
    elapsed = info$meta$elapsed,
    git = git,
    parameters = info$meta$parameters,
    logfile = logfile,
    error = error
  )
}

##' Return details of packages required by all src reports in this orderly repo
##'
##' @inheritParams orderly_list
##'
##' @return List of packages required by all src reports
##' @export
##'
##' @examples
##' orderly::orderly_packages()
##' orderly::orderly_packages(root = path)
orderly_packages <- function(root = NULL, locate = TRUE) {
  cfg <- orderly_config(root, locate)
  names <- basename(list_dirs(path_src(cfg$root)))
  packages <- unlist(lapply(names, function(name) orderly_recipe$new(name, cfg)$packages))
  unique(as.list(packages))
}
