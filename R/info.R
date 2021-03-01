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
        "Failed to retrieve info for report %s:%s, rds does not exists",
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
  if (!is.null(info$meta$git)) {
    git <- list(
      branch = info$meta$git$branch,
      ref = info$meta$git$sha_short
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
