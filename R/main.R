## Try to make it easy to run things
main <- function(args = commandArgs(TRUE)) {
  orderly_log_start()
  args <- main_args(args)
  ## Hmm, capturing the output here is going to be a trick really!
  ## We'll need some way of indicating where to save the recently
  ## created ID I think.  Or we spawn a *second* R process and capture
  ## output out of that as a log.  That's not a *terrible* idea,
  ## though we can do better.
  config <- orderly_config_get(args$root, TRUE)
  name <- args$name
  parameters <-
    if (!is.null(args$parameters)) jsonlite::fromJSON(args$parameters) else NULL
  log <- tempfile()
  commit <- !args[["no-commit"]]
  ## we should run this with try because failure is
  id <- capture_log(
    main_run(name, parameters, config, commit), log, TRUE)
  dest <- (if (commit) path_archive else path_draft)(config$path)
  file.copy(log, file.path(dest, name, id, "orderly.log"))
  message("id:", id)
}

main_args <- function(args) {
  c("Usage:",
    "  orderly [--root=<path>] [--no-commit] [--parameters=<json>] <name>",
    "",
    "Options",
    "  --root=<path>        Path to the orderly root",
    "  --no-commit          Do not commit the report",
    "  --parameters=<json>  Parameters (in json format)") -> usage
  ret <- docopt::docopt(paste(usage, collapse = "\n"), args)
  ret[c("root", "name", "no-commit", "parameters")]
}

## TODO: need to deal with error handling here as we'd want to collect
## up the failed jobs so that they can be reported on nicely.
main_run <- function(name, parameters, config, commit) {
  id <- orderly_run(name, parameters, config = config)
  if (commit) {
    orderly_commit(name, id, config)
  }
  id
}

write_script <- function(path) {
  if (!is_directory(path)) {
    stop("'path' must be a directory")
  }
  code <- c("#!/usr/bin/env Rscript", "orderly:::main()")
  path_bin <- file.path(path, "orderly")
  writeLines(code, path_bin, "orderly")
  Sys.chmod(path_bin, "755")
  invisible(path_bin)
}
