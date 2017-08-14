## Try to make it easy to run things
main <- function(args = commandArgs(TRUE)) {
  opts <- main_args(args)
  orderly_log_start()
  on.exit(orderly_log_stop())
  opts$target(opts)
}

main_args <- function(args) {
  ## With docopt there is no easy way of passing in a json string and
  ## have it survive quoting.  This is very sad.  So we'll use
  ## optparse instead I think.

  ## Argument parsing will be somewhat complicated because we'll
  ## implement a number of different commands.  We'll set up the
  ## parser to allow everything, and then do the validation later.
  root <- optparse::make_option(c("-r", "--root"),
                                help = "Path to the orderly root",
                                type = "character",
                                default = NULL)
  parser <- optparse::OptionParser(option_list = list(root),
                                   usage = "%prog [options] <name>")
  res <- optparse::parse_args(parser, args, positional_arguments = TRUE)
  res$command <- res$args[[1]]
  res$args <- res$args[-1L]
  res <- switch(res$command,
                run = main_args_run(res),
                list = main_args_list(res),
                cleanup = main_args_cleanup(res),
                commit = main_args_commit(res),
                publish = main_args_publish(res),
                rebuild = main_args_rebuild(res),
                stop(sprintf("unknown command '%s'", res$command)))
}

## 1. orderly [--root] run <name> [--no-commit] [--parameters=PARAMS]
main_args_run <- function(res) {
  opts <- list(
    optparse::make_option("--no-commit",
                          help = "Do not commit the report",
                          type = "logical",
                          default = FALSE,
                          action = "store_true",
                          dest = "no_commit"),
    optparse::make_option("--parameters",
                          help = "Parameters (in json format)",
                          type = "character",
                          default = NULL))
  parser <- optparse::OptionParser(option_list = opts,
                                   usage = "%prog [options] <name>")
  opts <- optparse::parse_args(parser, res$args, positional_arguments = 1L)
  opts_combine(res, opts, main_do_run)
}

main_do_run <- function(x) {
  config <- orderly_config_get(x$options$root, TRUE)
  name <- x$args
  commit <- !x$options$no_commit
  parameters <- x$options$parameters
  if (!is.null(x$options$parameters)) {
    parameters <- jsonlite::fromJSON(parameters)
  }

  main_run <- function(name, parameters, config, commit) {
    id <- orderly_run(name, parameters, config = config)
    if (commit) {
      orderly_commit(id, name, config)
    }
    id
  }

  log <- tempfile()
  ## we should run this with try() so that we can capture logs there
  id <- capture_log(main_run(name, parameters, config, commit), log, TRUE)
  dest <- (if (commit) path_archive else path_draft)(config$path)
  file.copy(log, file.path(dest, name, id, "orderly.log"))

  ## TODO: is it useful to write this to some location (rather than
  ## stderr) to indicate what was done?
  message("id:", id)
}

## 2. orderly cleanup [--draft]
main_args_cleanup <- function(res) {
  opts <- list(
    optparse::make_option("--no-draft",
                          help = "Do not clean draft reports",
                          type = "logical",
                          default = FALSE,
                          action = "store_true",
                          dest = "no_draft"),
    optparse::make_option("--no-data",
                          help = "Do not clean data",
                          type = "logical",
                          default = FALSE,
                          action = "store_true",
                          dest = "no_data"),
    optparse::make_option("--failed-only",
                          help = "Clean only failed drafts",
                          type = "logical",
                          default = FALSE,
                          action = "store_true",
                          dest = "failed_only"))
  parser <- optparse::OptionParser(option_list = opts,
                                   usage = "%prog [options] <name>")
  opts <- optparse::parse_args(parser, res$args, positional_arguments = 0L)
  opts_combine(res, opts, main_do_cleanup)
}

main_do_cleanup <- function(x) {
  config <- orderly_config_get(x$options$root, TRUE)
  draft <- !x$options$no_draft
  data <- !x$options$no_data
  failed_only <- x$options$failed_only
  orderly_cleanup(config = config, draft = draft, data = data,
                  failed_only = failed_only)
}

## 3. commit
main_args_commit <- function(res) {
  opts <- list()
  parser <- optparse::OptionParser(option_list = opts,
                                   usage = "%prog [options] <name>")
  opts <- optparse::parse_args(parser, res$args, positional_arguments = 1L)
  opts_combine(res, opts, main_do_commit)
}

main_do_commit <- function(x) {
  config <- orderly_config_get(x$options$root, TRUE)
  id <- x$args
  orderly_commit(id, config = config)
}

## 4. publish
main_args_publish <- function(res) {
  opts <- list(
    optparse::make_option("--unpublish",
                          help = "Set the report to unpublished",
                          type = "logical",
                          default = FALSE,
                          action = "store_true",
                          dest = "unpublish"))
  parser <- optparse::OptionParser(option_list = opts,
                                   usage = "%prog [options] <name>")
  opts <- optparse::parse_args(parser, res$args, positional_arguments = 1L)
  opts_combine(res, opts, main_do_publish)
}

main_do_publish <- function(x) {
  config <- orderly_config_get(x$options$root, TRUE)
  value <- !x$options$unpublish
  id <- x$args
  orderly_publish(id, value, config = config)
}

## 4. rebuild
main_args_rebuild <- function(res) {
  opts <- list()
  parser <- optparse::OptionParser(option_list = opts,
                                   usage = "%prog [options] <name>")
  opts <- optparse::parse_args(parser, res$args, positional_arguments = 0L)
  opts_combine(res, opts, main_do_rebuild)
}

main_do_rebuild <- function(x) {
  config <- orderly_config_get(x$options$root, TRUE)
  orderly_rebuild(config)
}

## 5. list
main_args_list <- function(res) {
  opts <- list()
  parser <- optparse::OptionParser(option_list = opts,
                                   usage = "%prog [options] <name>")
  opts <- optparse::parse_args(parser, res$args, positional_arguments = 1L)
  opts$args <- match.arg(res$args, c("names", "drafts", "archive"))
  opts_combine(res, opts, main_do_list)
}

main_do_list <- function(x) {
  config <- orderly_config_get(x$options$root, TRUE)
  switch(x$args,
         names = writeLines(orderly_list(config)),
         drafts = print(orderly_list_drafts(config)),
         archive = print(orderly_list_archive(config)),
         stop("orderly bug"))
  invisible(NULL)
}

write_script <- function(path) {
  if (!is_directory(path)) {
    stop("'path' must be a directory")
  }
  code <- c("#!/usr/bin/env Rscript", "orderly:::main()")
  path_bin <- file.path(path, "orderly")
  writeLines(code, path_bin)
  Sys.chmod(path_bin, "755")
  invisible(path_bin)
}

opts_combine <- function(base, new, target) {
  base$options <- modify_list(base$options, new$options)
  base$args <- new$args
  base$target <- target
  base
}
