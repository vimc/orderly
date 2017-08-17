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

  desc <- c("",
            "The <command> argument must be one of:",
            "",
            sprintf("  * %s: %s",
                    names(main_args_commands),
                    vcapply(main_args_commands, "[[", "name",
                            USE.NAMES = FALSE)))

  parser <- optparse::OptionParser(
    option_list = list(root),
    usage = "%prog [options] <command> <args>",
    description = paste(desc, collapse = "\n"))
  res <- optparse::parse_args(parser, args, positional_arguments = TRUE,
                              print_help_and_exit = FALSE)
  if (res$options$help) {
    if(length(res$args) == 0) {
      optparse_die_help(parser)
    }
  }
  res$command <- res$args[[1]]
  res$args <- res$args[-1L]

  use <- main_args_commands[[res$command]]
  if (is.null(use)) {
    optparse_die(parser, sprintf("unknown command '%s'", res$command))
  }
  use$args(res)
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
    optparse::make_option("--print-log",
                          help = "Print log (rather than storing it)",
                          type = "logical",
                          default = FALSE,
                          action = "store_true",
                          dest = "print_log"),
    optparse::make_option("--parameters",
                          help = "Parameters (in json format)",
                          type = "character",
                          default = NULL))
  parser <- optparse::OptionParser(
    option_list = opts,
    usage = "%prog [--root=ROOT] run [options] <name>")
  if (res$options$help) {
    optparse_die_help(parser)
  }
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
  print_log <- x$options$print_log

  main_run <- function(name, parameters, config, commit) {
    id <- orderly_run(name, parameters, config = config)
    if (commit) {
      orderly_commit(id, name, config)
    }
    id
  }

  if (print_log) {
    log <- tempfile()
    ## we should run this with try() so that we can capture logs there
    id <- capture_log(main_run(name, parameters, config, commit), log, TRUE)
    dest <- (if (commit) path_archive else path_draft)(config$path)
    file.copy(log, file.path(dest, name, id, "orderly.log"))
  } else {
    id <- main_run(name, parameters, config, commit)
  }

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
  parser <- optparse::OptionParser(
    option_list = opts,
    usage = "%prog [--root=ROOT] cleanup [options]")
  if (res$options$help) {
    optparse_die_help(parser)
  }
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
  parser <- optparse::OptionParser(
    option_list = opts,
    usage = "%prog [--root=ROOT] commit [options] <id>")
  if (res$options$help) {
    optparse_die_help(parser)
  }
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
  parser <- optparse::OptionParser(
    option_list = opts,
    usage = "%prog [--root=ROOT] publish [options] <id>")
  if (res$options$help) {
    optparse_die_help(parser)
  }
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
  parser <- optparse::OptionParser(
    option_list = opts,
    usage = "%prog [--root=ROOT] rebuild [options]")
  if (res$options$help) {
    optparse_die_help(parser)
  }
  opts <- optparse::parse_args(parser, res$args, positional_arguments = 0L)
  opts_combine(res, opts, main_do_rebuild)
}

main_do_rebuild <- function(x) {
  config <- orderly_config_get(x$options$root, TRUE)
  orderly_rebuild(config)
}

## 5. list
main_args_list <- function(res) {
  ## TODO: this should optionally filter name I think
  opts <- list()
  parser <- optparse::OptionParser(
    option_list = opts,
    usage = "%prog [--root=ROOT] list [options] (names | drafts | archive)")
  if (res$options$help) {
    optparse_die_help(parser)
  }
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

optparse_die <- function(parser, message) {
  optparse::print_help(parser)
  stop(message, call. = FALSE)
}

optparse_die_help <- function(parser) {
  optparse_die(parser, "(Aborting as help requested)")
}

main_args_commands <-
  list(run = list(name = "run a report",
                  args = main_args_run),
       commit = list(name = "commit a report",
                   args = main_args_commit),
       publish = list(name = "publish a report",
                      args = main_args_publish),
       list = list(name = "list reports",
                   args = main_args_list),
       cleanup = list(name = "remove drafts and dangling data",
                      args = main_args_cleanup),
       rebuild = list(name = "rebuild the database",
                      args = main_args_rebuild))
