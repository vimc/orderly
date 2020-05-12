## Try to make it easy to run things
main <- function(args = commandArgs(TRUE)) {
  dat <- cli_args_process(args)
  dat$target(dat)
}


cli_args_process <- function(args) {
  dat <- cli_args_preprocess(args)
  pos <- cli_commands()
  if (!(dat$command %in% names(pos))) {
    orderly_cli_error(sprintf(
      "'%s' is not an orderly command. See orderly --help",
      dat$command))
  }
  usage <- pos[[dat$command]]$usage
  dat$options <- docopt_parse(usage, c(dat$command, dat$args))
  names(dat$options) <- gsub("-", "_", names(dat$options), fixed = TRUE)
  dat$options$root <- dat$root
  dat$target <- pos[[dat$command]]$target

  ## Any additional processing:
  if (dat$command == "run") {
    dat$options$parameters <- cli_args_process_run_parameters(
      dat$options$parameter)
    dat$options$name <- dat$options[["<name>"]] # docopt bug?
  } else if (dat$command == "list") {
    dat$options$type <- cli_args_process_list_type(dat$options)
  } else if (dat$command == "batch") {
    dat$options$parameters <- cli_args_process_batch_parameters(
      dat$options$parameter, dat$options$file)
    dat$options$name <- dat$options[["<name>"]] # docopt bug?
  }

  dat
}


usage_base <- "Usage:
  orderly [options] <command> [<args>...]

Options:
  --root=ROOT  Path to the orderly root

Commands:
  run          Run a report
  commit       Commit a report
  list         List reports
  latest       Find the most recent report
  pull         Pull reports from remote servers
  cleanup      Remove drafts and dangling data
  rebuild      Rebuild the database
  migrate      Migrate the archive
  batch        Run a batch of reports"

cli_args_preprocess <- function(args) {
  ## This will work ok for filtering away unwanted arguments *if* the
  ## user chooses a reasonable argument name.  Things like
  ##
  ##   orderly run --option name
  ##
  ## will create some odd error messages, because the --option is not
  ## accepted.
  cmds <- names(cli_commands())
  i <- args %in% cmds
  if (any(i)) {
    j <- seq_along(args) <= which(i)[[1]]
    base <- args[j]
    args <- args[!j]
  } else {
    base <- args
    args <- character(0)
  }

  dat <- docopt_parse(usage_base, base)
  list(root = dat$root,
       list_commands = dat[["--list-commands"]],
       command = dat$command,
       args = args)
}


cli_args_process_run_parameters <- function(parameters) {
  if (length(parameters) == 0L) {
    NULL
  } else {
    p <- split_key_values(parameters)
    value <- lapply(p, function(x) parse_parameter(x[[2L]]))
    set_names(value, vcapply(p, "[[", 1L))
  }
}

cli_args_process_batch_parameters <- function(parameters, file) {
  if (!is.null(file)) {
    ## For now just assuming that this is a csv file but we can switch
    ## behaviour here and take json if needed.
    assert_file_exists(file, FALSE, name = "Parameters file")
    read_csv(file, check.names = FALSE)
  } else if (length(parameters) == 0L) {
    NULL
  } else {
    p <- split_key_values(parameters)
    values_per_param <- lengths(strsplit(unlist(parameters), ","))
    if (length(unique(values_per_param)) != 1) {
      stop(sprintf(
        "All params must have the same number of values, got \n%s",
        paste(parameters, collapse = "\n")
      ))
    }
    value <- lapply(p, function(x) parse_batch_parameters(x[[2L]]))
    t <- set_names(value, vcapply(p, "[[", 1L))
    columns <- lapply(t, function(col) {
      unlist(matrix(col))
    })
    do.call(cbind.data.frame, c(columns, stringsAsFactors = FALSE))
  }
}

split_key_values <- function(parameters) {
  p <- strsplit(parameters, "=", fixed = TRUE)
  err <- lengths(p) != 2
  if (any(err)) {
    stop(sprintf(
      "Invalid parameters %s - all must be in form key=value",
      paste(squote(parameters[err]), collapse = ", ")))
  }
  p
}


## docopt.R seems not to follow the docopt spec, so we can't do
##
## <name>... [--] [<parameter>...]
##
## and allow a vector of parameters through
##
## So we'll do a different approach for batch runs.
usage_run <- "Usage:
  orderly run [options] <name> [<parameter>...]

Options:
  --instance=NAME  Database instance to use (if instances are configured)
  --no-commit      Do not commit the report
  --print-log      Print the log (rather than storing it)
  --id-file=FILE   File to write the id into
  --ref=REF        Git reference (branch or sha) to use
  --fetch          Fetch git before updating reference
  --pull           Pull git before running report
  --message=TEXT   A message explaining why the report was run

Parameters, if given, must be passed through in key=value pairs"


main_do_run <- function(x) {
  ## TODO: Get some classed errors though here and then write out
  ## information about whether or not things worked and why they
  ## didn't.  Possible issues (in order)
  ##
  ## * orderly report not found
  ## * error while preparing (e.g., package not found)
  ## * error while running report
  ## * error checking artefacts
  config <- orderly_config_get(x$options$root, TRUE)
  name <- x$options$name
  commit <- !x$options$no_commit
  instance <- x$options$instance
  id_file <- x$options$id_file
  parameters <- x$options$parameters
  print_log <- x$options$print_log
  ref <- x$options$ref
  fetch <- x$options$fetch
  pull <- x$options$pull
  message <- x$options$message

  if (print_log) {
    sink(stderr(), type = "output")
    on.exit(sink(NULL, type = "output"))
  } else {
    config$add_run_option("capture_log", TRUE)
  }

  if (pull) {
    if (is.null(ref)) {
      git_pull(config$root)
    } else {
      orderly_cli_error(
        "Can't use --pull with --ref; perhaps you meant --fetch ?")
    }
  }
  id <- orderly_run2(name, parameters, root = config, id_file = id_file,
                     instance = instance,
                     ref = ref, fetch = fetch, message = message)
  if (commit) {
    orderly_commit(id, name, config)
    path_rds <- path_orderly_run_rds(
      file.path(config$root, "archive", name, id))
    post_success(readRDS(path_rds), config)
  }

  message("id:", id)
}


usage_cleanup <- "Usage:
  orderly cleanup [options]
Options:
  --no-draft     Do not clean draft reports
  --no-data      Do not clean data
  --failed-only  Clean only failed drafts"


main_do_cleanup <- function(x) {
  root <- x$options$root
  draft <- !x$options$no_draft
  data <- !x$options$no_data
  failed_only <- x$options$failed_only
  orderly_cleanup(root = root, draft = draft, data = data,
                  failed_only = failed_only)
}


usage_commit <- "Usage:
  orderly commit <id>"


main_do_commit <- function(x) {
  root <- x$options$root
  id <- x$options$id
  orderly_commit(id, root = root, locate = TRUE)
}


## 4. rebuild
usage_rebuild <- "Usage:
  orderly rebuild [options]

Options:
  --if-schema-changed  Only rebuild on schema change"


main_do_rebuild <- function(x) {
  root <- x$options$root
  if_schema_changed <- x$options$if_schema_changed
  orderly_rebuild(root, if_schema_changed = if_schema_changed)
}


## 5. list
usage_list <- "Usage:
  orderly list [names|drafts|archive]"

cli_args_process_list_type <- function(options) {
  if (options$archive) {
    "archive"
  } else if (options$drafts) {
    "drafts"
  } else {
    "names"
  }
}


main_do_list <- function(x) {
  root <- x$options$root
  switch(x$options$type,
         names = writeLines(orderly_list(root)),
         drafts = print(orderly_list_drafts(root)),
         archive = print(orderly_list_archive(root)))
  invisible(NULL)
}

## 6. latest
usage_latest <- "Usage:
  orderly latest [options] <name>...
Options:
  --draft                   Look for latest draft report
  --value-if-missing=VALUE  Value to print if missing"


main_do_latest <- function(x) {
  root <- x$options$root
  names <- x$options$name
  draft <- x$options$draft
  value_if_missing <- x$options$value_if_missing
  must_work <- is.null(value_if_missing)
  ids <- vcapply(names, orderly_latest, root = root, locate = TRUE,
                 draft = draft, must_work = must_work,
                 USE.NAMES = FALSE)
  ids[is.na(ids)] <- value_if_missing
  cat(paste0(ids, "\n", collapse = ""))
}


usage_pull <- "Usage:
  orderly pull [options] <name>...
Options:
  --dependencies  Pull *dependencies* of the report, not the report itself
  --id=ID         The id to use when pulling a single report (default: latest)
  --remote=NAME   Name of the remote to pull from"


main_do_pull <- function(x) {
  root <- x$options$root
  name <- x$options$name
  id <- x$options$id
  remote <- x$options$remote
  dependencies <- x$options$dependencies

  if (dependencies) {
    if (!is.null(id)) {
      stop("Do not provide --id with --dependencies", call. = FALSE)
    }
    orderly_pull_dependencies(name, remote = remote,
                              root = root, locate = TRUE)
  } else {
    orderly_pull_archive(name, id, remote = remote,
                         root = root, locate = TRUE)
  }
}


## 8. migrate
usage_migrate <- "Usage:
  orderly migrate [options]

Options:
  --to=VERSION  Version to migrate to
  --dry-run     Test run the migration only
  --clean       Clean migration backup files"


main_do_migrate <- function(x) {
  root <- x$options$root
  dry_run <- x$options$dry_run
  to <- x$options$to
  clean <- x$options$clean
  orderly_migrate(root, to = to, dry_run = dry_run, clean = clean)
}


## 8. batch
usage_batch <- "Usage:
  orderly batch [options] <name> ([<parameter>...]|--file=FILE)

Options:
  --file=FILE      File to read batch parameters from
  --instance=NAME  Database instance to use (if instances are configured)
  --print-log      Print the logs (rather than storing it)
  --ref=REF        Git reference (branch or sha) to use
  --fetch          Fetch git before updating reference
  --pull           Pull git before running report
  --message=TEXT   A message explaining why the reports were run

Parameters, if given, must be passed through as comma separated
key=value1,value2 pairs"

main_do_batch <- function(x) {
  ## TODO: Get some classed errors though here and then write out
  ## information about whether or not things worked and why they
  ## didn't.  Possible issues (in order)
  ##
  ## * orderly report not found
  ## * error while preparing (e.g., package not found)
  ## * error while running report
  ## * error checking artefacts
  config <- orderly_config_get(x$options$root, TRUE)
  name <- x$options$name
  instance <- x$options$instance
  parameters <- x$options$parameters
  print_log <- x$options$print_log
  ref <- x$options$ref
  fetch <- x$options$fetch
  pull <- x$options$pull
  message <- x$options$message

  if (print_log) {
    sink(stderr(), type = "output")
    on.exit(sink(NULL, type = "output"))
  } else {
    config$add_run_option("capture_log", TRUE)
  }

  if (pull) {
    if (is.null(ref)) {
      git_pull(config$root)
    } else {
      orderly_cli_error(
        "Can't use --pull with --ref; perhaps you meant --fetch ?")
    }
  }

  ids <- orderly_batch(name, parameters, root = config, instance = instance,
                    ref = ref, fetch = fetch, message = message)
  lapply(ids, function(id) {
    orderly_commit(id, name, config)
    path_rds <- path_orderly_run_rds(
      file.path(config$root, "archive", name, id))
    post_success(readRDS(path_rds), config)
  })

  message("ids:", paste(ids, collapse = ", "))
}

write_script <- function(path, versioned = FALSE) {
  if (!isTRUE(is_directory(path))) {
    stop("'path' must be a directory")
  }
  if (versioned) {
    rscript <- file.path(R.home(), "bin", "Rscript")
  } else {
    rscript <- "/usr/bin/env Rscript"
  }
  code <- c(sprintf("#!%s", rscript),
            readLines(orderly_file("script")))
  path_bin <- file.path(path, "orderly")
  writeLines(code, path_bin)
  Sys.chmod(path_bin, "755")
  invisible(path_bin)
}


cli_commands <- function() {
  list(run = list(name = "run a report",
                  usage = usage_run,
                  target = main_do_run),
       commit = list(name = "commit a report",
                     usage = usage_commit,
                     target = main_do_commit),
       list = list(name = "list reports",
                   usage = usage_list,
                   target = main_do_list),
       latest = list(name = "find most recent report",
                     usage = usage_latest,
                     target = main_do_latest),
       pull = list(name = "pull reports from remote servers",
                   usage = usage_pull,
                   target = main_do_pull),
       cleanup = list(name = "remove drafts and dangling data",
                      usage = usage_cleanup,
                      target = main_do_cleanup),
       rebuild = list(name = "rebuild the database",
                      usage = usage_rebuild,
                      target = main_do_rebuild),
       migrate = list(name = "migrate the archive",
                      usage = usage_migrate,
                      target = main_do_migrate),
       batch = list(name = "batch run reports",
                      usage = usage_batch,
                      target = main_do_batch))
}


parse_parameter <- function(x) {
  value <- tryCatch(parse(text = x)[[1]], error = function(e) x)
  if (is.logical(value) || is.numeric(value)) {
    value
  } else if (is.character(x) && grepl('^(".*"|\'.*\')$', x)) {
    substr(x, 2, nchar(x) - 1L)
  } else {
    x
  }
}

parse_batch_parameters <- function(x) {
  if (grepl("\\s", x)) {
    stop(sprintf(
      "Parameters with whitespace not supported in batch run, got param '%s'",
      x))
  }
  params <- strsplit(x, ",")[[1]]
  params <- lapply(params, parse_parameter)
  params
}

docopt_parse <- function(...) {
  tryCatch(
    docopt::docopt(...),
    error = function(e) {
      class(e) <- c("orderly_cli_error", class(e))
      stop(e)
    })
}


orderly_cli_error <- function(str) {
  err <- list(message = str)
  class(err) <- c("orderly_cli_error", "error", "condition")
  stop(err)
}
