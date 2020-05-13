##' Run a report.  This will create a new directory in
##' \code{drafts/<reportname>}, copy your declared resources there,
##' extract data from databases (if you are using them), run your
##' script and check that all expected artefacts were created.  Once
##' successfully run you can use \code{\link{orderly_commit}} to move
##' it to the \code{archive} directory.
##'
##' Parameters are passed to the report as a named list, for example
##'
##' \code{
##' id <- orderly::orderly_run("other", list(nmin = 0.2), root = path)
##' }
##'
##' (see the examples).  The names of the parameters (here,
##' \code{nmin}) must correspond to declared parameters in the
##' \code{orderly.yml}.  It is an error if parameters without a
##' default are omitted, and it is an error if unknown parameters are
##' provided.
##'
##' Environment variables that are created in \code{orderly_envir.yml}
##' will be available while the report runs.  Those that begin with
##' \code{ORDERLY_} will be saved into the \code{orderly_run.rds}
##' within the \code{$env} section (except for any that match the
##' patterns "TOKEN", "PAT" or "PASS").
##'
##' @title Run a report
##'
##' @param name Name of the report to run (see
##'   \code{\link{orderly_list}}).  A leading \code{src/} will be
##'   removed if provided, allowing easier use of autocomplete.
##'   Alternatively, the default of \code{NULL} is useful if you have
##'   already set the working directory to be the source directory.
##'
##' @param parameters Parameters passed to the report. A named list of
##'   parameters declared in the \code{orderly.yml}.  Each parameter
##'   must be a scalar character, numeric, integer or logical.
##'
##' @param envir The parent of the environment that will be used to
##'   evaluate the report script; by default a new environment will be
##'   made with the global environment as the parent.
##'
##' @param message An optional character string containing a message
##'   explaining why the report was run
##'
##' @param instance Select instance of the source database to be used,
##'   where multiple instances are configured.  Use a single
##'   \emph{unnamed} character string to indicate an instance to
##'   match.  If given, then this name must be present in all
##'   databases where instances are listed in
##'   \code{orderly_config.yml}, and will be ignored by all database
##'   where instances are not given.  See the "orderly" vignette for
##'   further information.
##'
##' @inheritParams orderly_list
##'
##' @param echo Print the result of running the R code to the console
##'
##' @param remote Remote to use to resolve dependencies.  Use this in
##'   order to run a report with the same dependencies as are
##'   available on a remote server, particularly when using \code{id =
##'   "latest"}.  Note that this is not the same as running
##'   \code{\link{orderly_pull_dependencies}}, then \code{orderly_run}
##'   with \code{remote = NULL}, as the pull/run approach will use the
##'   latest report in \emph{your} archive but the \code{remote =
##'   "remote"} approach will use the latest approach in the
##'   \emph{remote} archive (which might be less recent).
##'
##' @param tags Character vector of tags to add to the report.  Tags
##'   are immutable and cannot be removed once the report is run.
##'   Tags added here will be \emph{in addition} to any tags listed in
##'   the \code{tags:} field in \code{orderly.yml} and must be present
##'   in \code{orderly_config.yml}.
##'
##' @seealso \code{\link{orderly_log}} for controlling display of log
##'   messages (not just R output)
##'
##' @export
##' @return The id of the newly created report
##' @examples
##' path <- orderly::orderly_example("demo")
##'
##' # To run most reports, provide the report name (and the path if
##' # not running in the working directory, as is the case here):
##' id <- orderly::orderly_run("minimal", root = path)
##'
##' # Every report gets a unique identifier, based on the time (it is
##' # ISO 8601 time with random hex appended to end)
##' id
##'
##' # After being run, a report is a "draft" and will exist in the
##' # drafts directory:
##' orderly::orderly_list_drafts(root = path)
##'
##' # Draft reports are always stored in the path
##' # <root>/draft/<name>/<id>, so we have
##' dir(file.path(path, "draft", "minimal", id))
##'
##' # which contains the files when the report was run.
##'
##' # If a report has parameters, then these must be passed in as a
##' # named list.
##' id <- orderly::orderly_run("other", list(nmin = 0.2), root = path)
##'
##' # These parameters can be used in SQL queries or in the report
##' # code.
orderly_run <- function(name = NULL, parameters = NULL, envir = NULL,
                        root = NULL, locate = TRUE, echo = TRUE,
                        message = NULL, instance = NULL, use_draft = FALSE,
                        remote = NULL, tags = NULL) {
  version <- orderly_version$new(name, root, locate)
  version$run(parameters, instance, envir, message, tags, echo,
              use_draft, remote)
  version$id
}


orderly_run2 <- function(name = NULL, parameters = NULL, envir = NULL,
                        root = NULL, locate = TRUE, echo = TRUE,
                        message = NULL, instance = NULL, use_draft = FALSE,
                        remote = NULL, tags = NULL,
                        # specific to run2
                        id_file = NULL, batch_id = NULL,
                        fetch = FALSE, ref = NULL,
                        capture_log = NULL, commit = FALSE) {
  version <- orderly_version$new(name, root, locate)
  capture_log <- capture_log %||%
    version$config$get_run_option("capture_log") %||% FALSE
  version$run2(parameters, instance, envir, message, tags, echo,
              use_draft, remote,
              id_file, batch_id, ref, fetch, capture_log)

  ## TODO: Tidy this mess up:
  if (commit) {
    logfile <- file.path(path_draft(version$config$root),
                         version$name, version$id, "orderly.log")
    conditional_capture_log(
      capture_log, logfile,
      orderly_commit(version$id, root = version$config))
    path_rds <- path_orderly_run_rds(
      file.path(version$config$root, "archive", version$name, version$id))
    post_success(readRDS(path_rds), version$config)
  }
  version$id
}


recipe_parameters <- function(info, parameters) {
  if (!is.null(parameters)) {
    assert_named(parameters, unique = TRUE)
  }

  has_default <- names(info$parameters)[vlapply(info$parameters, function(x)
    "default" %in% names(x))]
  msg <- setdiff(setdiff(names(info$parameters), names(parameters)),
                 has_default)
  if (length(msg) > 0L) {
    stop("Missing parameters: ", pasteq(msg))
  }
  extra <- setdiff(names(parameters), names(info$parameters))
  if (length(extra) > 0L) {
    stop("Extra parameters: ", pasteq(extra))
  }

  use_default <- setdiff(has_default, names(parameters))
  if (length(use_default) > 0L) {
    parameters[use_default] <-
      lapply(info$parameters[use_default], "[[", "default")
  }

  ## This somewhat duplicates the checks in db2.R but designed to give
  ## more sensible errors back to the user.
  nonscalar <- lengths(parameters) != 1
  if (any(nonscalar)) {
    stop(sprintf(
      "Invalid parameters: %s - must be scalar",
      pasteq(names(nonscalar[nonscalar]))))
  }

  err <- !vlapply(parameters, function(x)
    is.character(x) || is.numeric(x) || is.logical(x))
  if (any(err)) {
    stop(sprintf(
      "Invalid parameters: %s - must be character, numeric or logical",
      pasteq(names(err[err]))))
  }

  parameters
}


recipe_substitute <- function(info, parameters) {
  if (length(parameters) > 0L) {
    info$views <- sql_str_sub(info$views, parameters)
    info$data <- sql_str_sub(info$data, parameters)
    orderly_log("parameter", sprintf("%s: %s", names(parameters), parameters))
  }

  info
}


recipe_check_artefacts <- function(info) {
  found <- recipe_exists_artefacts(info)
  artefacts <- names(found)
  if (!all(found)) {
    stop("Script did not produce expected artefacts: ",
         paste(artefacts[!found], collapse = ", "))
  }
  unexpected <- recipe_unexpected_artefacts(info, NULL)
  if (length(unexpected) != 0) {
    orderly_log("unexpected", sprintf("%s", unexpected))
  }

  h <- hash_artefacts(artefacts)
  orderly_log("artefact", sprintf("%s: %s", artefacts, h))
  h
}

hash_artefacts <- function(artefacts) {
  i <- is_directory(artefacts)
  i[is.na(i)] <- FALSE # for test mode
  if (any(i)) {
    stop("Produced a directory artefact: ",
         paste(squote(artefacts[i]), collapse = ", "),
         call. = FALSE)
  }
  hash_files(artefacts)
}

recipe_exists_artefacts <- function(info, id) {
  expected <- unlist(info$artefacts[, "filenames"], use.names = FALSE)
  exists <- file.exists(expected)
  names(exists) <- expected
  exists
}

recipe_unexpected_artefacts <- function(info, id) {
  artefacts <- unlist(info$artefacts[, "filenames"], use.names = FALSE)
  if (is.function(info$inputs)) {
    resources <- info$inputs(check = FALSE)$filename
  } else {
    resources <- info$inputs$filename
  }
  dependencies <- info$depends$as
  expected <- c(artefacts, resources, dependencies)

  ## this is set to recursive to ensure that artefacts created in directories
  ## are tracked
  found <- list.files(recursive = TRUE)
  ## TODO do we need to track when a user unexpectedly creates an
  ## empty directory ?

  setdiff(found, expected)
}

iso_time_str <- function(time = Sys.time()) {
  strftime(time, "%Y%m%d-%H%M%S")
}

new_report_id <- function(time = Sys.time()) {
  sprintf("%s-%s%s",
          iso_time_str(time),
          val_to_bytes(as.numeric(time), 2),
          ids::random_id(bytes = 2))
}

temporary_view <- function(name, sql) {
  sprintf("CREATE TEMPORARY VIEW %s AS\n%s", name, sql)
}

recipe_check_device_stack <- function(expected) {
  check <- length(grDevices::dev.list()) - expected
  if (check == 0) {
    return()
  } else if (check > 0) {
    for (i in seq_len(check)) {
      grDevices::dev.off()
    }
    stop(ngettext(check,
                  "Report left 1 device open",
                  sprintf("Report left %d devices open", check)))
  } else {
    stop(sprintf("Report closed %d more devices than it opened!", abs(check)))
  }
}

recipe_check_sink_stack <- function(expected) {
  check <- sink.number() - expected
  if (check == 0) {
    return()
  } else if (check > 0) {
    for (i in seq_len(check)) {
      sink(NULL) # nolint
    }
    stop(ngettext(check,
                  "Report left 1 sink open",
                  sprintf("Report left %d sinks open", check)))
  } else {
    stop(sprintf("Report closed %d more sinks than it opened!", abs(check)))
  }
}

orderly_environment <- function(envir) {
  if (is.null(envir)) {
    new.env(parent = .GlobalEnv)
  } else if (is.environment(envir)) {
    envir
  } else {
    stop("'envir' must be an environment")
  }
}


recipe_current_run_set <- function(info) {
  cache$current <- info
}


recipe_current_run_get <- function(path) {
  if (is.null(path)) {
    cache$current
  } else {
    assert_is_directory(path)
    cache$test[[normalizePath(path)]]
  }
}


recipe_current_run_clear <- function() {
  cache$current <- NULL
}


##' This function allows inspection of some of orderly's metadata
##' during an orderly run.  The format returned is internal to orderly
##' and subject to change.  It is designed to be used within report
##' code.  To use in conjunction with
##' \code{\link{orderly_test_start}}, you must pass in the path to the
##' report in question.
##'
##' @section Warning:
##'
##' It is important that this data is treated as \emph{readonly}!
##'
##' @title Information on current orderly run
##'
##' @param path Path to the report currently being run.  This should
##'   be left as \code{NULL} when running a report, and the path to
##'   the report being run should be used when using
##'   \code{\link{orderly_test_start}}
##'
##' @export
##' @return A list of metadata about the current report
##' @examples
##' path <- orderly::orderly_example("demo")
##'
##' # This example uses orderly_run_info within its script, saving the
##' # output to "output.rds"
##' readLines(file.path(path, "src", "use_dependency", "script.R"))
##'
##' # Run the dependency:
##' id <- orderly::orderly_run("other", list(nmin = 0), root = path)
##' orderly::orderly_commit(id, root = path)
##'
##' # Then the report
##' id <- orderly::orderly_run("use_dependency", root = path)
##'
##' # This is the contents:
##' readRDS(file.path(path, "draft", "use_dependency", id, "info.rds"))
orderly_run_info <- function(path = NULL) {
  info <- recipe_current_run_get(path)
  if (is.null(info)) {
    stop("Not currently running an orderly report")
  }
  info
}


recipe_file_inputs <- function(info) {
  file_in_data(
    orderly_yml = file_info("orderly.yml"),
    script = file_info(info$script),
    readme = file_info(names(info$readme)),
    source = file_info(info$sources),
    resource = file_info(info$resources),
    global = file_info(names(info$global_resources)))
}


recipe_check_file_inputs <- function(info) {
  pre <- info$inputs
  post <- recipe_file_inputs(info)
  recipe_check_hashes(pre, post, "input", "inputs")
}


recipe_check_depends <- function(info) {
  pre <- info$depends
  if (!is.null(info$depends)) {
    pre <- data_frame(filename = info$depends$as,
                      file_hash = info$depends$hash)
    post <- file_info(info$depends$as)[c("filename", "file_hash")]
    recipe_check_hashes(pre, post, "dependency", "dependencies")
  }
}


recipe_check_hashes <- function(pre, post, name1, name2) {
  if (identical(pre, post)) {
    return(NULL)
  }

  missing <- post$filename[is.na(post$file_hash)]
  if (length(missing) > 0L) {
    stop(sprintf("Script deleted %s: %s",
                 ngettext(length(missing), name1, name2),
                 paste(missing, collapse = ", ")),
         call. = FALSE)
  }

  changed <- pre$filename[!(pre$file_hash %in% post$file_hash)]
  if (length(changed) > 0L) {
    stop(sprintf("Script has modified %s: %s",
                 ngettext(length(changed), name1, name2),
                 paste(changed, collapse = ", ")),
         call. = FALSE)
  }
}


recipe_copy_script <- function(info, src) {
  dir_create(dirname(info$script))
  file_copy(file.path(src, info$script), info$script)
  info
}


recipe_copy_readme <- function(info, src) {
  if (!is.null(info$readme)) {
    dir_create(dirname(info$readme))
    file_copy(file.path(src, info$readme), names(info$readme))
  }
  info
}


## TODO (VIMC-20??): we need to organise where these files are coming
## from in the read section, so if a source is actually global we
## don't copy it here - but this should be done as a check during
## read.  We also need to ensure that source files are not listed as
## resources too, I think.
recipe_copy_sources <- function(info, src) {
  if (length(info$sources) > 0L) {
    dir_create(dirname(info$sources))
    orderly_log("sources", info$sources)
    file_copy(file.path(src, info$sources), info$sources)
  }
  info
}


## TODO: move this into the read!
recipe_copy_resources <- function(info, src) {
  if (length(info$resources) > 0L) {
    resources <- info$resources
    i <- is_directory(file.path(src, resources))
    if (any(i)) {
      resources <- as.list(resources)
      resources[i] <- lapply(resources[i], function(p)
        file.path(p, dir(file.path(src, p),
                         recursive = TRUE, all.files = TRUE)))
      resources <- unlist(resources)
    }
    info$resources <- resources

    dir_create(dirname(info$resources))
    orderly_log("resource", info$resources)
    file_copy(file.path(src, info$resources), info$resources)
  }
  info
}


recipe_copy_global <- function(info, config) {
  if (!is.null(info$global_resources)) {
    global_path <- file.path(config$root, config$global_resources)
    src <- file.path(global_path, info$global_resources)
    dest <- names(info$global_resources)
    dir_create(dirname(dest))
    file_copy(src, dest)
    orderly_log("global",
                sprintf("%s -> %s",
                        info$global_resources, names(info$global_resources)))
  }
  info
}


## TODO: return something else here
recipe_copy_depends <- function(info) {
  if (!is.null(info$depends)) {
    dep_src <- file.path(info$depends$path, info$depends$filename)
    dep_dst <- info$depends$as
    str <- sprintf("%s@%s:%s -> %s",
                   info$depends$name,
                   info$depends$id,
                   info$depends$filename,
                   info$depends$as)
    orderly_log("depends", str)
    dir_create(dirname(dep_dst))
    file_copy(dep_src, dep_dst)
  }
  info
}


recipe_check_unique_inputs <- function(info) {
  tmp <- rbind(
    info$inputs[c("filename", "file_purpose")],
    data_frame(filename = info$depends$as,
               file_purpose = rep("depends", NROW(info$depends))))
  err <- tmp[tmp$filename %in% tmp$filename[duplicated(tmp$filename)], ]
  if (nrow(err) > 0L) {
    err <- split(err$file_purpose, err$filename)
    details <- sprintf("\n  - %s: %s",
                       names(err), vcapply(err, paste, collapse = ", "))
    stop(sprintf("Orderly configuration implies duplicate files:%s",
                 paste(details, collapse = "")),
         call. = FALSE)
  }
}


recipe_check_connections <- function(info) {
  cons <- getAllConnections()
  cons <- cons[cons > 2] # drop stdin, stdout, stderr
  if (length(cons) > 0L) {
    open <- basename(vcapply(cons, function(x)
      summary.connection(x)$description))
    ours <- unlist(info$artefacts[, "filenames"], FALSE, FALSE)
    err <- ours[basename(ours) %in% open]
    if (any(ours %in% open)) {
      stop("File left open: ", paste(err, collapse = ", "))
    }
  }
}
