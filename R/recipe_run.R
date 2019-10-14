##' Run a report.  This will create a new directory in
##' \code{drafts/<reportname>}, copy your declared resources there,
##' extract data from databases (if you are using them), run your
##' script and check that all expected artefacts were created.  Once
##' successfully run you can use \code{\link{orderly_commit}} to move
##' it to the \code{archive} directory.
##'
##' If \code{ref} is provided then before running a report orderly
##' will try to check out (as a detached \code{HEAD}) \code{ref},
##' interpreted as a git reference.  This can be a commit, tag, or a
##' branch name (including remote).  The working directory must be
##' clean according to \code{git status} and this \emph{will} require
##' some careful use of \code{.gitignore} to exclude \code{draft},
##' \code{archive}, \code{data} and \code{orderly.sqlite}.  The git
##' tree will revert back to the original branch at completion (or
##' failure to complete) the report.
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
##' @title Run a report
##'
##' @param name Name of the report to run (see
##'   \code{\link{orderly_list}}).
##'
##' @param parameters Parameters passed to the report. A named list of
##'   parameters declared in the \code{orderly.yml}.
##'
##' @param envir The parent of the environment that will be used to
##'   evaluate the report script; by default a new environment will be
##'   made with the global environment as the parent.
##'
##' @param ref A git reference to use for this run (see Details)
##'
##' @param fetch Logical, indicating if git should be fetched before
##'   checking out the reference \code{ref}.
##'
##' @param message An optional character string containing a message
##'   explaining why the report was run
##'
##' @inheritParams orderly_list
##' @param echo Print the result of running the R code to the console
##' @param id_file Write the identifier into a file
##'
##' @seealso \code{\link{orderly_log}} for controlling display of log
##'   messages (not just R output)
##'
##' @export
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
orderly_run <- function(name, parameters = NULL, envir = NULL,
                        root = NULL, locate = TRUE, echo = TRUE,
                        id_file = NULL, fetch = FALSE, ref = NULL,
                        message = NULL) {
  envir <- orderly_environment(envir)
  config <- orderly_config_get(root, locate)
  config <- check_orderly_archive_version(config)

  info <- recipe_prepare(config, name, id_file, ref, fetch, message)

  recipe_current_run_set(info)
  on.exit(recipe_current_run_clear())

  info <- recipe_run(info, parameters, envir, config, echo = echo)

  info$id
}


recipe_prepare <- function(config, name, id_file = NULL, ref = NULL,
                           fetch = FALSE, message = NULL) {
  assert_is(config, "orderly_config")
  config <- orderly_config_get(config, FALSE)

  orderly_log("name", name)
  if (!is.null(ref)) {
    if (fetch) {
      git_fetch(config$root)
    }
    prev <- git_detach_head_at_ref(ref, config$root)
    on.exit(git_checkout_branch(prev, TRUE, config$root))
  }

  info <- recipe_read(file.path(path_src(config$root), name), config)

  id <- new_report_id()
  orderly_log("id", id)
  if (!is.null(id_file)) {
    orderly_log("id_file", id_file)
    writelines_atomic(id, id_file)
  }

  info$id <- id
  info$workdir <- file.path(path_draft(config$root), info$name, id)
  info <- recipe_prepare_workdir(info, message, config)
  info$git <- git_info(info$path)

  info
}


recipe_run <- function(info, parameters, envir, config, echo = TRUE) {
  assert_is(config, "orderly_config")

  owd <- setwd(info$workdir)
  on.exit(setwd(owd))

  ## should these go later?
  con_rds <- orderly_db("rds", config, FALSE)
  con_csv <- orderly_db("csv", config, FALSE)

  prep <- orderly_prepare_data(config, info, parameters, envir)
  resource_info <- info$resource_info

  t0 <- Sys.time()
  orderly_log("start", as.character(t0))
  source(info$script, local = envir,
         echo = echo, max.deparse.length = Inf)
  t1 <- Sys.time()
  elapsed <- t1 - t0
  orderly_log("end", as.character(t1))
  orderly_log("elapsed", sprintf("Ran report in %s", format(elapsed)))

  if (!is.null(info$connection)) {
    tryCatch(lapply(prep$con, DBI::dbDisconnect),
             error = identity,
             warning = identity)
  }

  recipe_check_device_stack(prep$n_dev)
  recipe_check_sink_stack(prep$n_sink)
  recipe_check_connections(info)
  hash_artefacts <- recipe_check_artefacts(info)

  hash_data_csv <- con_csv$mset(prep$data)
  hash_data_rds <- con_rds$mset(prep$data)
  stopifnot(identical(hash_data_csv, hash_data_rds))

  ## Ensure that inputs were not modified when the report was run:
  recipe_check_file_inputs(info)
  recipe_check_depends(info)

  if (is.null(info$depends)) {
    depends <- NULL
  } else {
    depends <- info$depends
    depends <- depends[c("name", "id", "filename", "as", "hash",
                         "id_requested", "is_latest", "is_pinned")]
  }

  extra_fields <- drop_null(set_names(
    lapply(config$fields$name, function(x) info[[x]]),
    config$fields$name))
  if (length(extra_fields) > 0L) {
    extra_fields <- as_data_frame(extra_fields)
  } else {
    extra_fields <- NULL
  }

  session <- session_info()
  session$git <- info$git

  artefacts <- data_frame(
    format = list_to_character(info$artefacts[, "format"], FALSE),
    description = list_to_character(info$artefacts[, "description"], FALSE),
    order = seq_len(nrow(info$artefacts)))

  n <- lengths(info$artefacts[, "filenames"])
  file_info_artefacts <- data_frame(
    order = rep(seq_along(n), n),
    filename = names(hash_artefacts),
    file_hash = unname(hash_artefacts),
    file_size = file_size(names(hash_artefacts)))

  meta <- list(id = info$id,
               name = info$name,
               parameters = prep$parameters,
               date = as.character(Sys.time()),
               displayname = info$displayname %||% NA_character_,
               description = info$description %||% NA_character_,
               extra_fields = extra_fields,
               connection = !is.null(info$connection),
               packages = info$packages,
               file_info_inputs = info$inputs,
               file_info_artefacts = file_info_artefacts,
               global_resources = info$global_resources,
               artefacts = artefacts,
               depends = depends,
               elapsed = as.numeric(elapsed, "secs"),
               changelog = info$changelog,
               git = info$git)

  ## All the information about data - it's a little more complicated
  ## than the other types of inputs because there are *two* sizes at
  ## present.  We should probably drop the csv one tbh and render to
  ## csv as required?
  if (!is.null(info$data)) {
    meta$data <- data_frame(
      name = names(info$data),
      database = vcapply(info$data, "[[", "database", USE.NAMES = FALSE),
      query = vcapply(info$data, "[[", "query", USE.NAMES = FALSE),
      hash = unname(hash_data_rds),
      size_csv = file_size(orderly_db("csv", config)$filename(hash_data_rds)),
      size_rds = file_size(orderly_db("rds", config)$filename(hash_data_csv)))
  }

  if (!is.null(info$views)) {
    meta$view <- data_frame(
      name = names(info$views),
      database = vcapply(info$views, "[[", "database", USE.NAMES = FALSE),
      query = vcapply(info$views, "[[", "query", USE.NAMES = FALSE))
  }

  session$meta <- meta
  session$archive_version <- cache$current_archive_version

  saveRDS(session, path_orderly_run_rds(info$workdir))

  meta
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

recipe_data <- function(config, info, parameters, dest) {
  assert_is(config, "orderly_config")
  if (!is.environment(dest)) {
    stop("Invalid input for 'dest'")
  }

  parameters <- recipe_parameters(info, parameters)
  if (!is.null(parameters)) {
    list2env(parameters, dest)
    info <- recipe_substitute(info, parameters)
  }

  ret <- list(dest = dest, parameters = parameters)

  if (length(info$data) == 0 && is.null(info$connection)) {
    return(ret)
  }

  con <- orderly_db("source", config)
  on.exit(lapply(con, DBI::dbDisconnect))

  views <- info$views
  for (v in names(views)) {
    orderly_log("view", sprintf("%s : %s", views[[v]]$database, v))
    sql <- temporary_view(v, views[[v]]$query)
    DBI::dbExecute(con[[views[[v]]$database]], sql)
  }

  data <- list()
  for (v in names(info$data)) {
    database <- info$data[[v]]$database
    query <- info$data[[v]]$query
    withCallingHandlers(
      data[[v]] <- dest[[v]] <- DBI::dbGetQuery(con[[database]], query),
      error = function(e)
        orderly_log("data", sprintf("%s => %s: <error>", database, v)))
    orderly_log("data",
                sprintf("%s => %s: %s x %s",
                        database, v, nrow(dest[[v]]), ncol(dest[[v]])))
  }
  if (length(data) > 0L) {
    ret$data <- data
  }

  if (!is.null(info$connection)) {
    for (i in names(info$connection)) {
      dest[[i]] <- con[[info$connection[[i]]]]
    }
    ## Ensure that only unexported connections are closed:
    con <- con[setdiff(list_to_character(info$connection, FALSE),
                       names(config$database))]
  }

  ret
}

recipe_prepare_workdir <- function(info, message, config) {
  if (file.exists(info$workdir)) {
    stop("'workdir' must not exist")
  }
  src <- normalizePath(info$path, mustWork = TRUE)
  dir_create(info$workdir)
  info$owd <- setwd(info$workdir)
  on.exit(setwd(info$owd))

  ## TODO: this supports a script in a subdirectory, but I don't think
  ## that is supported yet, and it's not clear it's a desirable thing
  ## because that makes working directories a little less clear.
  dir.create(dirname(info$script), FALSE, TRUE)
  file_copy(file.path(src, info$script), info$script)
  file_copy(file.path(src, "orderly.yml"), "orderly.yml")

  info <- recipe_copy_readme(info, src)
  info <- recipe_copy_sources(info, src)
  info <- recipe_copy_resources(info, src)
  info <- recipe_copy_global(info, config)
  info <- recipe_copy_depends(info)

  info$inputs <- recipe_file_inputs(info)
  info$changelog <- changelog_load(src, message, info, config)

  recipe_check_unique_inputs(info)
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
  resources <- info$inputs$filename
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
      sink(NULL)
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


orderly_prepare_data <- function(config, info, parameters, envir) {
  res <- recipe_data(config, info, parameters, envir)

  ## Compute the device stack size before starting work too
  n_dev <- length(grDevices::dev.list())
  n_sink <- sink.number()

  missing_packages <- setdiff(info$packages, .packages(TRUE))
  if (length(missing_packages) > 0) {
    handle_missing_packages(missing_packages)
  }

  ret <- list(data = res$data, parameters = res$parameters,
              n_dev = n_dev, n_sink = n_sink)

  if (!is.null(info$connection)) {
    ## NOTE: this is a copy of exported connections so that we can
    ## close them once the report finishes running.
    con <- list_to_character(info$connection)
    ret$con <- lapply(names(con)[!duplicated(con)], function(nm) res$data[[nm]])
  }

  for (p in info$packages) {
    library(p, character.only = TRUE)
  }
  for (s in info$sources) {
    source(s, envir)
  }

  ret
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
##' and subject to change.  It is designed to be used either within
##' report code.  To use in conjunction with
##' \code{\link{orderly_test_start}}, you must pass in the path to the
##' report in question.
##'
##' @title Information on current orderly run
##'
##' @param path Path to the report currently being run.  This should
##'   be left as \code{NULL} when running a report, and the path to
##'   the report being run should be used when using
##'   \code{\link{orderly_test_start}}
##'
##' @export
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
    readme = file_info(info$readme),
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


recipe_copy_readme <- function(info, src) {
  ## README logic:
  ## * if there's a readme we copy it
  ## * any casing is OK as input, but we always store in canonical case
  ## * if they also list it as a resource let them know that's redundant
  ## * if they also list it as an artefact then error
  src_files <- dir(src)
  readme_file <- src_files[tolower(src_files) == "readme.md"]

  readme_files <- dir(src, pattern = "README(\\.md)?$",
                      ignore.case = TRUE, recursive = TRUE)
  ## Two readme files e.g. README.md and Readme.MD can happen on unix
  ## systems; it is not clear what we should do here, so we just
  ## ignore the readme silently for now.
  if (length(readme_files) > 0) {
    ## we copy the readme.md file to README.md irrespective of what
    ## case filename the user has used
    dir_create(dirname(readme_files))
    canonical_readme_files <- sub("README(|.md)$", "README\\1", readme_files,
                                  ignore.case = TRUE)
    file_copy(file.path(src, readme_files), canonical_readme_files)
    info$readme <- canonical_readme_files

    ## now check if README is a resource
    if (length(info$resources) > 0) {
      i <- grepl("README(|.md)$", info$resources, ignore.case = TRUE)
      if (any(i)) {
        ## WARNING
        orderly_log("readme", "README.md should not be listed as a resource")
        info$resources <- info$resources[!i]
      }
    }

    ## now check if README is an artefact
    artefact_files <- unlist(info$artefacts[, "filenames"], use.names = FALSE)
    if (any(grepl("README(|.md)$", artefact_files, ignore.case = TRUE))) {
      stop("README.md should not be listed as an artefact")
    }
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

recipe_copy_depends <- function(info) {
  if (!is.null(info$depends)) {
    dep_src <- file.path(info$depends$path, info$depends$filename)
    dep_dst <- file.path(info$workdir, info$depends$as)
    info$depends$id_requested <- info$depends$id
    info$depends$id <- basename(info$depends$path)

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
