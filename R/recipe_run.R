##' Run a report.  The \code{orderly_data} function is for testing the
##' queries (and developing the report).
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
##' @title Run a report
##'
##' @param name Name of the report to run (see
##'   \code{\link{orderly_list}}).
##'
##' @param parameters Parameters passed to the report. A named list of
##'   parameters declared in the orderly.yml.
##'
##' @param envir The parent of environment to evaluate the report in;
##'   by default a new environment will be made with the global
##'   environment as the parent.
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
  on.exit(recipe_current_run_clear())

  info <- recipe_run(info, parameters, envir, config, echo = echo)

  info$id
}

##' @export
##' @rdname orderly_run
##' @examples
##' # The function orderly_data does all the preparation work that
##' # orderly_run does, but does not run the report; instead it
##' # returns the created environment with all the data and parameters
##' # set.
##' env <- orderly::orderly_data("other", list(nmin = 0.2), root = path)
##' ls(env)
##' env$nmin
##' env$extract
orderly_data <- function(name, parameters = NULL, envir = NULL,
                         root = NULL, locate = TRUE) {
  config <- orderly_config_get(root, locate)
  info <- recipe_read(file.path(path_src(config$root), name), config)
  envir <- orderly_environment(envir)
  recipe_data(config, info, parameters, envir)
}


##' For interactive testing of orderly code.  This runs through and
##' sets everything up as orderly would (creates a new working
##' directory and changes into it, pulls data from the database,
##' copies over any dependent reports) but then rather than running
##' the report hands back to the user.  The prompt \emph{looks} like
##' \code{\link{browser}} but it is just a plain old R prompt and the
##' code runs in the global environment.
##'
##' To quit run \code{orderly_test_end()} (or enter \code{Q}, like
##' \code{browser}).  To test if all artefacts have been created run
##' \code{orderly_test_check()}.
##'
##' @title Prepare a directory for orderly to use
##' @inheritParams orderly_run
##' @export
##' @examples
##'
##' path <- orderly::orderly_example("minimal")
##' orderly::orderly_test_start("example", root = path)
##'
##' # R is now running from the newly created draft directory for this
##' # report:
##' getwd()
##'
##' # The data in the orderly example is now available to use
##' dat
##'
##' # Check to see which artefacts have been created so far:
##' orderly::orderly_test_check()
##'
##' # Manually the code that this report has in its script
##' png("mygraph.png")
##' par(mar = c(15, 4, .5, .5))
##' barplot(setNames(dat$number, dat$name), las = 2)
##' dev.off()
##'
##' orderly::orderly_test_check()
##'
##' # Revert back to the original directory:
##' orderly::orderly_test_end()
orderly_test_start <- function(name, parameters = NULL, envir = .GlobalEnv,
                               root = NULL, locate = TRUE) {
  if (!is.null(cache$test)) {
    stop("Already running in test mode")
  }

  config <- orderly_config_get(root, locate)
  info <- recipe_prepare(config, name, id_file = NULL, ref = NULL,
                         fetch = FALSE, message = NULL)
  owd <- setwd(info$workdir)
  ## Ensure that if anything goes wrong we'll end up back where we
  ## started (see VIMC-1870)
  on.exit(setwd(owd))
  prep <- orderly_prepare_data(config, info, parameters, envir)

  cache$test <- list(owd = owd,
                     name = name,
                     id = info$id,
                     parameters = parameters,
                     config = config,
                     info = info,
                     prompt = getOption("prompt"))
  options(prompt = "[orderly test] > ")
  makeActiveBinding(quote("Q"), test_mode_end, .GlobalEnv)
  orderly_log("setwd", "running in test draft directory")
  on.exit()
}

##' @export
##' @rdname orderly_test_start
##'
##' @param cleanup Delete testing directory on exit?  If \code{FALSE}
##'   then you will probably want to use \code{\link{orderly_cleanup}}
##'   later to delete the test directory.  Note that it is not
##'   possible to commit the results of an orderly test run
orderly_test_end <- function(cleanup = FALSE) {
  if (is.null(cache$test)) {
    stop("Not running in test mode")
  }
  testdir <- setwd(cache$test$owd)
  orderly_log("setwd", "reverting to original directory")
  options(prompt = cache$test$prompt)
  cache$test <- NULL
  recipe_current_run_clear()
  if (cleanup) {
    unlink(testdir, recursive = TRUE)
  }
}

##' @export
##' @rdname orderly_test_start
orderly_test_restart <- function(cleanup = TRUE) {
  if (is.null(cache$test)) {
    stop("Not running in test mode")
  }
  name <- cache$test$name
  parameters <- cache$test$parameters
  config <- cache$test$config
  orderly_test_end(cleanup)
  orderly_test_start(name, parameters, root = config)
}

##' @export
##' @rdname orderly_test_start
orderly_test_check <- function() {
  if (is.null(cache$test)) {
    stop("Not running in test mode")
  }
  found <- recipe_exists_artefacts(cache$test$info)
  msg <- sprintf("%7s: %s", ifelse(found, "found", "missing"), names(found))
  artefacts <- names(found)
  h <- hash_artefacts(artefacts)
  h[is.na(h)] <- "<missing>"
  orderly_log("artefact", sprintf("%s: %s", artefacts, h))
  invisible(all(found))
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

  recipe_current_run_set(info)

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
               parameters = parameters,
               date = as.character(Sys.time()),
               displayname = info$displayname %||% NA_character_,
               description = info$description %||% NA_character_,
               extra_fields = extra_fields,
               connection = !is.null(info$connection),
               packages = info$packages,
               file_info_inputs = info$inputs,
               file_info_artefacts = file_info_artefacts,
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


recipe_substitute <- function(info, parameters) {
  if (!is.null(parameters)) {
    assert_named(parameters, unique = TRUE)
  }
  msg <- setdiff(info$parameters, names(parameters))
  if (length(msg) > 0L) {
    stop("Missing parameters: ", pasteq(msg))
  }
  extra <- setdiff(names(parameters), info$parameters)
  if (length(extra) > 0L) {
    stop("Extra parameters: ", pasteq(extra))
  }
  if (length(parameters) > 0L) {
    info$views <- sql_str_sub(info$views, parameters)
    info$data <- sql_str_sub(info$data, parameters)
    orderly_log("parameter", sprintf("%s: %s", names(parameters), parameters))
  }
  info$hash_parameters <- digest::digest(parameters)
  info
}

recipe_data <- function(config, info, parameters, dest) {
  assert_is(config, "orderly_config")
  if (!is.environment(dest)) {
    stop("Invalid input for 'dest'")
  }

  info <- recipe_substitute(info, parameters)
  if (!is.null(parameters)) {
    list2env(parameters, dest)
  }

  if (length(info$data) == 0 && is.null(info$connection)) {
    return(dest)
  }

  con <- orderly_db("source", config)
  on.exit(lapply(con, DBI::dbDisconnect))

  views <- info$views
  for (v in names(views)) {
    orderly_log("view", sprintf("%s : %s", views[[v]]$database, v))
    sql <- temporary_view(v, views[[v]]$query)
    DBI::dbExecute(con[[views[[v]]$database]], sql)
  }

  for (v in names(info$data)) {
    database <- info$data[[v]]$database
    query <- info$data[[v]]$query
    withCallingHandlers(
      dest[[v]] <- DBI::dbGetQuery(con[[database]], query),
      error = function(e)
        orderly_log("data", sprintf("%s => %s: <error>", database, v)))
    orderly_log("data",
                sprintf("%s => %s: %s x %s",
                        database, v, nrow(dest[[v]]), ncol(dest[[v]])))
  }

  if (!is.null(info$connection)) {
    for (i in names(info$connection)) {
      dest[[i]] <- con[[info$connection[[i]]]]
    }
    ## Ensure that only unexported connections are closed:
    con <- con[setdiff(list_to_character(info$connection, FALSE),
                       names(config$database))]
  }

  dest
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
  ## TODO: filter out globals
  # expected artefacts
  expected <- unlist(info$artefacts[, "filenames"], use.names = FALSE)
  # expected resources
  resources <- c()
  if (!is.null(info$resources)) {
    resources <- info$resources
  }
  # expected dependencies
  dependencies <- c()
  if (!is.null(info$depends)) {
    dependencies <- info$depends$as
  }
  ## we expect to see all artefacts from the config, the source file
  ## and the yml config; the changelog may or may not be present, but
  ## it's never unexpected.
  expected <- c(expected, resources, dependencies, info$script, "orderly.yml")

  # this is set to recursive to ensure that artefacts created in directories
  # are tracked
  found <- list.files(recursive = TRUE)
  # TODO do we need to track when a user unexpectedly creates an empty directory ?

  # what files have we found that were not contained in expected
  unexpected <- setdiff(found, expected)

  # remove any files of the form readme or readme.md
  unexpected <- unexpected[!grepl("^readme(|.md)$", unexpected,
                                  ignore.case = TRUE)]

  unexpected
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
  ## Because the script (including the files in sources) might modify
  ## the data we need to make sure that we grab a copy of it now (as a
  ## list 'ldata') to pass back
  data <- recipe_data(config, info, parameters, envir)
  ldata <- as.list(data)[names(info$data)]

  ## Compute the device stack size before starting work too
  n_dev <- length(grDevices::dev.list())

  missing_packages <- setdiff(info$packages, .packages(TRUE))
  if (length(missing_packages) > 0) {
    handle_missing_packages(missing_packages)
  }

  ret <- list(data = ldata, n_dev = n_dev)

  if (!is.null(info$connection)) {
    ## NOTE: this is a copy of exported connections so that we can
    ## close them once the report finishes running.
    con <- list_to_character(info$connection)
    ret$con <- lapply(names(con)[!duplicated(con)], function(nm) data[[nm]])
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


recipe_current_run_get <- function() {
  cache$current
}


recipe_current_run_clear <- function() {
  cache$current <- NULL
}


##' This function allows inspection of some of orderly's metadata
##'   during an orderly run.  The format returned is internal to
##'   orderly and subject to change.  It is designed to be used either
##'   within report code, or in conjunction with
##'   \code{\link{orderly_test_start}}
##'
##' @title Information on current orderly run
##'
##' @export
##' @examples
##' path <- orderly::orderly_example("depends")
##'
##' # This example uses orderly_run_info within its script, saving the
##' # output to "output.rds"
##' readLines(file.path(path, "src", "depend", "script.R"))
##'
##' orderly::orderly_run("example", root = path)
##' id <- orderly::orderly_run("depend", root = path)
##'
##' # This is the contents:
##' readRDS(file.path(path, "draft", "depend", id, "output.rds"))
orderly_run_info <- function() {
  info <- recipe_current_run_get()
  if (is.null(info)) {
    stop("Not currently running an orderly report")
  }
  info
}


test_mode_end <- function(env = .GlobalEnv) {
  suppressWarnings(rm(list = "Q", envir = env))
  tryCatch(orderly_test_end(), error = function(e) NULL)
}


recipe_file_inputs <- function(info) {
  file_in_data(
    orderly_yml = file_info("orderly.yml"),
    script = file_info(info$script),
    readme = file_info(info$readme),
    source = file_info(info$sources),
    ## TODO: What we really should do is not have put the sources in
    ## here in the first place, then this bit would not be necessary.
    resource = file_info(info$resources),
    global = file_info(info$global_resources))
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

  ## Two readme files e.g. README.md and Readme.MD can happen on unix
  ## systems; it is not clear what we should do here, so we just
  ## ignore the readme silently for now.
  if (length(readme_file) == 1) {
    ## we copy the readme.md file to README.md irrespective of what
    ## case filename the user has used
    file_copy(file.path(src, readme_file), "README.md")
    info$readme <- "README.md"

    ## now check if README is a resource
    if (length(info$resources) > 0) {
      i <- grepl("^(?i)readme(|.md)$", info$resources, ignore.case = FALSE)
      if (any(i)) {
        ## WARNING
        orderly_log("readme",
                    "README.md should not be listed as a resource")
        info$resources <- info$resources[!i]
      }
    }

    ## now check if README is an artefact
    artefact_files <- unlist(info$artefacts[, "filenames"], use.names = FALSE)
    if (any(grepl("^(?i)readme(|.md)$", artefact_files, ignore.case = TRUE))) {
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
    assert_file_exists(
      info$global_resources, check_case = TRUE, workdir = global_path,
      name = sprintf("Global resources in '%s'", global_path))

    global_src <- file.path(global_path, info$global_resources)
    ## See VIMC-2961: the copy here is different to sources and
    ## resources because we can't rename files as they're copied; we
    ## don't support directories and we're pretty limited in how
    ## copying can happen.  I believe the "." that is the destination
    ## of the copy will strip all leading path fragments (path/to/x
    ## becoming x).
    if (any(is_directory(global_src))) {
      stop("global resources cannot yet be directories")
    }
    orderly_log("global", info$global_resources)
    file_copy(global_src, ".", recursive = TRUE)
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
