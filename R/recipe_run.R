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
##' @param open Open the directory after running?
##'
##' @param message An optional character string containing a message
##'   explaining why the report was run
##'
##' @param extended_output Return detailed output about the run
##'   (similar to the contents of \code{orderly_run.rds}).
##'
##' @inheritParams orderly_list
##' @param echo Print the result of running the R code to the console
##' @param id_file Write the identifier into a file
##' @export
orderly_run <- function(name, parameters = NULL, envir = NULL,
                        root = NULL, locate = TRUE, echo = TRUE,
                        id_file = NULL, fetch = FALSE, ref = NULL,
                        open = FALSE, message = NULL, extended_output = FALSE) {
  assert_scalar_logical(open)
  envir <- orderly_environment(envir)
  config <- orderly_config_get(root, locate)
  check_orderly_archive_version(config)

  info <- recipe_prepare(config, name, id_file, ref, fetch, message)
  on.exit(recipe_current_run_clear())

  info <- recipe_run(info, parameters, envir, config, echo = echo)

  if (open) {
    open_directory(file.path(config$root, "draft", name, info$id))
  }

  if (extended_output) {
    info
  } else {
    info$id
  }
}

##' @export
##' @rdname orderly_run
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
orderly_test_start <- function(name, parameters = NULL, envir = .GlobalEnv,
                               root = NULL, locate = TRUE) {
  if (!is.null(cache$test)) {
    stop("Already running in test mode")
  }

  config <- orderly_config_get(root, locate)
  ## TODO: support ref here
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

  if (file_exists("README.md", check_case = FALSE)) {
    readme <- dir(pattern = "readme.md", ignore.case = TRUE)
    hash_readme <- hash_files(readme)
  } else {
    hash_readme <- NULL
  }

  if (is.null(info$sources)) {
    hash_sources <- NULL
  } else {
    hash_sources <- resource_info$hash_resources[info$sources]
  }
  hash_orderly_yml <- hash_files("orderly.yml")
  hash_script <- hash_files(info$script)
  hash_data_csv <- con_csv$mset(prep$data)
  hash_data_rds <- con_rds$mset(prep$data)
  stopifnot(identical(hash_data_csv, hash_data_rds))

  post_run_resources <- get_resource_info(info)
  compare_resource_hashes(resource_info$hash_resources,
                          post_run_resources$hash_resources)

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

  meta <- list(id = info$id,
               name = info$name,
               parameters = parameters,
               date = as.character(Sys.time()),
               displayname = info$displayname %||% NA_character_,
               description = info$description %||% NA_character_,
               extra_fields = extra_fields,
               connection = !is.null(info$connection),
               packages = info$packages,
               hash_orderly = info$hash,
               hash_orderly_yml = as.list(hash_orderly_yml),
               hash_script = as.list(hash_script),
               hash_readme = as.list(hash_readme),
               hash_sources = as.list(hash_sources),
               hash_resources = as.list(resource_info$hash_resources),
               hash_global = as.list(resource_info$hash_global),
               hash_data = as.list(hash_data_rds),
               hash_artefacts = as.list(hash_artefacts),
               artefacts = info$artefacts,
               depends = depends,
               elapsed = as.numeric(elapsed, "secs"),
               git = info$git)

  if (!is.null(info$data)) {
    meta$data <- data_frame(
      name = names(info$data),
      database = vcapply(info$data, "[[", "database", USE.NAMES = FALSE),
      query = vcapply(info$data, "[[", "query", USE.NAMES = FALSE),
      hash = unname(hash_data_rds))
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
  owd <- setwd(info$workdir)
  on.exit(setwd(owd))

  dir.create(dirname(info$script), FALSE, TRUE)

  file_copy(file.path(src, info$script), info$script)
  file_copy(file.path(src, "orderly.yml"), "orderly.yml")

  # README logic:
  # if there's a readme we copy it
  # if they also list it as a resource let them know that's redundant (edited)
  # if they also list it as an artefact then error
  src_files <- dir(src)
  readme_file <- src_files[tolower(src_files) == "readme.md"]
  ## Two readme files e.g. README.md and Readme.MD can happen on unix systems
  ## I t is not clear what we should do here.
  if (length(readme_file) == 1) {
    ## we copy the readme.md file to README.md irrespective of what
    ## case filename the user has used
    file_copy(file.path(src, readme_file), "README.md")
    ## now check if README is a resource
    if (length(info$resources) > 0) {
      if (any(grepl("README.md", info$resources, ignore.case = FALSE))) {
        ## WARNING
        orderly_log("readme",
                    "README.md should not be listed as a resource")
      }
    }
    ## now check if README is an artefact
    expected <- unlist(info$artefacts[, "filenames"], use.names = FALSE)
    if (length(expected) > 0) {
      if (any(grepl("README.md", expected, ignore.case = FALSE))) {
        stop("README.md should not be listed as an artefact")
      }
    }
  }
  
  if (!is.null(info$resources)) {
    dir_create(dirname(info$resources))
    ## There's a bit of awfulness in R's path handling to deal with here.
    path_resources_src <- file.path(src, info$resources)
    i <- is_directory(path_resources_src)
    file_copy(path_resources_src[!i], info$resources[!i])
    for (j in which(i)) {
      file_copy(path_resources_src[j], dirname(info$resources[j]),
                recursive = TRUE)
    }
  }

  if (!is.null(info$depends)) {
    dep_src <- file.path(info$depends$path, info$depends$filename)
    dep_dst <- file.path(info$workdir, info$depends$as)
    dir_create(dirname(dep_dst))
    info$depends$id_requested <- info$depends$id
    info$depends$id <- basename(info$depends$path)

    str <- sprintf("%s@%s:%s -> %s",
                   info$depends$name,
                   info$depends$id,
                   info$depends$filename,
                   info$depends$as)
    orderly_log("depends", str)
    file_copy(dep_src, dep_dst)
  }

  if (!is.null(info$global_resources)) {
    root_path <- normalizePath(config$root, mustWork = TRUE)
    global_resource_dir <- file.path(root_path, config$global_resources)
    assert_file_exists(x = info$global_resources, check_case = TRUE,
                       workdir = global_resource_dir,
                       name = sprintf("Global resources in '%s'",
                                      global_resource_dir))

    path_global_recs <- file.path(global_resource_dir, info$global_resources)
    dest_resources_src <- file.path(info$workdir, info$global_resources)
    file_copy(path_global_recs, info$workdir, recursive = TRUE)
  }

  ## if we are using resources or global resources...
  if (!is.null(info$resources) || !is.null(info$global_resources)) {
    ## ...hash the resources + calculate the file size,
    ## before we the report has a chance to modify them
    resource_info <- get_resource_info(info)
  } else {
    resource_info <- NULL
  }

  info$changelog <- changelog_load(src, message, info, config)
  changelog_save_json(info$changelog, info$workdir)

  info$owd <- owd
  info$resource_info <- resource_info
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
  expected <- c(expected, resources, dependencies, info$script, "orderly.yml",
                "changelog.json")

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

get_resource_info <- function(info) {
  hash_resources <- hash_files(expand_directory_list(info$resources))
  if (length(hash_resources) > 0L) {
    orderly_log("resources",
                sprintf("%s: %s", names(hash_resources), hash_resources))
  } else {
    hash_resources <- NULL
  }

  hash_global <- hash_files(expand_directory_list(info$global_resources))
  if (length(hash_global) > 0L) {
    orderly_log("global",
                sprintf("%s: %s", names(hash_global), hash_global))
  } else {
    hash_global <- NULL
  }
  list(hash_resources = hash_resources, hash_global = hash_global)
}

compare_resource_hashes <- function(pre_run_hashes, post_run_hashes) {
  ## compare resource hashes here
  found <- names(post_run_hashes)
  expected <- names(pre_run_hashes)

  ## we expected a resource but can't find it.
  not_found <- which(!(expected %in% found))
  if (any(not_found)) {
    stop("Script deleted the following resources: ",
         paste(expected[not_found], collapse = ", "))
  }

  ## at this point the set of found == the set of expected
  found_hash <- post_run_hashes[found] ## put the hashes in the same order
  expected_hash <- pre_run_hashes[found]
  hash_miss <- which(found_hash != expected_hash)
  if (any(hash_miss)) {
    stop("Script has modified resources: ",
         paste(found[hash_miss], collapse = ", "))
  }
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


##' Get information on current orderly run
##' @title Information on current orderly run
##' @export
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
