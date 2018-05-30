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
##' @param parameters Parameters passed to the report
##'
##' @param envir The parent of environment to evalute the report in;
##'   by default a new environment will be made with the global
##'   environment as the parent.  For \code{orderly_data}, this may be
##'   \code{list()} in which case a list will be returned (rather than
##'   an environment).
##' @param ref A git reference to use for this run (see Details)
##' @param open Open the directory after running?
##' @param message An optional message explaining why the report was
##'   run
##'
##' @inheritParams orderly_list
##' @param echo Print the result of running the R code to the console
##' @param id_file Write the identifier into a file
##' @export
orderly_run <- function(name, parameters = NULL, envir = NULL,
                        config = NULL, locate = TRUE, echo = TRUE,
                        id_file = NULL, ref = NULL, open = FALSE,
                        message = NULL) {
  assert_scalar_logical(open)
  envir <- orderly_environment(envir)
  config <- orderly_config_get(config, locate)
  if (!is.null(ref)) {
    prev <- git_detach_head_at_ref(ref, config$path)
    on.exit(git_checkout_branch(prev, TRUE, config$path))
  }
  info <- recipe_read(file.path(path_src(config$path), name), config)
  path <- recipe_run(info, parameters, envir,
                     config = config, locate = FALSE, echo = echo,
                     id_file = id_file, message = message)
  ## TODO: I might want to give this as <name>/<id> - not sure?
  ##
  ## The disadvantage of this is that we need to parse these, check
  ## them, etc, and they don't deal well with renames.
  if (open) {
    open_directory(path)
  }
  basename(path)
}

##' @export
##' @rdname orderly_run
orderly_data <- function(name, parameters = NULL, envir = list(),
                         config = NULL, locate = TRUE) {
  config <- orderly_config_get(config, locate)
  info <- recipe_read(file.path(path_src(config$path), name), config)
  envir <- orderly_environment(envir, TRUE)
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
                               config = NULL, locate = TRUE) {
  if (!is.null(cache$test)) {
    stop("Already running in test mode")
  }

  config <- orderly_config_get(config, locate)
  info <- recipe_read(file.path(path_src(config$path), name), config)
  prep <- orderly_prepare(config, info, parameters, envir, NULL)

  cache$test <- list(owd = prep$owd,
                     name = name,
                     id = prep$id,
                     parameters = parameters,
                     config = config,
                     info = info,
                     prompt = getOption("prompt"))
  options(prompt = "[orderly test] > ")
  makeActiveBinding(quote("Q"), function() {
    rm(list = "Q", envir = .GlobalEnv)
    orderly_test_end()
  }, .GlobalEnv)
  orderly_log("setwd", "running in test draft directory")
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
  orderly_test_start(name, parameters, config = config)
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

recipe_run <- function(info, parameters, envir,
                       config = NULL, locate = TRUE, echo = TRUE,
                       id_file = NULL, message = NULL) {
  config <- orderly_config_get(config, locate)
  
  if (!is.null(message)) {
    assert_scalar_character(message)
  }

  ## should these go later?
  con_rds <- orderly_db("rds", config, FALSE)
  con_csv <- orderly_db("csv", config, FALSE)

  prep <- orderly_prepare(config, info, parameters, envir, id_file)
  ## NOTE: using on.exit here because some of the checking code
  ## depends on hashing files that are relative to the draft working
  ## directory.
  on.exit(setwd(prep$owd))

  orderly_log("start", as.character(Sys.time()))
  source(info$script, local = envir,
         echo = echo, max.deparse.length = Inf)
  orderly_log("end", as.character(Sys.time()))

  recipe_check_device_stack(prep$n_dev)
  hash_artefacts <- recipe_check_artefacts(info, prep$id)

  hash_data_csv <- con_csv$mset(prep$data)
  hash_data_rds <- con_rds$mset(prep$data)
  stopifnot(identical(hash_data_csv, hash_data_rds))

  if (is.null(info$depends)) {
    depends <- NULL
  } else {
    depends <- info$depends
    depends$id <- basename(depends$path)
    depends <- depends[c("name", "id", "filename", "as", "hash")]
  }

  session <- session_info()

  meta <- list(id = prep$id,
               name = info$name,
               parameters = parameters,
               date = as.character(Sys.time()),
               message = message,
               ## Don't know what of these two are most useful:
               hash_orderly = info$hash,
               hash_input = hash_files("orderly.yml", FALSE),
               ## Below here all seems sensible enough to track
               hash_script = hash_files(info$script, FALSE),
               hash_resources = as.list(prep$hash_resources),
               hash_data = as.list(hash_data_rds),
               hash_artefacts = as.list(hash_artefacts),
               depends = depends,
               git = session$git)

  session$meta <- meta
  saveRDS(session, path_orderly_run_rds("."))
  writeLines(yaml::as.yaml(meta, column.major = FALSE),
             path_orderly_run_yml("."))
  prep$workdir
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
  if (!(is.environment(dest) || is.list(dest))) {
    stop("Invalid input for 'dest'")
  }

  info <- recipe_substitute(info, parameters)
  if (!is.null(parameters)) {
    if (is.environment(dest)) {
      list2env(parameters, dest)
    } else {
      dest <- modify_list(dest, parameters)
    }
  }

  if (length(info$data) == 0 && is.null(info$connection)) {
    return(dest)
  }

  con <- orderly_db("source", config)
  on.exit(DBI::dbDisconnect(con))

  views <- info$views
  for (v in names(views)) {
    orderly_log("view", v)
    DBI::dbExecute(con, temporary_view(v, views[[v]]))
  }

  for (v in names(info$data)) {
    dest[[v]] <- DBI::dbGetQuery(con, info$data[[v]])
    orderly_log("data",
                sprintf("%s: %s x %s", v, nrow(dest[[v]]), ncol(dest[[v]])))
  }

  if (!is.null(info$connection)) {
    dest[[info$connection]] <- con
    on.exit()
  }

  dest
}

recipe_prepare_workdir <- function(info, workdir) {
  if (file.exists(workdir)) {
    stop("'workdir' must not exist")
  }
  src <- normalizePath(info$path, mustWork = TRUE)
  dir_create(workdir)
  owd <- setwd(workdir)
  on.exit(setwd(owd)) # (or use withCallingHandlers to do this on error)

  dir.create(dirname(info$script), FALSE, TRUE)
  file_copy(file.path(src, info$script), info$script)
  file_copy(file.path(src, "orderly.yml"), "orderly.yml")

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
    src <- file.path(info$depends$path, info$depends$filename)
    dst <- file.path(workdir, info$depends$as)
    dir_create(dirname(dst))
    str <- sprintf("%s@%s:%s -> %s",
                   info$depends$name,
                   basename(info$depends$path),
                   info$depends$filename,
                   info$depends$as)
    orderly_log("depends", str)
    file.copy(src, dst)
  }

  on.exit() # did not fail

  owd
}

recipe_check_artefacts <- function(info, id) {
  found <- recipe_exists_artefacts(info)
  artefacts <- names(found)
  if (!all(found)) {
    stop("Script did not produce expected artefacts: ",
         paste(artefacts[!found], collapse = ", "))
  }
  unexpected <- recipe_unexpected_artefacts(info, NULL)
  if (length(unexpected) != 0) {
    orderly_log("unex_art", paste("Unexpected artefacts created:",
                                  unexpected, collapse = ", "))
  }
  
  ## TODO: we should watermark the images here but there are some
  ## issues to resolve first:
  ##
  ## * installation on windows/mac is nontrivial
  ## * deal with "_original" files that are being left behind *sometimes*
  ## * general fragility of using system()
  ##
  ## for (filename in artefacts) {
  ##   watermark_write(filename, id)
  ## }
  h <- hash_artefacts(artefacts)
  orderly_log("artefact", sprintf("%s: %s", artefacts, h))
  h
}

hash_artefacts <- function(artefacts) {
  i <- is_directory(artefacts)
  i[is.na(i)] <- FALSE
  h <- set_names(character(length(artefacts)), artefacts)
  h[i] <- hash_directory(artefacts[i])
  h[!i] <- hash_files(artefacts[!i])
  h
}

recipe_exists_artefacts <- function(info, id) {
  expected <- unlist(info$artefacts[, "filenames"], use.names = FALSE)
  exists <- file.exists(expected)
  names(exists) <- expected
  exists
}

recipe_unexpected_artefacts <- function(info, id) {
  expected <- unlist(info$artefacts[, "filenames"], use.names = FALSE)
  # we expect to see all artefacts from the config, the source file and the yml
  # config
  expected <- c(expected, info$script, "orderly.yml")
  # did the yaml file list files to be ignored? [TODO regex]
  ignorance <- info$ignore
  if (!is.null(ignorance)) {
    expected <- c(expected, ignorance)
  }

  found <- list.files()
  # what files have we found that we not contained in expected
  unexpected <- found[!(found %in% expected)]
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

orderly_environment <- function(envir, list_ok = FALSE) {
  if (is.null(envir)) {
    new.env(parent = .GlobalEnv)
  } else if (is.environment(envir) || (is.list(envir) && list_ok)) {
    envir
  } else {
    stop("'envir' must be an ",
         if (list_ok) "environment or list" else "environment")
  }
}

orderly_prepare <- function(config, info, parameters, envir, id_file) {
  orderly_log("name", info$name)
  id <- new_report_id()
  orderly_log("id", id)
  if (!is.null(id_file)) {
    orderly_log("id_file", id_file)
    writelines_atomic(id, id_file)
  }

  ## Because the script (including the files in sources) might modify
  ## the data we need to make sure that we grab a copy of it now (as a
  ## list 'ldata') to pass back
  data <- recipe_data(config, info, parameters, envir)
  ldata <- as.list(data)[names(info$data)]

  ## Compute the device stack size before starting work too
  n_dev <- length(grDevices::dev.list())

  workdir <- file.path(path_draft(config$path), info$name, id)

  owd <- recipe_prepare_workdir(info, workdir)
  withCallingHandlers({
    hash_resources <- hash_files(expand_directory_list(info$resources))
    if (length(hash_resources) > 0L) {
      orderly_log("resources",
                  sprintf("%s: %s", names(hash_resources), hash_resources))
    } else {
      hash_resources <- NULL
    }
    for (p in info$packages) {
      library(p, character.only = TRUE)
    }
    for (s in info$sources) {
      source(s, envir)
    }
  }, error = function(e) setwd(owd))

  list(data = ldata, hash_resources = hash_resources,
       id = id, n_dev = n_dev, owd = owd, workdir = workdir)
}
