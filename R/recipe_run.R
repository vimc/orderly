##' Run a report.  The \code{orderly_data} function is for testing the
##' queries (and developing the report).
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
##'   \code{NULL} in which case a list will be returned (rather than
##'   an environment).
##'
##' @inheritParams orderly_list
##' @param echo Print the result of running the R code to the console
##' @export
orderly_run <- function(name, parameters = NULL, envir = .GlobalEnv,
                        config = NULL, locate = TRUE, echo = TRUE) {
  config <- orderly_config_get(config, locate)
  info <- recipe_read(file.path(path_src(config$path), name), config)
  path <- recipe_run(info, parameters, envir,
                     config = config, locate = FALSE, echo = echo)
  ## TODO: I might want to give this as <name>/<id> - not sure?
  ##
  ## The disadvantage of this is that we need to parse these, check
  ## them, etc, and they don't deal well with renames.
  basename(path)
}

##' @export
##' @rdname orderly_run
orderly_data <- function(name, parameters = NULL, envir = NULL,
                         config = NULL, locate = TRUE) {
  config <- orderly_config_get(config, locate)
  info <- recipe_read(file.path(path_src(config$path), name), config)
  con <- orderly_db("source", config)
  if (is.null(info$connection)) {
    on.exit(DBI::dbDisconnect(con))
  }
  dest <- if (is.null(envir)) list() else new.env(parent = envir)
  recipe_data(con, info, parameters, dest)
}


##' For interactive testing of orderly code
##' @title Prepare a directory for orderly to use
##' @inheritParams orderly_run
##' @export
orderly_test_start <- function(name, parameters = NULL, envir = .GlobalEnv,
                               config = NULL, locate = TRUE) {
  if (!is.null(cache$test)) {
    stop("Already running in test mode")
  }

  ## TODO: this should be pulled together with recipe_run as
  ## recipe_prepare but it requires some work to get all the error
  ## handling to behave
  config <- orderly_config_get(config, locate)
  info <- recipe_read(file.path(path_src(config$path), name), config)
  con <- orderly_db("source", config)
  orderly_log("name", info$name)
  id <- new_report_id()
  orderly_log("id", id)

  data <- recipe_data(con$source, info, parameters,
                      new.env(parent = envir))

  workdir <- file.path(path_draft(config$path), info$name, id)
  owd <- recipe_prepare_workdir(info, workdir)
  cache$test <- list(owd = owd,
                     name = name,
                     id = id,
                     parameters = parameters,
                     config = config,
                     prompt = getOption("prompt"))
  withCallingHandlers({
    for (p in info$packages) {
      library(p, character.only = TRUE)
    }
    for (s in info$sources) {
      source(s, envir)
    }
  },
  error = function(e) {
    setwd(cache$test)
    cache$test <- NULL
  })
  options(prompt = "[orderly test] > ")
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

recipe_run <- function(info, parameters, envir = .GlobalEnv,
                       config = NULL, locate = TRUE, echo = TRUE) {
  config <- orderly_config_get(config, locate)
  ## TODO: do this more tidily
  con <- orderly_connect(config)
  on.exit({
    DBI::dbDisconnect(con$source)
    DBI::dbDisconnect(con$destination)
  })

  orderly_log("name", info$name)
  id <- new_report_id()
  orderly_log("id", id)

  data <- recipe_data(con$source, info, parameters,
                      new.env(parent = envir))

  workdir <- file.path(path_draft(config$path), info$name, id)
  owd <- recipe_prepare_workdir(info, workdir)
  on.exit(setwd(owd), add = TRUE)

  hash_resources <- hash_files(info$resources)
  if (length(info$resources) > 0L) {
    orderly_log("resources", sprintf("%s: %s", info$resources, hash_resources))
  } else {
    hash_resources <- NULL
  }

  for (p in info$packages) {
    library(p, character.only = TRUE)
  }
  for (s in info$sources) {
    source(s, envir)
  }
  n_dev <- length(grDevices::dev.list())
  orderly_log("start", as.character(Sys.time()))
  ## TODO: perhaps like context do the ok/fail logging here.  Perhaps
  ## *use* context because this looks an awful lot like the same thing.
  source(info$script, local = new.env(parent = data),
         echo = echo, max.deparse.length = Inf)
  orderly_log("end", as.character(Sys.time()))

  recipe_check_device_stack(n_dev)
  hash_artefacts <- recipe_check_artefacts(info)

  ldata <- as.list(data)[names(info$data)]
  hash_data_csv <- con$csv$mset(ldata)
  hash_data_rds <- con$rds$mset(ldata)
  stopifnot(identical(hash_data_csv, hash_data_rds))

  if (is.null(info$depends)) {
    depends <- NULL
  } else {
    depends <- info$depends
    depends$id <- basename(depends$path)
    depends <- depends[c("name", "id", "filename", "as", "hash")]
  }

  meta <- list(id = id,
               name = info$name,
               parameters = parameters,
               date = as.character(Sys.time()),
               ## Don't know what of these two are most useful:
               hash_orderly = info$hash,
               hash_input = hash_files("orderly.yml", FALSE),
               ## Below here all seems sensible enough to track
               hash_script = hash_files(info$script, FALSE),
               hash_resources = hash_resources,
               hash_data = as.list(hash_data_rds),
               hash_artefacts = as.list(hash_artefacts),
               depends = depends)

  saveRDS(session_info(), path_orderly_run_rds("."))
  writeLines(yaml::as.yaml(meta, column.major = FALSE),
             path_orderly_run_yml("."))
  workdir
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

recipe_data <- function(con, info, parameters, dest) {
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
    file.copy(file.path(src, info$resources), info$resources)
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

recipe_check_artefacts <- function(info) {
  ## Having run the script we should then be able
  expected <- unlist(info$artefacts[, "filename"], use.names = FALSE)
  msg <- !file.exists(expected)
  if (any(msg)) {
    stop("Script did not produce expected artefacts: ",
         paste(expected[msg], collapse = ", "))
  }
  h <- hash_files(expected)
  orderly_log("artefact", sprintf("%s: %s", expected, h))
  h
}

iso_time_str <- function(time = Sys.time()) {
  strftime(time, "%Y%m%d-%H%M%S")
}

new_report_id <- function(time = Sys.time()) {
  sprintf("%s-%s", iso_time_str(time), ids::random_id(bytes = 4))
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
