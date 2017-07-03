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
  ## I might want to give this as <name>/<id> - not sure?
  basename(path)
}

##' @export
##' @rdname orderly_run
orderly_data <- function(name, parameters = NULL, envir = NULL,
                         config = NULL, locate = TRUE) {
  config <- orderly_config_get(config, locate)
  info <- recipe_read(file.path(path_src(config$path), name), config)
  con <- orderly_db("source", config)
  dest <- if (is.null(envir)) list() else new.env(parent = envir)
  recipe_data(con, info, parameters, dest)
}

recipe_run <- function(info, parameters, envir = .GlobalEnv,
                       config = NULL, locate = TRUE, echo = TRUE) {
  config <- orderly_config_get(config, locate)
  con <- orderly_connect(config)

  orderly_log("name", info$name)
  id <- new_report_id()
  orderly_log("id", id)

  data <- recipe_data(con$source, info, parameters,
                      new.env(parent = envir))

  workdir <- file.path(path_draft(config$path), info$name, id)
  owd <- recipe_prepare_workdir(info, workdir)
  on.exit(setwd(owd))

  hash_resources <- hash_files(info$resources)
  if (length(info$resources) > 0L) {
    orderly_log("resources", sprintf("%s: %s", info$resources, hash_resources))
  }

  for (p in info$packages) {
    library(p, character.only = TRUE)
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

  meta <- list(id = id,
               name = info$name,
               parameters = parameters,
               date = as.character(Sys.time()),
               ## Don't know what of these two are most useful:
               hash_orderly = info$hash,
               hash_input = hash_files("orderly.yml", FALSE),
               ## Below here all seems sensible enough to track
               hash_resources = hash_resources,
               hash_data = hash_data_rds,
               hash_artefacts = hash_artefacts)

  saveRDS(utils::sessionInfo(), path_orderly_run_rds("."))
  writeLines(yaml::as.yaml(meta), path_orderly_run_yml("."))
  invisible(workdir)
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
  for (i in seq_along(parameters)) {
    if (is.environment(dest)) {
      list2env(parameters, dest)
    } else {
      dest <- modifyList(dest, parameters)
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

  dest
}

recipe_prepare_workdir <- function(info, workdir) {
  if (file.exists(workdir)) {
    stop("'workdir' must not exist")
  }
  src <- normalizePath(info$path, mustWork = TRUE)
  dir_create(workdir)
  owd <- setwd(workdir)

  dir.create(dirname(info$script), FALSE, TRUE)
  file_copy(file.path(src, info$script), info$script)
  file_copy(file.path(src, "orderly.yml"), "orderly.yml")

  if (!is.null(info$resources)) {
    dir_create(dirname(info$resources))
    file.copy(file.path(src, info$resources), info$resources)
  }

  owd
}

recipe_check_artefacts <- function(info) {
  ## Having run the script we should then be able
  expected <- info$artefacts[, "filename"]
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

new_report_id <- function() {
  sprintf("%s-%s", iso_time_str(), ids::random_id(bytes = 4))
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
