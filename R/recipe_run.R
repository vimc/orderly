recipe_run <- function(info, parameters, envir = .GlobalEnv,
                       config = NULL, locate = TRUE, echo = TRUE) {
  config <- orderly_config_get(config, locate)
  con <- orderly_connect(config)
  data <- recipe_data(con$source, info, parameters,
                      new.env(parent = envir))

  id <- new_report_id()
  workdir <- file.path(path_draft(config$path), id)
  owd <- recipe_prepare_workdir(info, workdir)
  on.exit(setwd(owd))

  for (p in info$packages) {
    library(p, character.only = TRUE)
  }
  n_dev <- length(grDevices::dev.list())
  source(info$script, local = new.env(parent = data),
         echo = echo, max.deparse.length = Inf)
  recipe_check_device_stack(n_dev)
  recipe_check_artefacts(info)

  ldata <- as.list(data)
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
               hash_resources = hash_files(info$resources),
               hash_data = hash_data_rds,
               hash_artefacts = hash_files(info$artefacts[, "filename"]),
               identifier = ids::adjective_animal())

  saveRDS(utils::sessionInfo(), path_orderly_run_rds("."))
  writeLines(yaml::as.yaml(meta), path_orderly_run_yml("."))
  invisible(workdir)
}

recipe_substitute <- function(info, parameters) {
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
  }
  info$hash_parameters <- digest::digest(parameters)
  info
}

recipe_data <- function(con, info, parameters, dest) {
  if (!(is.environment(dest) || is.list(dest))) {
    stop("Invalid input for 'dest'")
  }

  info <- recipe_substitute(info, parameters)

  views <- info$views
  for (v in names(views)) {
    DBI::dbExecute(con, temporary_view(v, views[[v]]))
  }

  for (v in names(info$data)) {
    dest[[v]] <- DBI::dbGetQuery(con, info$data[[v]])
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
}

recipe_commit <- function(workdir, config) {
  config <- orderly_config_get(config)
  dat <- report_read_data(workdir)
  con <- orderly_db("destination", config)
  tbl <- report_db_init(con, config)
  dest <- copy_report(workdir, dat$name, config)
  on.exit(unlink(dest, recursive = TRUE))
  if (DBI::dbWriteTable(con, tbl, dat, append = TRUE)) {
    unlink(workdir, recursive = TRUE)
    on.exit()
  } else {
    ## Really not sure about this, and until the error handling is
    ## done this will be a worry; not sure what dbWriteTable can fail
    ## on.
    stop("Unknown error [orderly bug]") # nocov
  }
  dest
}

report_read_data <- function(path) {
  yml <- path_orderly_run_yml(path)
  if (!file.exists(yml)) {
    stop("Did not find run metadata file!")
  }
  info <- utils::modifyList(yaml_read(file.path(path, "orderly.yml")),
                     yaml_read(yml))
  if (info$id != basename(path)) {
    stop("Unexpected path") # should never happen
  }
  data.frame(id = info$id,
             name = info$name,
             ## Inputs
             requester = info$requester,
             author = info$author,
             views = to_json_string(info$views),
             data = to_json_string(info$data),
             script = info$script,
             artefacts = to_json_string(info$artefacts),
             resources = to_json_string(info$resources),
             ## Outputs
             parameters = to_json_string(info$parameters),
             date = info$date,
             hash_orderly = info$hash_orderly,
             hash_input = info$hash_input,
             hash_resources = to_json_string(info$hash_resources),
             hash_data = to_json_string(info$hash_data),
             hash_artefacts = to_json_string(info$hash_artefacts),
             stringsAsFactors = FALSE)
}

copy_report <- function(workdir, name, config) {
  assert_is(config, "orderly_config")
  id <- basename(workdir)
  parent <- path_archive(config$path, name)
  dest <- file.path(parent, id)
  if (file.exists(dest)) {
    stop("Already been copied?")
  }

  dir_create(parent)
  ok <- file.copy(workdir, parent, recursive = TRUE)
  if (!ok) {
    try(unlink(dest, recursive = TRUE), silent = TRUE)
    stop("Error copying file")
  }
  dest
}

new_report_id <- function() {
  ids::random_id()
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
