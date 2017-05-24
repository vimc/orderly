recipe_run <- function(info, parameters, envir = .GlobalEnv,
                       config = NULL, locate = TRUE) {
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
  n_dev <- length(dev.list())
  source(info$script, local = new.env(parent = data),
         echo = TRUE, max.deparse.length = Inf)
  recipe_check_device_stack(n_dev)
  recipe_check_artefacts(info)

  ldata <- as.list(data)
  hash_data_csv <- con$csv$mset(ldata)
  hash_data_rds <- con$rds$mset(ldata)
  stopifnot(identical(hash_data_csv, hash_data_rds))

  ## The question here is do we actually want to store the data files
  ## by name or store hashes to the global set of these?  Probably the
  ## latter!

  meta <- list(id = id,
               name = info$name,
               parameters = parameters,
               date = Sys.time(),
               ## Don't know what of these two are most useful:
               hash_orderly = info$hash,
               hash_input = hash_files("orderly.yml", FALSE),
               ## Below here all seems sensible enough to track
               hash_resources = hash_files(info$resources),
               hash_data = hash_data_rds,
               hash_artefacts = hash_files(info$artefacts[, "filename"]),
               identifier = ids::adjective_animal())

  saveRDS(sessionInfo(), path_orderly_run_rds("."))
  writeLines(yaml::as.yaml(meta), path_orderly_run_yml("."))
  invisible(workdir)
}

recipe_substitute <- function(info, parameters) {
  if (!setequal(names(parameters), info$parameters)) {
    stop("Unexpected parameters")
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
    stop("'workdir' must be an empty directory")
  }
  dir_create(workdir)
  owd <- setwd(workdir)

  src <- file.path(owd, info$path)

  dir.create(dirname(info$script), FALSE, TRUE)
  file.copy(file.path(src, info$script), info$script)
  file.copy(file.path(src, "orderly.yml"), "orderly.yml")

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

recipe_save_data <- function(data) {
  dir.create("data")
  ## Then we collect up all our bits:
  dest <- file.path("data", paste0(names(data)), ".csv")
  for (i in seq_along(data)) {
    write.csv(data[[i]], dest[[i]], row.names = FALSE)
  }
  hash_files(dest)
}

recipe_commit <- function(workdir, config = NULL, locate = TRUE) {
  config <- orderly_config_get(config, locate)
  dat <- report_read_data(workdir)
  con <- orderly_db("destination", config)
  tbl <- report_db_init(con, config)
  dest <- copy_report(workdir, info$name, config)
  on.exit(unlink(dest, recursive = TRUE))
  if (DBI::dbWriteTable(con, tbl, dat, append = TRUE)) {
    unlink(workdir, recursive = TRUE)
    on.exit()
  }
  dest
}

report_read_data <- function(path) {
  yml <- path_orderly_run_yml(path)
  if (!file.exists(yml)) {
    stop("Did not find run metadata file!")
  }
  info <- modifyList(yaml_read(file.path(path, "orderly.yml")),
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

recipes_check_device_stack <- function(expected) {
  check <- length(dev.list()) - n_dev
  if (check == 0) {
  } else if (check > 1) {
    for (i in seq_len(check)) {
      dev.off()
    }
    stop(ngettext(check,
                  "Report left device open",
                  sprintf("Report left %d devices open", check)))
  } else {
    stop(sprintf("Report closed %d more devices than it opened!", check))
  }
}
