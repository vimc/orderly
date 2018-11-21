context("run")

## This is an integration test really.  I need to carve more units out
## of this code still but this will let me know if I'm going along the
## right track and if I'm breaking things!
test_that("run", {
  path <- tempfile()
  orderly_init(path)

  with_sqlite(file.path(path, "source.sqlite"), fake_db)

  file.copy("example_config.yml", file.path(path, "orderly_config.yml"),
            overwrite = TRUE)

  path_example <- file.path(path, "src", "example")

  dir.create(path_example)
  file.copy("example_report.yml", file.path(path_example, "orderly.yml"))
  file.copy("example_script.R", file.path(path_example, "script.R"))

  parameters <- list(minvalue = 0.5)

  ## This probably all rolls together I think?  It's not totally clear
  ## what the run/commit workflow should look like (especially when
  ## developing a draft analysis)
  config <- orderly_config(path)
  info <- recipe_read(file.path(path, "src", "example"), config)
  expect_equal(info$name, basename(path_example))

  envir <- orderly_environment(NULL)
  info <- recipe_prepare(config, "example")
  res <- recipe_run(info, parameters, envir, config, echo = FALSE)
  p <- file.path(path_draft(config$path), res$name, res$id)
  expect_true(is_directory(p))

  expect_true(file.exists(file.path(p, "mygraph.png")))
  expect_true(file.exists(file.path(p, "script.R")))
  expect_true(file.exists(file.path(p, "orderly.yml")))
  expect_true(file.exists(file.path(p, "orderly_run.yml")))
  expect_true(file.exists(file.path(p, "orderly_run.rds")))
  expect_equal(length(dir(p)), 5) # the above are _all_ files produced
  files <- dir(p)
  cmp <- set_names(hash_files(dir(p, full.names = TRUE), FALSE), files)

  ## These files are unmodified
  expect_equal(hash_files(file.path(path_example, "orderly.yml"), FALSE),
               hash_files(file.path(p, "orderly.yml"), FALSE))
  expect_equal(hash_files(file.path(path_example, "script.R"), FALSE),
               hash_files(file.path(p, "script.R"), FALSE))

  ## This needs to look reasonable:
  d <- readRDS(file.path(p, "orderly_run.rds"))
  expect_is(d$session_info, "sessionInfo")
  expect_is(d$time, "POSIXt")
  expect_is(d$env, "list")

  expect_identical(readBin(file.path(p, "mygraph.png"), raw(), 8),
                   MAGIC_PNG)

  run <- yaml_read(file.path(p, "orderly_run.yml"))
  expect_equal(run$id, basename(p))
  expect_equal(run$name, info$name)
  expect_identical(unname(unlist(run$hash_artefacts, use.names = FALSE)),
                   hash_files(file.path(p, "mygraph.png"), FALSE))
  expect_identical(run$hash_resources, list())
  expect_identical(run$parameters, parameters)
  expect_is(run$date, "character")
  ## I feel hash_orderly and hash_input have the wrong names here
  expect_identical(run$hash_orderly, info$hash)
  expect_identical(run$hash_input,
                   hash_files(file.path(p, "orderly.yml"), FALSE))

  expect_is(run$hash_data, "list")
  expect_equal(length(run$hash_data), 1)

  con_rds <- orderly_db("rds", config)
  con_csv <- orderly_db("csv", config)

  expect_identical(con_rds$list(), unlist(run$hash_data, use.names = FALSE))
  expect_identical(con_csv$list(), unlist(run$hash_data, use.names = FALSE))

  ## Confirm that things are OK:
  expect_equal(con_rds$get(run$hash_data),
               con_csv$get(run$hash_data))

  ## Confirm that the *format* is OK too by reading the files manually:
  expect_identical(con_rds$get(run$hash_data),
                   readRDS(con_rds$filename(run$hash_data)))
  expect_identical(con_csv$get(run$hash_data),
                   read_csv(con_csv$filename(run$hash_data)))

  ## Then we commit the results:
  q <- recipe_commit(p, path)
  expect_false(file.exists(p))
  expect_true(file.exists(q))

  ## Everything copied over ok:
  expect_equal(set_names(hash_files(dir(q, full.names = TRUE), FALSE), files),
               cmp)

  con_destination <- orderly_db("destination", config)
  on.exit(DBI::dbDisconnect(con_destination))
  expect_true(DBI::dbExistsTable(con_destination, "orderly"))
  d <- DBI::dbReadTable(con_destination, "orderly")
  expect_true(all(vlapply(d[names(d) != "published"], is.character)))
  expect_true(is.numeric(d$published))

  expect_equal(d$id, basename(q))
})

## Same as in read; we generate a report and then break it
test_that("minimal", {
  path <- prepare_orderly_example("minimal")
  on.exit(unlink(path, recursive = TRUE))

  config <- orderly_config(path)
  info <- recipe_read(file.path(path, "src/example"), config)
  data <- recipe_data(config, info, NULL, new.env(parent = .GlobalEnv))
  expect_is(data$dat, "data.frame")

  expect_error(
    recipe_data(config, info, list(a = 1), new.env(parent = .GlobalEnv)),
    "Extra parameters: 'a'")
  expect_error(
    recipe_data(config, info, NULL, NULL),
    "Invalid input for 'dest'")

  info$workdir <- tempfile()
  dir.create(info$workdir)
  expect_error(recipe_prepare_workdir(info),
               "'workdir' must not exist")
  unlink(info$workdir, recursive = TRUE)

  envir <- orderly_environment(NULL)
  info <- recipe_prepare(config, "example")
  res <- recipe_run(info, NULL, envir, config, echo = FALSE)
  p <- file.path(path_draft(config$path), res$name, res$id)
  files <- dir(p)
  expect_true(file.exists(file.path(p, "orderly.yml")))
  expect_true(file.exists(file.path(p, "orderly_run.yml")))
  expect_true(file.exists(file.path(p, "orderly_run.rds")))
  expect_true(file.exists(file.path(p, "script.R")))
  expect_true(file.exists(file.path(p, "mygraph.png")))

  recipe_commit(p, config)
})

test_that("orderly_data", {
  path <- prepare_orderly_example("minimal")
  on.exit(unlink(path, recursive = TRUE))

  d <- orderly_data("example", config = path)
  expect_is(d, "environment")
  expect_is(d$dat, "data.frame")

  e1 <- new.env(parent = baseenv())
  e <- orderly_data("example", config = path, envir = e1)
  expect_identical(e, e1)

  expect_identical(e$dat, d$dat)
})

test_that("fail to create artefact", {
  path <- prepare_orderly_example("minimal")
  on.exit(unlink(path, recursive = TRUE))
  config <- orderly_config(path)
  writeLines("1 + 1", file.path(path, "src/example/script.R"))
  info <- recipe_read(file.path(path, "src/example"), config)
  envir <- orderly_environment(NULL)
  info <- recipe_prepare(config, "example")
  expect_error(recipe_run(info, NULL, envir, config = config, echo = FALSE),
               "Script did not produce expected artefacts: mygraph.png")
})

test_that("leave device open", {
  path <- prepare_orderly_example("minimal")
  on.exit(unlink(path, recursive = TRUE))
  config <- orderly_config(path)
  txt <- readLines(file.path(path, "src/example/script.R"))
  writeLines(txt[!grepl("dev.off()", txt, fixed = TRUE)],
             file.path(path, "src/example/script.R"))
  info <- recipe_read(file.path(path, "src/example"), config)
  envir <- orderly_environment(NULL)
  info <- recipe_prepare(config, "example")
  expect_error(recipe_run(info, NULL, envir, config = config, echo = FALSE),
               "Report left 1 device open")
})

test_that("close too many devices", {
  path <- prepare_orderly_example("minimal")
  png(tempfile())
  n <- length(dev.list())
  on.exit({
    unlink(path, recursive = TRUE)
    if (length(dev.list()) == n) {
      dev.off()
    }
  })

  config <- orderly_config(path)
  txt <- readLines(file.path(path, "src/example/script.R"))
  writeLines(c(txt, "dev.off()"), file.path(path, "src/example/script.R"))
  info <- recipe_read(file.path(path, "src/example"), config)
  envir <- orderly_environment(NULL)
  info <- recipe_prepare(config, "example")
  expect_error(recipe_run(info, NULL, envir, config = config, echo = FALSE),
               "Report closed 1 more devices than it opened")
})

test_that("included example", {
  path <- prepare_orderly_example("example")
  id <- orderly_run("example", list(cyl = 4), config = path, echo = FALSE)
  p <- orderly_commit(id, config = path)
  expect_true(is_directory(p))
  dat <- read_orderly_db(path)
  expect_equal(dat$description, NA_character_)
  expect_equal(dat$displayname, NA_character_)
})

test_that("included other", {
  path <- prepare_orderly_example("other")
  id <- orderly_run("other", list(nmin = 0), config = path, echo = FALSE)
  p <- orderly_commit(id, config = path)
  info <- recipe_read(file.path(path_src(path), "other"),
                      orderly_config(path))
  dat <- read_orderly_db(path)
  expect_equal(dat$description, info$description)
  expect_equal(dat$displayname, info$displayname)
})

test_that("connection", {
  path <- prepare_orderly_example("minimal")
  on.exit(unlink(path, recursive = TRUE))

  path_example <- file.path(path, "src", "example")
  yml <- file.path(path_example, "orderly.yml")
  txt <- readLines(yml)
  writeLines(c(txt, "connection: con"), yml)

  config <- orderly_config(path)
  info <- recipe_read(path_example, config)
  expect_identical(info$connection, "con")

  data <- orderly_data("example",
                       envir = new.env(parent = .GlobalEnv),
                       config = path)
  expect_is(data$con, "SQLiteConnection")
  expect_is(DBI::dbReadTable(data$con, "data"), "data.frame")
  DBI::dbDisconnect(data$con)
})


test_that("connection is saved to db", {
  path <- prepare_orderly_example("minimal")

  id1 <- orderly_run("example", config = path, echo = FALSE)
  orderly_commit(id1, config = path)

  path_example <- file.path(path, "src", "example")
  yml <- file.path(path_example, "orderly.yml")
  txt <- readLines(yml)
  writeLines(c(txt, "connection: con"), yml)

  id2 <- orderly_run("example", config = path, echo = FALSE)
  orderly_commit(id2, config = path)

  con <- orderly_db("destination", config = path)
  on.exit(DBI::dbDisconnect(con))
  d <- DBI::dbGetQuery(con, "SELECT id, connection FROM report_version")
  expect_equal(d$connection[d$id == id1], 0L)
  expect_equal(d$connection[d$id == id2], 1L)
})


test_that("no data", {
  path <- prepare_orderly_example("minimal")
  yml <- c("data: ~",
           "script: script.R",
           "artefacts:",
           "  data:",
           "    filenames: data.rds",
           "    description: the data")
  script <- "saveRDS(mtcars, 'data.rds')"
  path_example <- file.path(path, "src", "example")
  writeLines(yml, file.path(path_example, "orderly.yml"))
  writeLines(script, file.path(path_example, "script.R"))

  data <- orderly_data("example",
                       envir = new.env(parent = .GlobalEnv),
                       config = path)
  expect_equal(ls(data, all.names = TRUE), character(0))

  id <- orderly_run("example", config = path, echo = FALSE)
  p <- file.path(path_draft(path), "example", id, "data.rds")
  expect_true(file.exists(p))
  expect_equal(readRDS(p), mtcars)
})

test_that("use artefact", {
  path <- prepare_orderly_example("depends")

  path_example <- file.path(path, "src", "example")
  path_depend <- file.path(path, "src", "depend")
  id1 <- orderly_run("example", config = path, echo = FALSE)
  orderly_log_break()
  path_orig <- file.path(path_draft(path), "example", id1, "data.rds")
  expect_true(file.exists(path_orig))

  data <- orderly_data("depend",
                       envir = new.env(parent = .GlobalEnv),
                       config = path)
  expect_identical(ls(data), character(0))
  id2 <- orderly_run("depend", config = path, echo = FALSE)
  orderly_log_break()
  path_previous <- file.path(path_draft(path), "depend", id2, "previous.rds")
  expect_true(file.exists(path_previous))
  expect_equal(hash_files(path_previous, FALSE),
               hash_files(path_orig, FALSE))

  d <-
    yaml_read(path_orderly_run_yml(file.path(path_draft(path), "depend", id2)))
  expect_equal(d$depends[[1]]$hash, hash_files(path_previous, FALSE))

  ## Then rebuild the original:
  id3 <- orderly_run("example", config = path, echo = FALSE)
  orderly_log_break()
  id4 <- orderly_run("depend", config = path, echo = FALSE)
  orderly_log_break()
  path_orig2 <- file.path(path_draft(path), "example", id3, "data.rds")
  path_previous2 <- file.path(path_draft(path), "depend", id4, "previous.rds")

  expect_equal(hash_files(path_previous2, FALSE),
               hash_files(path_orig2, FALSE))
  expect_true(hash_files(path_previous2, FALSE) !=
              hash_files(path_previous, FALSE))

  ## Then we need to commit things and check that it all still works OK.
  expect_error(orderly_commit(id2, config = path),
               "Report uses draft id - commit first")
  p1 <- orderly_commit(id1, config = path)
  p2 <- orderly_commit(id2, config = path)
  expect_error(orderly_commit(id4, config = path), id3)
  p3 <- orderly_commit(id3, config = path)
  p4 <- orderly_commit(id4, config = path)
})

test_that("Can't commit report using nonexistant id", {
  path <- prepare_orderly_example("depends")
  id1 <- orderly_run("example", config = path, echo = FALSE)
  id2 <- orderly_run("depend", config = path, echo = FALSE)
  unlink(file.path(path, "draft", "example", id1), recursive = TRUE)
  expect_error(orderly_commit(id2, config = path),
               "Report uses nonexistant id")
})

test_that("resources", {
  path <- prepare_orderly_example("resources")
  id <- orderly_run("use_resource", config = path, echo = FALSE)
  p <- file.path(path, "draft", "use_resource", id)
  expect_true(file.exists(file.path(p, "meta/data.csv")))
  p <- orderly_commit(id, config = path)

  d <- read_orderly_db(path)
  expect_identical(d$resources, '["meta/data.csv"]')
  expect_identical(d$hash_resources,
                   '{"meta/data.csv":"0bec5bf6f93c547bc9c6774acaf85e1a"}')
  expect_true(file.exists(file.path(p, "meta/data.csv")))
})


test_that("markdown", {
  skip_if_not_installed("rmarkdown")
  path <- prepare_orderly_example("knitr")

  id <- orderly_run("example", config = path, echo = FALSE)

  report <- file.path(path, "draft", "example", id, "report.html")
  expect_true(file.exists(report))
  expect_true(any(grepl("ANSWER:2", readLines(report))))
})

test_that("database is not loaded unless needed", {
  vars <- c(SOME_ENVVAR = "source.sqlite")
  path <- withr::with_envvar(vars, prepare_orderly_example("nodb"))

  expect_identical(as.list(orderly_data("example", config = path)), list())
  id <- orderly_run("example", config = path, echo = FALSE)
  expect_true(
    file.exists(file.path(path, "draft", "example", id, "mygraph.png")))
  expect_error(orderly_db("source", path), "SOME_ENVVAR")
})

test_that("id file", {
  path <- prepare_orderly_example("minimal")
  tmp <- tempfile()
  id <- orderly_run("example", config = path, id_file = tmp, echo = FALSE)
  expect_true(file.exists(tmp))
  expect_equal(readLines(tmp), id)
})

test_that("test_start, test_restart", {
  owd <- getwd()
  on.exit(setwd(owd))

  path <- prepare_orderly_example("minimal")
  orderly_test_start("example", config = path)

  expect_equal(normalizePath(dirname(getwd())),
               normalizePath(file.path(path, "draft/example")))
  id <- basename(getwd())
  expect_equal(orderly_list_drafts(path)$id, id)

  expect_error(orderly_test_start("example", config = path),
               "Already running in test mode")

  orderly_test_restart()
  id2 <- basename(getwd())
  expect_false(id2 == id)
  expect_equal(orderly_list_drafts(path)$id, id2)

  orderly_test_end()
  expect_equal(getwd(), owd)
  expect_error(orderly_test_end(), "Not running in test mode")
  withr::with_options(
    list(orderly.config = orderly_config(path)),
    expect_error(orderly_test_restart(), "Not running in test mode"))
})

test_that("test mode artefacts", {
  owd <- getwd()
  on.exit(setwd(owd))

  path <- prepare_orderly_example("minimal")
  orderly_test_start("example", config = path)
  on.exit(orderly_test_end(), add = FALSE)

  expect_false(orderly_test_check())

  writeLines(character(0), "mygraph.png")
  expect_true(orderly_test_check())
})


test_that("orderly_test_check requires test mode", {
  expect_error(orderly_test_check(), "Not running in test mode")
})


test_that("test mode end", {
  env <- new.env()
  env$Q <- TRUE
  test_mode_end(env)
  expect_null(env$Q)
})


test_that("run with message", {
  path <- prepare_orderly_example("changelog")
  test_message <- "[label1] test"
  id <- orderly_run("example", config = path, echo = FALSE,
                    message = test_message)
  p <- orderly_commit(id, config = path)

  expect_equal(changelog_read_json(p),
               data_frame(
                 label = "label1", value = "test", from_file = FALSE,
                 report_version = id))
})


test_that("no unexpected artefact", {
  path <- prepare_orderly_example("minimal")
  tmp <- tempfile()
  path_example <- file.path(path, "src", "example")
  # we're not expecting an 'unexpected' message at this point
  # grab all messages...
  messages <- capture_messages(orderly_run("example", config = path,
                                           id_file = tmp, echo = FALSE))
  # ...make sure none of the messages contain "unexpected"
  expect_false(any(grep("unexpected", messages)))
})


test_that("renamed dependencies are expected", {
  path <- prepare_orderly_example("depends")
  orderly_run("example", config = path, echo = FALSE)
  messages <- capture_messages(
    orderly_run("depend", config = path, echo = FALSE))
  expect_false(any(grep("unexpected", messages)))
})


test_that("shiny app", {
  path <- prepare_orderly_example("shiny")
  id <- orderly_run("example", config = path, echo = FALSE)
  p_shiny <- file.path(path, "draft", "example", id, "shiny")
  expect_true(file.exists(p_shiny))
  expect_true(file.exists(file.path(p_shiny, "app.R")))
  expect_true(file.exists(file.path(p_shiny, "data.RData")))

  dest <- file.path(path, "shiny")
  expect_error(orderly_deploy_shiny(dest, config = path),
               "Did not find any archive reports for example")
  orderly_commit(id, config = path)
  orderly_deploy_shiny(dest, config = path)

  expect_true(file.exists(file.path(dest, "index.html")))
  expect_true(file.exists(file.path(dest, "example-shiny-app")))

  p <- file.path(dest, "example-shiny-app")
  q <- file.path(path, "archive", "example", id, "shiny")
  expect_true(file.exists(file.path(p, "orderly_id")))
  expect_equal(readLines(file.path(p, "orderly_id")), id)

  expect_true(all(dir(q) %in% dir(p)))
  expect_equal(hash_files(file.path(p, dir(q)), FALSE),
               hash_files(file.path(q, dir(q)), FALSE))
})

test_that("non-existent package", {
  path <- prepare_orderly_example("minimal")
  tmp <- tempfile()
  path_example <- file.path(path, "src", "example")
  yml_path <- file.path(path_example, "orderly.yml")

  # add a non-existent package to the yaml
  write(sprintf("packages: %s", "non_existent_package"),
        file = yml_path, append = TRUE)
  # has orderly detected that the package does not exist>
  expect_error(orderly_run("example", config = path, id_file = tmp,
                           echo = FALSE),
               "Missing packages: 'non_existent_package'")
})

test_that("multiple non-existent packages", {
  path <- prepare_orderly_example("minimal")
  tmp <- tempfile()
  path_example <- file.path(path, "src", "example")
  yml_path <- file.path(path_example, "orderly.yml")

  # add a non-existent package to the yaml
  write(sprintf("packages: \n  - %s", "non_existent_package"),
        file = yml_path, append = TRUE)
  write(sprintf("  - %s", "non_existent_package_2"),
        file = yml_path, append = TRUE)
  # has orderly detected that the package does not exist>
  expect_error(orderly_run("example", config = path, id_file = tmp,
                           echo = FALSE),
               paste("Missing packages:",
                     "'non_existent_package', 'non_existent_package_2'"))
})

test_that("use multiple versions of an artefact", {
  path <- prepare_orderly_example("depends")

  id1 <- orderly_run("example", config = path, echo = FALSE)
  id2 <- orderly_run("example", config = path, echo = FALSE)
  orderly_commit(id2, config = path)

  id3 <- orderly_run("depend2", config = path, echo = FALSE)

  p1 <- file.path(path, "draft", "depend2", id3,
                  c("previous1.rds", "previous2.rds"))
  expect_true(all(file.exists(p1)))

  p2 <- file.path(path, c("draft", "archive"), "example", c(id1, id2),
                  "data.rds")
  expect_equal(hash_files(p1, FALSE),
               hash_files(p2, FALSE))
})

test_that("required field OK", {
  path <- prepare_orderly_example("minimal")
  tmp <- tempfile()
  # we need to use an orderly config with required fields set so copy it over
  # this must have exactly two required fields
  file.copy("example_config.yml", file.path(path, "orderly_config.yml"),
            overwrite = TRUE)
  path_example <- file.path(path, "src", "example")
  # grab the current report yml without required fields
  # this will fail with the new orderly_config.yml
  yml_path <- file.path(path_example, "orderly.yml")
  minimal_yml <- readLines(yml_path)
  # get required fields out of config
  config <- orderly_config(path)
  req_fields <- config$fields$name[config$fields$required]
  # set required fields to correct type
  minimal_yml <- c(minimal_yml, sprintf("%s: %s", req_fields[1], "character"))
  minimal_yml <- c(minimal_yml, sprintf("%s: %s", req_fields[2], "character"))
  writeLines(minimal_yml, yml_path)
  
  id <- orderly_run("example", config = path, id_file = tmp, echo = FALSE)
  p <- file.path(path_draft(path), "example", id, "mygraph.png")
  expect_true(file.exists(p))
})

test_that("missing required field", {
  path <- prepare_orderly_example("minimal")
  tmp <- tempfile()
  # we need to use an orderly config with required fields set so copy it over
  file.copy("example_config.yml", file.path(path, "orderly_config.yml"),
            overwrite = TRUE)
  path_example <- file.path(path, "src", "example")
  # grab the current report yml without required fields
  # this will fail with the new orderly_config.yml
  yml_path <- file.path(path_example, "orderly.yml")
  minimal_yml <- readLines(yml_path)
  # get required fields out of config
  config <- orderly_config(path)
  req_fields <- config$fields$name[config$fields$required]
  # iterate over the required fields...
  for (field in req_fields) {
    # ...add one of them to the end of the file
    broken_yml <- c(minimal_yml, sprintf("%s: %s", field, "Value"))
    writeLines(broken_yml, yml_path)
    # required fields still missing
    missing_required <- setdiff(req_fields, field)

    # we are expecting an error message here
    if (length(missing_required) > 0) {
      err_msg <- sprintf("Fields missing from .*: %s",
                        paste(missing_required, collapse = ", ")
                        )
      expect_error(orderly_run("example", config = path, id_file = tmp,
                               echo = FALSE),
                   regexp = err_msg)
    }
  }
})

test_that("required field wrong type", {
  path <- prepare_orderly_example("minimal")
  tmp <- tempfile()
  # we need to use an orderly config with required fields set so copy it over
  # this must have exactly two required fields
  file.copy("example_config.yml", file.path(path, "orderly_config.yml"),
            overwrite = TRUE)
  path_example <- file.path(path, "src", "example")
  # grab the current report yml without required fields
  # this will fail with the new orderly_config.yml
  yml_path <- file.path(path_example, "orderly.yml")
  minimal_yml <- readLines(yml_path)
  # get required fields out of config
  config <- orderly_config(path)
  req_fields <- config$fields$name[config$fields$required]
  # add the second required to the yml with the wrong type
  minimal_yml <- c(minimal_yml, sprintf("%s: %s", req_fields[1], "character"))
  minimal_yml <- c(minimal_yml, sprintf("%s: %s", req_fields[2], 1))
  writeLines(minimal_yml, yml_path)
  
  # first required field wont give an error, the second will
  err_msg <- sprintf("'.*orderly.yml:%s' must be character", req_fields[2])
  expect_error(orderly_run("example", config = path, id_file = tmp, 
                           echo = FALSE),
               regexp = err_msg)
})


test_that("commit failure", {
  path <- prepare_orderly_example("minimal")
  on.exit(unlink(path, recursive = TRUE))
  expect_error(orderly_commit(new_report_id(), "example", path),
               "Did not find draft report example/")
})


test_that("can't commit failed run", {
  path <- prepare_orderly_example("minimal")
  on.exit(unlink(path, recursive = TRUE))

  append_lines('stop("some error")',
               file.path(path, "src", "example", "script.R"))
  expect_error(orderly_run("example", config = path, echo = FALSE),
               "some error")
  id <- dir(file.path(path, "draft", "example"))

  expect_error(orderly_commit(id, config = path),
               "Did not find run metadata file for example/")
})


test_that("can't commit report twice", {
  path <- prepare_orderly_example("minimal")
  on.exit(unlink(path, recursive = TRUE))

  id <- orderly_run("example", config = path, echo = FALSE)
  dir.create(file.path(path, "archive", "example", id), FALSE, TRUE)
  expect_error(orderly_commit(id, config = path),
               "Report example/.* appears to have already been copied")
})


test_that("open after run", {
  mock <- mockery::mock(TRUE)
  mockery::stub(orderly_run, "open_directory", mock)

  path <- prepare_orderly_example("minimal")
  id <- orderly_run("example", config = path, echo = FALSE, open = TRUE)

  expect_equal(length(mock), 1)
  args <- mockery::mock_args(mock)[[1]]
  expect_equal(length(args)[[1]], 1)
  expect_equal(normalizePath(args[[1]]),
               normalizePath(file.path(path, "draft", "example", id)))
})
