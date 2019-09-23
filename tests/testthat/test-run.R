context("run")

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
  p <- file.path(path_draft(config$root), res$name, res$id)
  files <- dir(p)
  expect_true(file.exists(file.path(p, "orderly.yml")))
  expect_true(file.exists(file.path(p, "orderly_run.rds")))
  expect_true(file.exists(file.path(p, "script.R")))
  expect_true(file.exists(file.path(p, "mygraph.png")))

  recipe_commit(p, config)
})

test_that("orderly_data", {
  path <- prepare_orderly_example("minimal")
  on.exit(unlink(path, recursive = TRUE))

  d <- orderly_data("example", root = path)
  expect_is(d, "environment")
  expect_is(d$dat, "data.frame")

  e1 <- new.env(parent = baseenv())
  e <- orderly_data("example", root = path, envir = e1)
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
  config <- orderly_config(path)
  expect_error(recipe_run(info, NULL, envir, config = config, echo = FALSE),
               "Report closed 1 more devices than it opened")
})

test_that("sink imbalance", {
  path <- prepare_orderly_example("minimal")
  config <- orderly_config(path)
  path_script <- file.path(path, "src/example/script.R")
  txt <- readLines(path_script)
  writeLines(c("sink('somefile', split = TRUE)", txt), path_script)

  expect_error(
    orderly_run("example", root = path, echo = FALSE),
    "Report left 1 sink open")

  logfile <- tempfile()
  expect_error(
    capture_log(
      orderly_run("example", root = path, echo = FALSE), logfile),
    "Report left 1 sink open")

  p <- tempfile()
  sink(p, split = TRUE)
  writeLines(c("sink()", txt), path_script)
  withr::with_options(
    list(orderly.nolog = TRUE),
    expect_error(
      capture_log(
        orderly_run("example", root = path, echo = FALSE), logfile),
      "Report closed 1 more sinks than it opened!"))
  expect_equal(sink.number(), 0)
})


test_that("leave connection open", {
  ## The issue here is that a garbage collection *might* occur causing
  ## the connection to close, but that is not guaranteed!  So we pop
  ## the connection into the environment so that it will not be
  ## garbage collected.
  e <- new.env(parent = .GlobalEnv)
  path <- prepare_orderly_example("minimal")
  config <- orderly_config(path)
  path_script <- file.path(path, "src/example/script.R")

  writeLines(
    'con <- file("mygraph.png", "w")',
    path_script)

  expect_error(
    orderly_run("example", root = path, echo = FALSE, envir = e),
    "File left open: mygraph.png")

  ## And check we can recover!
  close(e$con)

  writeLines(
    c('con <- file("mygraph.png", "w")', 'close(con)'),
    path_script)

  e <- new.env(parent = .GlobalEnv)
  expect_error(
    orderly_run("example", root = path, echo = FALSE, envir = e),
    NA)
})


test_that("included example", {
  path <- prepare_orderly_example("example")
  id <- orderly_run("example", list(cyl = 4), root = path, echo = FALSE)
  p <- orderly_commit(id, root = path)
  expect_true(is_directory(p))
  con <- orderly_db("destination", root = path)
  on.exit(DBI::dbDisconnect(con))
  dat <- DBI::dbReadTable(con, "report_version")
  expect_equal(dat$description, NA_character_)
  expect_equal(dat$displayname, NA_character_)
})

test_that("included other", {
  path <- prepare_orderly_example("other")
  id <- orderly_run("other", list(nmin = 0), root = path, echo = FALSE)
  p <- orderly_commit(id, root = path)
  info <- recipe_read(file.path(path_src(path), "other"),
                      orderly_config(path))
  con <- orderly_db("destination", root = path)
  on.exit(DBI::dbDisconnect(con))
  dat <- DBI::dbReadTable(con, "report_version")
  expect_equal(dat$description, info$description)
  expect_equal(dat$displayname, info$displayname)
})

test_that("connection", {
  path <- prepare_orderly_example("minimal")
  on.exit(unlink(path, recursive = TRUE))

  path_example <- file.path(path, "src", "example")
  yml <- file.path(path_example, "orderly.yml")
  txt <- readLines(yml)
  dat <- list(connection = list(con = "source"))
  writeLines(c(txt, yaml::as.yaml(dat)), yml)

  config <- orderly_config(path)
  info <- recipe_read(path_example, config)
  expect_identical(info$connection, list("con" = "source"))

  data <- orderly_data("example",
                       envir = new.env(parent = .GlobalEnv),
                       root = path)
  expect_is(data$con, "SQLiteConnection")
  expect_is(DBI::dbReadTable(data$con, "data"), "data.frame")
  DBI::dbDisconnect(data$con)
})


test_that("connection is saved to db", {
  path <- prepare_orderly_example("minimal")

  id1 <- orderly_run("example", root = path, echo = FALSE)
  orderly_commit(id1, root = path)

  path_example <- file.path(path, "src", "example")
  yml <- file.path(path_example, "orderly.yml")
  txt <- readLines(yml)
  dat <- list(connection = list(con = "source"))
  writeLines(c(txt, yaml::as.yaml(dat)), yml)

  id2 <- orderly_run("example", root = path, echo = FALSE)
  orderly_commit(id2, root = path)

  con <- orderly_db("destination", root = path)
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
                       root = path)
  expect_equal(ls(data, all.names = TRUE), character(0))

  id <- orderly_run("example", root = path, echo = FALSE)
  p <- file.path(path_draft(path), "example", id, "data.rds")
  expect_true(file.exists(p))
  expect_equal(readRDS(p), mtcars)
})

test_that("use artefact", {
  path <- prepare_orderly_example("depends")

  path_example <- file.path(path, "src", "example")
  path_depend <- file.path(path, "src", "depend")
  id1 <- orderly_run("example", root = path, echo = FALSE)
  path_orig <- file.path(path_draft(path), "example", id1, "data.rds")
  expect_true(file.exists(path_orig))

  data <- orderly_data("depend",
                       envir = new.env(parent = .GlobalEnv),
                       root = path)
  expect_identical(ls(data), character(0))
  id2 <- orderly_run("depend", root = path, echo = FALSE)
  path_previous <- file.path(path_draft(path), "depend", id2, "previous.rds")
  expect_true(file.exists(path_previous))
  expect_equal(hash_files(path_previous, FALSE),
               hash_files(path_orig, FALSE))

  d <-
    readRDS(path_orderly_run_rds(file.path(path_draft(path), "depend", id2)))
  expect_equal(d$meta$depends$hash, hash_files(path_previous, FALSE))

  ## Then rebuild the original:
  id3 <- orderly_run("example", root = path, echo = FALSE)
  id4 <- orderly_run("depend", root = path, echo = FALSE)
  path_orig2 <- file.path(path_draft(path), "example", id3, "data.rds")
  path_previous2 <- file.path(path_draft(path), "depend", id4, "previous.rds")

  expect_equal(hash_files(path_previous2, FALSE),
               hash_files(path_orig2, FALSE))
  expect_true(hash_files(path_previous2, FALSE) !=
              hash_files(path_previous, FALSE))

  ## Then we need to commit things and check that it all still works OK.
  expect_error(orderly_commit(id2, root = path),
               "Report uses draft id - commit first")
  p1 <- orderly_commit(id1, root = path)
  p2 <- orderly_commit(id2, root = path)
  expect_error(orderly_commit(id4, root = path), id3)
  p3 <- orderly_commit(id3, root = path)
  p4 <- orderly_commit(id4, root = path)
})

test_that("Can't commit report using nonexistant id", {
  path <- prepare_orderly_example("depends")
  id1 <- orderly_run("example", root = path, echo = FALSE)
  id2 <- orderly_run("depend", root = path, echo = FALSE)
  unlink(file.path(path, "draft", "example", id1), recursive = TRUE)
  expect_error(orderly_commit(id2, root = path),
               "Report uses nonexistant id")
})

test_that("resources", {
  path <- prepare_orderly_example("resources")
  id <- orderly_run("use_resource", root = path, echo = FALSE)
  p <- file.path(path, "draft", "use_resource", id)
  expect_true(file.exists(file.path(p, "meta/data.csv")))
  p <- orderly_commit(id, root = path)

  h <- hash_files(file.path(path, "src", "use_resource", "meta", "data.csv"), FALSE)

  con <- orderly_db("destination", root = path)
  d <- DBI::dbGetQuery(
    con, "SELECT * FROM file_input WHERE file_purpose = 'resource'")

  expect_identical(d$filename, "meta/data.csv")
  expect_identical(d$file_hash, h)
  expect_true(file.exists(file.path(p, "meta/data.csv")))
})


test_that("markdown", {
  path <- prepare_orderly_example("knitr")

  id <- orderly_run("example", root = path, echo = FALSE)

  report <- file.path(path, "draft", "example", id, "report.html")
  expect_true(file.exists(report))
  expect_true(any(grepl("ANSWER:2", readLines(report))))
})

test_that("database is not loaded unless needed", {
  vars <- c(SOME_ENVVAR = "source.sqlite")
  path <- withr::with_envvar(vars, prepare_orderly_example("nodb"))

  expect_identical(as.list(orderly_data("example", root = path)), list())
  id <- orderly_run("example", root = path, echo = FALSE)
  expect_true(
    file.exists(file.path(path, "draft", "example", id, "mygraph.png")))
  expect_error(orderly_db("source", path), "SOME_ENVVAR")
})

test_that("id file", {
  path <- prepare_orderly_example("minimal")
  tmp <- tempfile()
  id <- orderly_run("example", root = path, id_file = tmp, echo = FALSE)
  expect_true(file.exists(tmp))
  expect_equal(readLines(tmp), id)
})

test_that("test_start, test_restart", {
  owd <- getwd()
  on.exit(setwd(owd))

  path <- prepare_orderly_example("minimal")
  orderly_test_start("example", root = path)

  expect_equal(normalizePath(dirname(getwd())),
               normalizePath(file.path(path, "draft/example")))
  id <- basename(getwd())
  expect_equal(orderly_list_drafts(path)$id, id)

  expect_error(orderly_test_start("example", root = path),
               "Already running in test mode")

  orderly_test_restart()
  id2 <- basename(getwd())
  expect_false(id2 == id)
  expect_equal(orderly_list_drafts(path)$id, id2)

  orderly_test_end()
  expect_equal(getwd(), owd)
  expect_error(orderly_test_end(), "Not running in test mode")
  expect_error(orderly_test_restart(), "Not running in test mode")
})

test_that("test mode artefacts", {
  owd <- getwd()
  on.exit(setwd(owd))

  path <- prepare_orderly_example("minimal")
  orderly_test_start("example", root = path)
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
  id <- orderly_run("example", root = path, echo = FALSE,
                    message = test_message)
  p <- orderly_commit(id, root = path)

  d <- readRDS(path_orderly_run_rds(p))$meta$changelog
  expect_equal(d[names(d) != "id"],
               data_frame(
                 label = "label1", value = "test", from_file = FALSE,
                 report_version = id))
  expect_match(d$id, "^[[:xdigit:]]{32}")
})


test_that("no unexpected artefact", {
  path <- prepare_orderly_example("minimal")
  tmp <- tempfile()
  path_example <- file.path(path, "src", "example")
  # we're not expecting an 'unexpected' message at this point
  # grab all messages...
  messages <- capture_messages(orderly_run("example", root = path,
                                           id_file = tmp, echo = FALSE))
  # ...make sure none of the messages contain "unexpected"
  expect_false(any(grep("unexpected", messages)))
})


test_that("renamed dependencies are expected", {
  path <- prepare_orderly_example("depends")
  orderly_run("example", root = path, echo = FALSE)
  messages <- capture_messages(
    orderly_run("depend", root = path, echo = FALSE))
  expect_false(any(grep("unexpected", messages)))
})


test_that("non-existent package", {
  # if logging is on this test will wait for user input, so turn off
  orderly_log_off()
  on.exit(orderly_log_on())
  path <- prepare_orderly_example("minimal")
  tmp <- tempfile()
  path_example <- file.path(path, "src", "example")
  yml_path <- file.path(path_example, "orderly.yml")

  # add a non-existent package to the yaml
  write(sprintf("packages: %s", "non_existent_package"),
        file = yml_path, append = TRUE)
  # has orderly detected that the package does not exist>
  expect_error(orderly_run("example", root = path, id_file = tmp,
                           echo = FALSE),
               "Missing packages: 'non_existent_package'")
})

test_that("multiple non-existent packages", {
  # if logging is on this test will wait for user input, so turn off
  orderly_log_off()
  on.exit(orderly_log_on())
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
  expect_error(orderly_run("example", root = path, id_file = tmp,
                           echo = FALSE),
               paste("Missing packages:",
                     "'non_existent_package', 'non_existent_package_2'"))
})

test_that("use multiple versions of an artefact", {
  path <- prepare_orderly_example("depends")

  id1 <- orderly_run("example", root = path, echo = FALSE)
  id2 <- orderly_run("example", root = path, echo = FALSE)
  orderly_commit(id2, root = path)

  id3 <- orderly_run("depend2", root = path, echo = FALSE)

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

  id <- orderly_run("example", root = path, id_file = tmp, echo = FALSE)
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
      expect_error(orderly_run("example", root = path, id_file = tmp,
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
  expect_error(orderly_run("example", root = path, id_file = tmp,
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
  expect_error(orderly_run("example", root = path, echo = FALSE),
               "some error")
  id <- dir(file.path(path, "draft", "example"))

  expect_error(orderly_commit(id, root = path),
               "Did not find run metadata file for example/")
})


test_that("can't commit report twice", {
  path <- prepare_orderly_example("minimal")
  on.exit(unlink(path, recursive = TRUE))

  id <- orderly_run("example", root = path, echo = FALSE)
  dir.create(file.path(path, "archive", "example", id), FALSE, TRUE)
  expect_error(orderly_commit(id, root = path),
               "Report example/.* appears to have already been copied")
})


test_that("missing parameters throws an error", {
  path <- prepare_orderly_example("example")
  on.exit(unlink(path, recursive = TRUE))

  expect_error(orderly_run("example", root = path),
               "Missing parameters: 'cyl'")
  expect_error(orderly_run("example", list(cl = 2), root = path),
               "Missing parameters: 'cyl'")
})


test_that("orderly_environment", {
  expect_identical(orderly_environment(.GlobalEnv), .GlobalEnv)
  e <- orderly_environment(NULL)
  expect_identical(parent.env(e), .GlobalEnv)
  expect_identical(orderly_environment(e), e)
  expect_error(orderly_environment(list()), "'envir' must be an environment")
})

test_that("modify one resource", {
  path <- prepare_orderly_example("resources")
  path_example <- file.path(path, "src", "use_resource")
  script_path <- file.path(path_example, "script.R")

  ## add a line to script.R that modifies a resource
  write(sprintf("write.csv(x = c(1, 2, 3), file = 'meta/data.csv')"),
        file = script_path, append = TRUE)

  expect_error(
    orderly_run("use_resource", root = path, echo = FALSE),
    "Script has modified input: meta/data.csv")
})


test_that("modify multiple resources", {
  path <- prepare_orderly_example("resources")
  path_example <- file.path(path, "src", "multiple_resources")
  script_path <- file.path(path_example, "script.R")

  write(sprintf("write.csv(x = c(1, 2, 3), file = 'meta/data.csv')"),
        file = script_path, append = TRUE)
  write(sprintf("write.csv(x = c('Hello', 'World'), file = 'meta/data2.csv')"),
        file = script_path, append = TRUE)

  expect_error(
    orderly_run("multiple_resources", root = path, echo = FALSE),
    "Script has modified inputs: meta/data.csv, meta/data2.csv")
})


test_that("delete a resource", {
  ## delete 1 resource
  path <- prepare_orderly_example("resources")
  path_example <- file.path(path, "src", "use_resource")
  script_path <- file.path(path_example, "script.R")

  ## add a line to script.R that deletes a resource
  write(sprintf("file.remove('meta/data.csv')"),
        file = script_path, append = TRUE)

  expect_error(
    orderly_run("use_resource", root = path, echo = FALSE),
    "Script deleted input: meta/data.csv")
})


test_that("delete multiple resources", {
  path <- prepare_orderly_example("resources")
  path_example <- file.path(path, "src", "multiple_resources")
  script_path <- file.path(path_example, "script.R")

  # add a line to script.R that modifies a resource
  write(sprintf("file.remove('meta/data.csv')"),
        file = script_path, append = TRUE)
  write(sprintf("file.remove('meta/data2.csv')"),
        file = script_path, append = TRUE)

  error_message <-
    sprintf("Script deleted inputs: %s, %s",
            "meta/data.csv", "meta/data2.csv")
  expect_error(
    orderly_run("multiple_resources", root = path, echo = FALSE),
    error_message)
})

test_that("multiple resources", {
  path <- prepare_orderly_example("resources")
  id <- orderly_run("multiple_resources", root = path, echo = FALSE)
  p <- file.path(path, "draft", "multiple_resources", id)
  expect_true(file.exists(file.path(p, "meta/data.csv")))
  expect_true(file.exists(file.path(p, "meta/data2.csv")))
  p <- orderly_commit(id, root = path)

  h1 <- hash_files(file.path(path, "src", "multiple_resources", "meta", "data.csv"), FALSE)
  h2 <- hash_files(file.path(path, "src", "multiple_resources", "meta", "data2.csv"), FALSE)

  con <- orderly_db("destination", root = path)
  on.exit(DBI::dbDisconnect(con))
  d <- DBI::dbReadTable(con, "file_input")
  d <- d[d$file_purpose == "resource", ]

  expect_identical(d$filename, c("meta/data.csv", "meta/data2.csv"))
  expect_identical(d$file_hash, c(h1, h2))
  expect_true(file.exists(file.path(p, "meta/data.csv")))
  expect_true(file.exists(file.path(p, "meta/data2.csv")))
})


test_that("producing a directory is an error", {
  path <- prepare_orderly_example("minimal")
  on.exit(unlink(path, recursive = TRUE))

  writeLines(
    'dir.create("mygraph.png")',
    file.path(path, "src", "example", "script.R"))
  expect_error(orderly_run("example", root = path, echo = FALSE),
               "Produced a directory artefact: 'mygraph.png'",
               fixed = TRUE)
})


test_that("can run report with a view", {
  path <- prepare_orderly_example("demo")
  id <- orderly_run("view", root = path, echo = FALSE)
  orderly_commit(id, root = path)
  con <- orderly_db("destination", root = path)
  on.exit(DBI::dbDisconnect(con))
  res <- DBI::dbReadTable(con, "report_version_view")
  expect_equal(res$database, "source")
})


test_that("can run a report from orderly with no database", {
  path <- prepare_orderly_example("db0")
  id <- orderly_run("example", root = path, echo = FALSE)
  expect_true(file.exists(
    file.path(path, "draft", "example", id, "mygraph.png")))
  p <- orderly_commit(id, root = path)
  expect_true(file.exists(file.path(p, "mygraph.png")))
})


test_that("can run a report from orderly with one (named) database", {
  path <- prepare_orderly_example("db1")
  id <- orderly_run("example", root = path, echo = FALSE)
  expect_true(file.exists(
    file.path(path, "draft", "example", id, "mygraph.png")))
  p <- orderly_commit(id, root = path)
  expect_true(file.exists(file.path(p, "mygraph.png")))
})


test_that("can run a report from orderly with two databases", {
  path <- prepare_orderly_example("db2")
  id <- orderly_run("example", root = path, echo = FALSE)
  expect_true(file.exists(
    file.path(path, "draft", "example", id, "mygraph.png")))
  p <- orderly_commit(id, root = path)
  expect_true(file.exists(file.path(p, "mygraph.png")))
})


test_that("Can use connections with two databases", {
  path <- prepare_orderly_example("db2")
  id <- orderly_run("connection", root = path, echo = FALSE)
  expect_true(file.exists(
    file.path(path, "draft", "connection", id, "mygraph.png")))
  p <- orderly_commit(id, root = path)
  expect_true(file.exists(file.path(p, "mygraph.png")))
})


test_that("prevent duplicate filenames", {
  path <- prepare_orderly_example("depends")
  id1 <- orderly_run("example", root = path, echo = FALSE)

  p <- file.path(path, "src", "depend", "orderly.yml")
  d <- yaml_read(p)
  d$resources <- "previous.rds"
  yaml_write(d, p)
  file.create(file.path(path, "src", "depend", "previous.rds"))

  expect_error(
    orderly_run("depend", root = path, echo = FALSE),
    "Orderly configuration implies duplicate files:\\s+- previous.rds:")
})
