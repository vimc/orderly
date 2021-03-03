context("run")

test_that("minimal", {
  path <- prepare_orderly_example("minimal")
  on.exit(unlink(path, recursive = TRUE))

  envir <- orderly_environment(NULL)
  id <- orderly_run("example", envir = envir, root = path, echo = FALSE)
  expect_is(id, "character")
  expect_match(id, "^[0-9]{8}-[0-9]{6}-[[:xdigit:]]{8}$")
  expect_equal(ls(envir), "dat")
  expect_is(envir$dat, "data.frame")

  p <- orderly_commit(id, root = path)
  expect_true(file.exists(p))
  expect_true(same_path(p, file.path(path, "archive", "example", id)))
  expect_setequal(
    dir(p),
    c("orderly.yml", "orderly_run.rds", "script.R", "mygraph.png"))
})


test_that("fail to create artefact", {
  path <- prepare_orderly_example("minimal")
  on.exit(unlink(path, recursive = TRUE))
  writeLines("1 + 1", file.path(path, "src/example/script.R"))
  expect_error(orderly_run("example", root = path, echo = FALSE),
               "Script did not produce expected artefacts: mygraph.png")
})

test_that("leave device open", {
  path <- prepare_orderly_example("minimal")
  on.exit(unlink(path, recursive = TRUE))
  txt <- readLines(file.path(path, "src/example/script.R"))
  writeLines(txt[!grepl("dev.off()", txt, fixed = TRUE)],
             file.path(path, "src/example/script.R"))
  expect_error(orderly_run("example", root = path, echo = FALSE),
               "Report left 1 device open")
  expect_false(file.exists("mygraph.png"))
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

  txt <- readLines(file.path(path, "src/example/script.R"))
  writeLines(c(txt, "dev.off()"), file.path(path, "src/example/script.R"))
  expect_error(orderly_run("example", root = path, echo = FALSE),
               "Report closed 1 more devices than it opened")
})

test_that("sink imbalance", {
  skip_on_cran()
  skip_on_cran_windows()
  path <- prepare_orderly_example("minimal")
  config <- orderly_config_$new(path)
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
  sink(p, split = FALSE)
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
  config <- orderly_config_$new(path)
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
    c('con <- file("mygraph.png", "w")', "close(con)"),
    path_script)

  e <- new.env(parent = .GlobalEnv)
  expect_error(
    orderly_run("example", root = path, echo = FALSE, envir = e),
    NA)
})


test_that("connection is saved to db", {
  skip_on_cran_windows()
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

  id <- orderly_run("example", root = path, echo = FALSE)
  p <- file.path(path_draft(path), "example", id, "data.rds")
  expect_true(file.exists(p))
  expect_equal(readRDS(p), mtcars)
})

test_that("use artefact", {
  skip_on_cran_windows()
  path <- prepare_orderly_example("depends", testing = TRUE)

  path_example <- file.path(path, "src", "example")
  path_depend <- file.path(path, "src", "depend")
  id1 <- orderly_run("example", root = path, echo = FALSE)
  path_orig <- file.path(path_draft(path), "example", id1, "data.rds")
  expect_true(file.exists(path_orig))

  id2 <- orderly_run("depend", root = path, echo = FALSE, use_draft = TRUE)
  path_previous <- file.path(path_draft(path), "depend", id2, "previous.rds")
  expect_true(file.exists(path_previous))
  expect_equal(hash_files(path_previous, FALSE),
               hash_files(path_orig, FALSE))

  d <-
    readRDS(path_orderly_run_rds(file.path(path_draft(path), "depend", id2)))
  expect_equal(d$meta$depends$hash, hash_files(path_previous, FALSE))

  ## Then rebuild the original:
  id3 <- orderly_run("example", root = path, echo = FALSE)
  id4 <- orderly_run("depend", root = path, echo = FALSE, use_draft = TRUE)
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
  skip_on_cran_windows()
  path <- prepare_orderly_example("depends", testing = TRUE)
  id1 <- orderly_run("example", root = path, echo = FALSE)
  id2 <- orderly_run("depend", root = path, echo = FALSE, use_draft = TRUE)
  unlink(file.path(path, "draft", "example", id1), recursive = TRUE)
  expect_error(orderly_commit(id2, root = path),
               "Report uses nonexistant id")
})

test_that("resources", {
  skip_on_cran_windows()
  path <- prepare_orderly_example("resources", testing = TRUE)
  id <- orderly_run("use_resource", root = path, echo = FALSE)
  p <- file.path(path, "draft", "use_resource", id)
  expect_true(file.exists(file.path(p, "meta/data.csv")))
  p <- orderly_commit(id, root = path)

  h <- hash_files(
    file.path(path, "src", "use_resource", "meta", "data.csv"), FALSE)

  con <- orderly_db("destination", root = path)
  d <- DBI::dbGetQuery(
    con, "SELECT * FROM file_input WHERE file_purpose = 'resource'")

  expect_identical(d$filename, "meta/data.csv")
  expect_identical(d$file_hash, h)
  expect_true(file.exists(file.path(p, "meta/data.csv")))
})


test_that("markdown", {
  skip_on_cran_windows()
  path <- prepare_orderly_example("demo")

  id <- orderly_run("html", root = path, echo = FALSE)

  report <- file.path(path, "draft", "html", id, "myreport.html")
  expect_true(file.exists(report))
  expect_true(any(grepl("ANSWER:2", readLines(report))))
})

test_that("database is not loaded unless needed", {
  vars <- c(SOME_ENVVAR = "source.sqlite")
  path <- withr::with_envvar(
    vars,
    prepare_orderly_example("nodb", testing = TRUE))

  id <- orderly_run("example", root = path, echo = FALSE)
  expect_true(
    file.exists(file.path(path, "draft", "example", id, "mygraph.png")))
  expect_error(orderly_db("source", path), "SOME_ENVVAR")
})

test_that("id file", {
  path <- prepare_orderly_example("minimal")
  tmp <- tempfile()
  id <- orderly_run_internal("example", root = path, id_file = tmp,
                             echo = FALSE)
  expect_true(file.exists(tmp))
  expect_equal(readLines(tmp), id)
})


test_that("test mode artefacts", {
  owd <- getwd()
  on.exit(setwd(owd))

  path <- prepare_orderly_example("minimal")
  p <- orderly_test_start("example", root = path)

  expect_false(orderly_test_check(p))

  writeLines(character(0), file.path(p, "mygraph.png"))
  expect_true(orderly_test_check(p))
})


test_that("orderly_test_check requires test mode", {
  p <- tempfile()
  dir.create(p)
  on.exit(unlink(p, recursive = TRUE))
  expect_error(orderly_test_check(p), "Not running in test mode")
})


test_that("run with message", {
  skip_on_cran_windows()
  path <- prepare_orderly_example("changelog", testing = TRUE)
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
  path_example <- file.path(path, "src", "example")
  # we're not expecting an 'unexpected' message at this point
  # grab all messages...
  messages <- capture_messages(orderly_run("example", root = path,
                                           echo = FALSE))
  # ...make sure none of the messages contain "unexpected"
  expect_false(any(grep("unexpected", messages)))
})


test_that("renamed dependencies are expected", {
  skip_on_cran_windows()
  path <- prepare_orderly_example("depends", testing = TRUE)
  orderly_run("example", root = path, echo = FALSE)
  messages <- capture_messages(
    orderly_run("depend", root = path, echo = FALSE, use_draft = TRUE))
  expect_false(any(grep("unexpected", messages)))
})


test_that("non-existent package", {
  # if logging is on this test will wait for user input, so turn off
  orderly_log_off()
  on.exit(orderly_log_on())
  path <- prepare_orderly_example("minimal")
  path_example <- file.path(path, "src", "example")
  yml_path <- file.path(path_example, "orderly.yml")

  # add a non-existent package to the yaml
  write(sprintf("packages: %s", "non_existent_package"),
        file = yml_path, append = TRUE)
  # has orderly detected that the package does not exist>
  expect_error(orderly_run("example", root = path, echo = FALSE),
               "Missing packages: 'non_existent_package'")
})

test_that("multiple non-existent packages", {
  # if logging is on this test will wait for user input, so turn off
  orderly_log_off()
  on.exit(orderly_log_on())
  path <- prepare_orderly_example("minimal")
  path_example <- file.path(path, "src", "example")
  yml_path <- file.path(path_example, "orderly.yml")

  # add a non-existent package to the yaml
  write(sprintf("packages: \n  - %s", "non_existent_package"),
        file = yml_path, append = TRUE)
  write(sprintf("  - %s", "non_existent_package_2"),
        file = yml_path, append = TRUE)
  # has orderly detected that the package does not exist>
  expect_error(orderly_run("example", root = path, echo = FALSE),
               paste("Missing packages:",
                     "'non_existent_package', 'non_existent_package_2'"))
})

test_that("use multiple versions of an artefact", {
  skip_on_cran_windows()
  path <- prepare_orderly_example("depends", testing = TRUE)

  id1 <- orderly_run("example", root = path, echo = FALSE)
  id2 <- orderly_run("example", root = path, echo = FALSE)
  orderly_commit(id1, root = path)
  orderly_commit(id2, root = path)

  p <- file.path(path, "src", "depend2", "orderly.yml")
  dat <- yaml_read(p)
  dat$depends[[1]]$example$id <- id1
  dat$depends[[2]]$example$id <- id2
  yaml_write(dat, p)

  id3 <- orderly_run("depend2", root = path, echo = FALSE)

  p1 <- file.path(path, "draft", "depend2", id3,
                  c("previous1.rds", "previous2.rds"))
  expect_true(all(file.exists(p1)))

  p2 <- file.path(path, "archive", "example", c(id1, id2), "data.rds")
  expect_equal(hash_files(p1, FALSE),
               hash_files(p2, FALSE))
})

test_that("required field OK", {
  path <- prepare_orderly_example("minimal")
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
  config <- orderly_config_$new(path)
  req_fields <- config$fields$name[config$fields$required]
  # set required fields to correct type
  minimal_yml <- c(minimal_yml, sprintf("%s: %s", req_fields[1], "character"))
  minimal_yml <- c(minimal_yml, sprintf("%s: %s", req_fields[2], "character"))
  writeLines(minimal_yml, yml_path)

  id <- orderly_run("example", root = path, echo = FALSE)
  p <- file.path(path_draft(path), "example", id, "mygraph.png")
  expect_true(file.exists(p))
})

test_that("missing required field", {
  path <- prepare_orderly_example("minimal")
  # we need to use an orderly config with required fields set so copy it over
  file.copy("example_config.yml", file.path(path, "orderly_config.yml"),
            overwrite = TRUE)
  path_example <- file.path(path, "src", "example")
  # grab the current report yml without required fields
  # this will fail with the new orderly_config.yml
  yml_path <- file.path(path_example, "orderly.yml")
  minimal_yml <- readLines(yml_path)
  # get required fields out of config
  config <- orderly_config_$new(path)
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
      err_msg <- sprintf("Fields missing from .*: '%s'", missing_required)
      expect_error(orderly_run("example", root = path, echo = FALSE),
                   regexp = err_msg)
    }
  }
})

test_that("required field wrong type", {
  path <- prepare_orderly_example("minimal")
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
  config <- orderly_config_$new(path)
  req_fields <- config$fields$name[config$fields$required]
  # add the second required to the yml with the wrong type
  minimal_yml <- c(minimal_yml, sprintf("%s: %s", req_fields[1], "character"))
  minimal_yml <- c(minimal_yml, sprintf("%s: %s", req_fields[2], 1))
  writeLines(minimal_yml, yml_path)

  # first required field wont give an error, the second will
  err_msg <- sprintf("'.*orderly.yml:%s' must be character", req_fields[2])
  expect_error(orderly_run("example", root = path, echo = FALSE),
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
  skip_on_cran_windows()
  path <- prepare_orderly_example("demo")
  on.exit(unlink(path, recursive = TRUE))

  expect_error(orderly_run("other", root = path),
               "Missing parameters: 'nmin'")
  expect_error(orderly_run("other", list(cl = 2), root = path),
               "Missing parameters: 'nmin'")
})


test_that("orderly_environment", {
  expect_identical(orderly_environment(.GlobalEnv), .GlobalEnv)
  e <- orderly_environment(NULL)
  expect_identical(parent.env(e), .GlobalEnv)
  expect_identical(orderly_environment(e), e)
  expect_error(orderly_environment(list()), "'envir' must be an environment")
})

test_that("modify one resource", {
  path <- prepare_orderly_example("resources", testing = TRUE)
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
  path <- prepare_orderly_example("resources", testing = TRUE)
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
  path <- prepare_orderly_example("resources", testing = TRUE)
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
  path <- prepare_orderly_example("resources", testing = TRUE)
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
  skip_on_cran_windows()
  path <- prepare_orderly_example("resources", testing = TRUE)
  id <- orderly_run("multiple_resources", root = path, echo = FALSE)
  p <- file.path(path, "draft", "multiple_resources", id)
  expect_true(file.exists(file.path(p, "meta/data.csv")))
  expect_true(file.exists(file.path(p, "meta/data2.csv")))
  p <- orderly_commit(id, root = path)

  h1 <- hash_files(
    file.path(path, "src", "multiple_resources", "meta", "data.csv"), FALSE)
  h2 <- hash_files(
    file.path(path, "src", "multiple_resources", "meta", "data2.csv"), FALSE)

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
  skip_on_cran_windows()
  path <- prepare_orderly_example("demo")
  id <- orderly_run("view", root = path, echo = FALSE)
  orderly_commit(id, root = path)
  con <- orderly_db("destination", root = path)
  on.exit(DBI::dbDisconnect(con))
  res <- DBI::dbReadTable(con, "report_version_view")
  expect_equal(res$database, "source")
})


test_that("can run a report from orderly with no database", {
  skip_on_cran_windows()
  path <- prepare_orderly_example("db0", testing = TRUE)
  id <- orderly_run("example", root = path, echo = FALSE)
  expect_true(file.exists(
    file.path(path, "draft", "example", id, "mygraph.png")))
  p <- orderly_commit(id, root = path)
  expect_true(file.exists(file.path(p, "mygraph.png")))
})


test_that("can run a report from orderly with one (named) database", {
  skip_on_cran_windows()
  path <- prepare_orderly_example("db1", testing = TRUE)
  id <- orderly_run("example", root = path, echo = FALSE)
  expect_true(file.exists(
    file.path(path, "draft", "example", id, "mygraph.png")))
  p <- orderly_commit(id, root = path)
  expect_true(file.exists(file.path(p, "mygraph.png")))
})


test_that("can run a report from orderly with two databases", {
  skip_on_cran_windows()
  path <- prepare_orderly_example("db2", testing = TRUE)
  id <- orderly_run("example", root = path, echo = FALSE)
  expect_true(file.exists(
    file.path(path, "draft", "example", id, "mygraph.png")))
  p <- orderly_commit(id, root = path)
  expect_true(file.exists(file.path(p, "mygraph.png")))
})


test_that("Can use connections with two databases", {
  skip_on_cran_windows()
  path <- prepare_orderly_example("db2", testing = TRUE)
  id <- orderly_run("connection", root = path, echo = FALSE)
  expect_true(file.exists(
    file.path(path, "draft", "connection", id, "mygraph.png")))
  p <- orderly_commit(id, root = path)
  expect_true(file.exists(file.path(p, "mygraph.png")))
})


test_that("prevent duplicate filenames", {
  path <- prepare_orderly_example("depends", testing = TRUE)
  id1 <- orderly_run("example", root = path, echo = FALSE)

  p <- file.path(path, "src", "depend", "orderly.yml")
  d <- yaml_read(p)
  d$resources <- "previous.rds"
  yaml_write(d, p)
  file.create(file.path(path, "src", "depend", "previous.rds"))

  expect_error(
    orderly_run("depend", root = path, echo = FALSE, use_draft = TRUE),
    "Orderly configuration implies duplicate files:\\s+- previous.rds:")
})


test_that("allow src/ in report name during run", {
  path <- prepare_orderly_example("minimal")
  id <- orderly_run("src/example", root = path, echo = FALSE)
  expect_true(file.exists(file.path(path, "draft", "example", id)))
})


test_that("run with different database instance", {
  path <- prepare_orderly_example("depends", testing = TRUE)

  p <- file.path(path, "orderly_config.yml")
  writeLines(c(
    "database:",
    "  source:",
    "    driver: RSQLite::SQLite",
    "    instances:",
    "      default:",
    "        dbname: source.sqlite",
    "      alternative:",
    "        dbname: alternative.sqlite"),
    p)

  file.copy(file.path(path, "source.sqlite"),
            file.path(path, "alternative.sqlite"))

  con <- orderly_db("source", root = path, instance = "alternative")
  DBI::dbExecute(con$source, "DELETE from thing where id > 10")

  id1 <- orderly_run("example", root = path, echo = FALSE)
  id2 <- orderly_run("example", root = path, echo = FALSE,
                     instance = "default")
  id3 <- orderly_run("example", root = path, echo = FALSE,
                     instance = "alternative")

  f <- function(id) {
    nrow(readRDS(file.path(path, "draft", "example", id, "data.rds")))
  }
  expect_equal(f(id1), 20)
  expect_equal(f(id2), 20)
  expect_equal(f(id3), 10)

  g <- function(id) {
    d <- readRDS(file.path(path, "draft", "example", id, "orderly_run.rds"))
    d$meta$instance
  }
  expect_equal(g(id1), list(source = "default"))
  expect_equal(g(id2), list(source = "default"))
  expect_equal(g(id3), list(source = "alternative"))
})


test_that("Require simple parameters", {
  path <- prepare_orderly_example("parameters", testing = TRUE)

  expect_error(
    orderly_run("example", parameters = list(a = Sys.Date(), b = TRUE),
                root = path, echo = FALSE),
    "Invalid parameters: 'a' - must be character, numeric or logical")
  expect_error(
    orderly_run("example", parameters = list(a = NULL, b = 1:2),
                root = path, echo = FALSE),
    "Invalid parameters: 'a', 'b' - must be scalar")
})


test_that("preserve tags in metadata", {
  root <- prepare_orderly_example("minimal")
  append_lines(c("tags:", "  - tag1", "  - tag2"),
               file.path(root, "orderly_config.yml"))
  append_lines(c("tags:", "  - tag1"),
               file.path(root, "src", "example", "orderly.yml"))

  id <- orderly_run("example", root = root, echo = FALSE)
  d <- readRDS(path_orderly_run_rds(file.path(root, "draft", "example", id)))
  expect_equal(d$meta$tags, "tag1")
})


test_that("Pass tags during run", {
  root <- prepare_orderly_example("minimal")
  append_lines(c("tags:", "  - tag1", "  - tag2"),
               file.path(root, "orderly_config.yml"))
  append_lines(c("tags:", "  - tag1"),
               file.path(root, "src", "example", "orderly.yml"))

  ## Add new tag
  id <- orderly_run("example", root = root, echo = FALSE, tags = "tag2")
  d <- readRDS(path_orderly_run_rds(file.path(root, "draft", "example", id)))
  expect_equal(d$meta$tags, c("tag1", "tag2"))

  ## Ignore already present tag
  id <- orderly_run("example", root = root, echo = FALSE, tags = "tag1")
  d <- readRDS(path_orderly_run_rds(file.path(root, "draft", "example", id)))
  expect_equal(d$meta$tags, "tag1")

  ## Error on unknown tag
  expect_error(
    orderly_run("example", root = root, echo = FALSE, tags = "tag3"),
    "Unknown tag: 'tag3'")
  expect_error(
    orderly_run("example", root = root, echo = FALSE, tags = 1),
    "'tags' must be character")
})


test_that("orderly_envir is available during run", {
  path <- prepare_orderly_example("minimal")
  p <- file.path(path, "src", "example")
  writeLines(c("MY_A: a", "ORDERLY_B: b"), file.path(path, "orderly_envir.yml"))
  append_lines('writeLines(Sys.getenv("MY_A"), "env")',
               file.path(p, "script.R"))
  id <- orderly_run("example", root = path, echo = FALSE)

  p <- file.path(path, "draft", "example", id)
  expect_equal(readLines(file.path(p, "env")), "a")
  expect_equal(readRDS(path_orderly_run_rds(p))$env,
               list(ORDERLY_B = "b"))
})

test_that("Use secrets in report", {
  srv <- vaultr::vault_test_server()
  cl <- srv$client()
  cl$write("/secret/users/alice", list(password = "ALICE"))
  cl$write("/secret/users/bob", list(password = "BOB"))

  path <- prepare_orderly_example("minimal")
  append_lines(
    c("vault:",
      paste("  addr:", srv$addr),
      paste("  login: token"),
      paste("  token:", srv$token)),
    file.path(path, "orderly_config.yml"))

  append_lines(c("secrets:",
                 "  alice: /secret/users/alice:password",
                 "  bob: /secret/users/bob:password"),
               file.path(path, "src", "example", "orderly.yml"))

  append_lines(
    'writeLines(c(alice, bob), "passwords")',
    file.path(path, "src", "example", "script.R"))

  id <- orderly_run("example", root = path, echo = FALSE)
  expect_equal(
    readLines(file.path(path, "draft", "example", id, "passwords")),
    c("ALICE", "BOB"))
})

test_that("can use environment variables in report", {
  path <- prepare_orderly_example("minimal")

  append_lines(
    c("environment:",
      paste("  data_path: EXTRA_DATA_PATH"),
      paste("  example_var: EXAMPLE_VAR")),
    file.path(path, "src", "example", "orderly.yml"))

  append_lines(
    'writeLines(c(data_path, example_var), "env_vars")',
    file.path(path, "src", "example", "script.R"))

  expect_error(orderly_run("example", root = path),
               "Environment variable 'EXTRA_DATA_PATH' is not set
\t(used in orderly.yml:environment:data_path)", fixed = TRUE)

  ## On windows if env variable is empty then windows will return NA from call
  ## to Sys.getenv
  if (is_windows()) {
    expected_err <- "Environment variable 'EXAMPLE_VAR' is not set
\t(used in orderly.yml:environment:example_var)"
  } else {
    expected_err <- "Environment variable 'EXAMPLE_VAR' is empty
\t(used in orderly.yml:environment:example_var)"
  }

  data_path <- tempfile()
  withr::with_envvar(
    c("EXTRA_DATA_PATH" = data_path,
      "EXAMPLE_VAR" = ""),
    expect_error(orderly_run("example", root = path),
                 expected_err, fixed = TRUE)
  )

  withr::with_envvar(
    c("EXTRA_DATA_PATH" = data_path,
      "EXAMPLE_VAR" = "example value"),
    id <- orderly_run("example", root = path, echo = FALSE)
  )
  expect_equal(
    readLines(file.path(path, "draft", "example", id, "env_vars")),
    c(data_path, "example value"))
})


test_that("pick up name from the working directory", {
  skip_on_cran_windows()
  path <- prepare_orderly_example("minimal")

  id <- withr::with_dir(
    file.path(path, "src", "example"),
    orderly_run(echo = FALSE))
  expect_true(file.exists(file.path(path, "draft", "example", id)))
})

test_that("orderly_run can capture messages", {
  path <- prepare_orderly_example("minimal")
  on.exit(unlink(path, recursive = TRUE))

  id <- orderly_run_internal("example", root = path, echo = FALSE,
                             capture_log = TRUE, commit = TRUE)

  archive_logs <- path_orderly_log(file.path(path, "archive", "example", id))
  expect_true(file.exists(archive_logs))
  log <- readLines(archive_logs)
  expect_true("[ name       ]  example" %in% log)
  expect_true(any(grepl("^\\[ commit     \\]  .*", log)))
})

test_that("logs from failed runs can still be written to file", {
  path <- prepare_orderly_example("minimal")
  on.exit(unlink(path, recursive = TRUE))

  append_lines(
    c("f <- function() g()",
      "g <- function() h()",
      "h <- function() stop('some error')",
      "f()"),
    file.path(path, "src", "example", "script.R"))

  expect_error(orderly_run_internal("example", root = path, echo = FALSE,
                                    capture_log = TRUE),
               "some error")
  id <- dir(file.path(path, "draft", "example"))

  draft_logs <- file.path(path, "draft", "example", id, "orderly.log")
  expect_true(file.exists(draft_logs))
  log <- readLines(draft_logs)
  expect_true("[ name       ]  example" %in% log)

  ## Error is found
  expect_match(log, "Error: some error", all = FALSE)

  ## traceback preserved
  expect_match(log, "f()", all = FALSE, fixed = TRUE)
  expect_match(log, "g()", all = FALSE, fixed = TRUE)
})


test_that("run errors if given extra parameters", {
  path <- prepare_orderly_example("demo")
  expect_error(
    orderly_run("minimal", list(a = 1), root = path),
    "Extra parameters: 'a'")
  expect_error(
    orderly_run("other", list(nmin = 1, a = 1), root = path),
    "Extra parameters: 'a'")
})


test_that("parameters passed to dependency resolution include defaults", {
  dat <- prepare_orderly_query_example()
  root <- dat$root
  ids <- dat$ids

  config <- orderly_config_$new(root)

  p <- file.path(root, "src", "use_dependency", "orderly.yml")
  txt <- readLines(p)
  txt <- sub("latest", "latest(parameter:nmin < x)", txt, fixed = TRUE)
  txt <- c(txt, c("parameters:",
                  "  x:",
                  "    default: 0.25"))
  writeLines(txt, p)

  ## Use default
  id <- orderly_run("use_dependency", echo = FALSE, root = root)
  info <- readRDS(
    path_orderly_run_rds(file.path(root, "draft", "use_dependency", id)))
  expect_equal(info$meta$depends$id, dat$ids[[2]])

  ## Override default
  id <- orderly_run("use_dependency", list(x = 0.15), echo = FALSE, root = root)
  info <- readRDS(
    path_orderly_run_rds(file.path(root, "draft", "use_dependency", id)))
  expect_equal(info$meta$depends$id, dat$ids[[1]])
})

test_that("failed run creates failed rds", {
  path <- prepare_orderly_git_example()

  append_lines('stop("some error")',
               file.path(path[["local"]], "src", "minimal", "script.R"))
  expect_error(orderly_run("minimal", root = path[["local"]], echo = FALSE),
               "some error")
  drafts <- orderly_list_drafts(root = path[["local"]], include_failed = TRUE)
  expect_equal(nrow(drafts), 1)

  draft_dir <- file.path(path_draft(path[["local"]]), drafts$name, drafts$id)
  expect_true(file.exists(path_orderly_fail_rds(draft_dir)))

  failed_rds <- readRDS(path_orderly_fail_rds(draft_dir))
  expect_equal(names(failed_rds),
               c("session_info", "time", "env", "git", "error", "meta",
                 "archive_version"))

  expect_s3_class(failed_rds$error$error, "simpleError")
  expect_equal(failed_rds$error$error$message, "some error")
  expect_true(length(failed_rds$error$trace) > 5)
  expect_match(failed_rds$error$trace[length(failed_rds$error$trace)],
               'stop\\("some error"\\)')

  expect_equal(failed_rds$meta$id, drafts$id)
  expect_equal(failed_rds$meta$name, "minimal")
  expect_null(failed_rds$meta$parameters)
  expect_true(!is.null(failed_rds$meta$elapsed))

  expect_equal(failed_rds$archive_version, cache$current_archive_version)

  expect_equal(failed_rds$meta$git$branch, "master")
  expect_equal(failed_rds$meta$git$status, " M src/minimal/script.R")
})

test_that("fail during cleanup creates failed rds", {
  path <- prepare_orderly_example("minimal")
  on.exit(unlink(path, recursive = TRUE))

  writeLines("1 + 1", file.path(path, "src/example/script.R"))
  expect_error(orderly_run("example", root = path, echo = FALSE),
               "Script did not produce expected artefacts: mygraph.png")

  drafts <- orderly_list_drafts(root = path, include_failed = TRUE)
  expect_equal(nrow(drafts), 1)

  draft_dir <- file.path(path_draft(path), drafts$name, drafts$id)
  expect_true(file.exists(path_orderly_fail_rds(draft_dir)))

  failed_rds <- readRDS(path_orderly_fail_rds(draft_dir))
  expect_equal(names(failed_rds),
               c("session_info", "time", "env", "error", "meta",
                 "archive_version"))
  expect_equal(failed_rds$error$error$message,
               "Script did not produce expected artefacts: mygraph.png")
  expect_true(length(failed_rds$error$trace) > 5)
  expect_match(failed_rds$error$trace[length(failed_rds$error$trace)],
               "Script did not produce expected artefacts:")
})

test_that("message printed if run fails before working dir is set", {
  path <- prepare_orderly_git_example()
  ## git checkout happens before workdir is set, so we trigger an error there
  expect_message(expect_error(
    orderly_run_internal("minimal", root = path[["local"]],
                         echo = FALSE, fetch = TRUE, ref = "123")),
    "Can't save fail RDS, workdir not set")
})

test_that("orderly_run_internal writes fail rds on error", {
  path <- prepare_orderly_example("minimal")
  on.exit(unlink(path, recursive = TRUE))

  append_lines(
    c("f <- function() g()",
      "g <- function() h()",
      "h <- function() stop('some error')",
      "f()"),
    file.path(path, "src", "example", "script.R"))

  expect_error(orderly_run_internal("example", root = path, echo = FALSE,
                                    capture_log = TRUE),
               "some error")

  drafts <- orderly_list_drafts(root = path, include_failed = TRUE)
  expect_equal(nrow(drafts), 1)

  draft_dir <- file.path(path_draft(path), drafts$name, drafts$id)
  expect_true(file.exists(path_orderly_fail_rds(draft_dir)))

  failed_rds <- readRDS(path_orderly_fail_rds(draft_dir))
  expect_equal(names(failed_rds),
               c("session_info", "time", "env", "error", "meta",
                 "archive_version"))
  expect_equal(failed_rds$error$error$message, "some error")
  expect_true(length(failed_rds$error$trace) > 5)
  expect_match(failed_rds$error$trace[length(failed_rds$error$trace) - 3],
               "f()")
  expect_match(failed_rds$error$trace[length(failed_rds$error$trace) - 2],
               "g()")
  expect_match(failed_rds$error$trace[length(failed_rds$error$trace) - 1],
               "h()")
  expect_match(failed_rds$error$trace[length(failed_rds$error$trace)],
               'stop\\("some error"\\)')
})
