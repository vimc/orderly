context("readme")

test_that("auto copy README.md",  {
  path <- prepare_orderly_example("minimal")
  ## in report directory create a file called README.md
  report_path <- file.path(path, "src", "example")
  file.create(file.path(report_path, "README.md"))
  id <- orderly_run("example", root = path, echo = FALSE)
  p <- file.path(path, "draft", "example", id)
  expect_true(file.exists(file.path(p, "README.md")))
  orderly_commit(id, root = path)
  con <- orderly_db("destination", root = path)
  on.exit(DBI::dbDisconnect(con))
  dat <- DBI::dbReadTable(con, "file_input")
  expect_equal(sum(dat$filename == "README.md"), 1)
})

test_that("lowercase README.md",  {
  path <- prepare_orderly_example("minimal")
  ## in report directory create a file called README.md
  report_path <- file.path(path, "src", "example")
  file.create(file.path(report_path, "readme.MD"))
  id <- orderly_run("example", root = path, echo = FALSE)
  p <- file.path(path, "draft", "example", id)
  expect_true(file.exists(file.path(p, "README.MD")))
  orderly_commit(id, root = path)
  con <- orderly_db("destination", root = path)
  on.exit(DBI::dbDisconnect(con))
  dat <- DBI::dbReadTable(con, "file_input")
  expect_equal(sum(dat$filename == "README.MD"), 1)
})

test_that("list README.md as resource",  {
  path <- prepare_orderly_example("minimal")
  report_path <- file.path(path, "src", "example")
  ## in report directory create a file called README.md
  path_example <- file.path(path, "src", "example")
  file.create(file.path(report_path, "README.md"))

  yml_path <- file.path(path_example, "orderly.yml")
  minimal_yml <- readLines(yml_path)
  extended_yml <- c(minimal_yml,
                    "resources:",
                    "  README.md")
  writeLines(extended_yml, yml_path)

  file.create(file.path(path_example, "README.md"))
  messages <- capture_messages(
    id <- orderly_run("example", root = path, echo = FALSE))
  # ...make sure none of the messages contain "unexpected"
  expect_true(any(grep("should not be listed as a resource", messages)))
  orderly_commit(id, root = path)
  con <- orderly_db("destination", root = path)
  on.exit(DBI::dbDisconnect(con))
  dat <- DBI::dbReadTable(con, "file_input")
  expect_equal(sum(dat$filename == "README.md"), 1)
})

test_that("list README.md as artefact",  {
  path <- prepare_orderly_example("minimal")
  report_path <- file.path(path, "src", "example")
  ## in report directory create a file called README.md
  path_example <- file.path(path, "src", "example")
  file.create(file.path(report_path, "README.md"))

  yml_path <- file.path(path_example, "orderly.yml")
  ## rewrite the yaml to include README.md as a resource
  yml <- c("data:",
           "  dat:",
           "    query: SELECT name, number FROM thing",
           "script: script.R",
           "artefacts:",
           "  - staticgraph:",
           "      description: A graph of things",
           "      filenames: mygraph.png",
           "  - data:",
           "      description: a readme file",
           "      filenames: README.md"
           )
  writeLines(yml, file.path(yml_path))

  expect_error(orderly_run("example", root = path),
               "README.md should not be listed as an artefact")
})

test_that("readme db",  {
  path <- prepare_orderly_example("minimal")
  report_path <- file.path(path, "src", "example")
  file.create(file.path(report_path, "README.md"))
  id <- orderly_run("example", root = path, echo = FALSE)
  orderly_commit(id, root = path)

  con <- orderly_db("destination", root = path)
  ## read the file input table
  d <- DBI::dbReadTable(con, "file_input")
  DBI::dbDisconnect(con)

  ## check README.md has been added to file_input table
  readme_file <- (d$filename == "README.md")
  expect_true(any(readme_file))

  ## check that the purpose of README.md is readme
  expect_true(all(d$file_purpose[readme_file] == "readme"))
})

test_that("copy readme in sub-directory", {
  path <- prepare_orderly_example("demo")
  ## in report directory create a file called README.md
  report_path <- file.path(path, "src", "use_resource")

  # add a readme file to the meta data directory
  file.create(file.path(report_path, "meta", "README.md"))
  id <- orderly_run("use_resource", root = path, echo = FALSE)
  p <- file.path(path, "draft", "use_resource", id)

  # make sure the file has been copied across
  expect_true(file.exists(file.path(p, "meta", "README.md")))
  orderly_commit(id, root = path)
  con <- orderly_db("destination", root = path)
  on.exit(DBI::dbDisconnect(con))
  dat <- DBI::dbReadTable(con, "file_input")

  # make sure the file has been inserted to the database
  expect_equal(sum(dat$filename == "meta/README.md"), 1)
})

test_that("list README.md as resource in sub-directory", {
  path <- prepare_orderly_example("demo")
  ## in report directory create a file called README.md
  report_path <- file.path(path, "src", "use_resource")
  # add a readme fiel to the meta data directory
  file.create(file.path(report_path, "meta", "README.md"))
  #rewrite yml to include extra readme file
  yml_path <- file.path(report_path, "orderly.yml")
  yml <- c("data:",
           "  dat:",
           "    query: SELECT name, number FROM thing",
           "script: script.R",
           "resources:",
           "  - meta/data.csv",
           "  - meta/README.md",
           "  - README.md",
           "artefacts:",
           "  staticgraph:",
           "    description: A graph of things",
           "    filenames: mygraph.png",
           "author: Dr Serious",
           "requester: ACME"
           )
  writeLines(yml, file.path(yml_path))
  # make sure we get a warning about this
  messages <- capture_messages(
    id <- orderly_run("use_resource", root = path, echo = FALSE))
  expect_true(any(grep("should not be listed as a resource", messages)))

  ## Try again _without_ listing the READMEs as a resource so that we
  ## see that they're copied over
  writeLines(yml[!grepl("README", yml)], file.path(yml_path))
  id <- orderly_run("use_resource", root = path, echo = FALSE)

  p <- file.path(path, "draft", "use_resource", id)
  # make sure the file has been copied across
  expect_true(file.exists(file.path(p, "meta", "README.md")))
  expect_true(file.exists(file.path(p, "README.md")))
  orderly_commit(id, root = path)
  con <- orderly_db("destination", root = path)
  on.exit(DBI::dbDisconnect(con))
  dat <- DBI::dbReadTable(con, "file_input")

  ## make sure the file has been inserted to the database
  i <- dat$file_purpose == "readme"
  expect_equal(sum(i), 2)
  expect_setequal(dat$filename[i], c("README.md", "meta/README.md"))
})
