context("readme")

test_that("auto copy README.md",  {
  path <- prepare_orderly_example("minimal")
  ## in report directory create a file called README.md
  report_path <- file.path(path, "src", "example")
  file.create(file.path(report_path, "README.md"))
  id <- orderly_run("example", config = path, echo = FALSE)
  p <- file.path(path, "draft", "example", id)
  expect_true(file.exists(file.path(p, "README.md")))
})

test_that("lowercase README.md",  {
  path <- prepare_orderly_example("minimal")
  ## in report directory create a file called README.md
  report_path <- file.path(path, "src", "example")
  file.create(file.path(report_path, "readme.MD"))
  id <- orderly_run("example", config = path, echo = FALSE)
  p <- file.path(path, "draft", "example", id)
  expect_true(file.exists(file.path(p, "README.md")))
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
  messages <- capture_messages(orderly_run("example", config = path,
                                           echo = FALSE))
  # ...make sure none of the messages contain "unexpected"
  expect_true(any(grep("readme", messages)))
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
           "  dat: SELECT name, number FROM thing",
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

  expect_error(orderly_run("example", config = path),
               "README.md should not be listed as an artefact")
})

test_that("readme db",  {
  path <- prepare_orderly_example("minimal")
  report_path <- file.path(path, "src", "example")
  file.create(file.path(report_path, "README.md"))
  id <- orderly_run("example", config = path, echo = FALSE)
  orderly_commit(id, config = path)

  con <- orderly_db("destination", config = path)
  ## read the file input table
  d <- DBI::dbReadTable(con, "file_input")
  DBI::dbDisconnect(con)

  ## check README.md has been added to file_input table
  readme_file <- (d$filename == "README.md")
  expect_true(any(readme_file))

  ## check that the purpose of README.md is readme
  expect_true(all(d$file_purpose[readme_file] == "readme"))
})
