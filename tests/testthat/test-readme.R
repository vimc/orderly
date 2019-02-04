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
