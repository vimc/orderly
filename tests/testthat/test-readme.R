test_that("auto copy README.md",  {
  path <- prepare_orderly_example("minimal")
  ## in report directory create a file called README.md
  report_path <- file.path(path, "src", "example")
  file.create(file.path(report_path, "README.md"))
  id <- orderly_run("example", config = path, echo = FALSE)
  p <- file.path(path, "draft", "example", id)
  expect_true(file.exists(file.path(p, "README.md")))
})

test_that("yaml copy README.md",  {
  path <- prepare_orderly_example("minimal")
  ## in report directory create a file called README.md
  path_example <- file.path(path, "src", "example")
  
  yml_path <- file.path(path_example, "orderly.yml")
  minimal_yml <- readLines(yml_path)
  extended_yml <- c(minimal_yml, sprintf("%s: %s", "has_readme", "TRUE"))
  writeLines(extended_yml, yml_path)
  
  file.create(file.path(path_example, "README.md"))
  
  id <- orderly_run("example", config = path, echo = FALSE)
  p <- file.path(path, "draft", "example", id)
  expect_true(file.exists(file.path(p, "README.md")))
})

test_that("yaml copy README.md missing",  {
  path <- prepare_orderly_example("minimal")
  ## in report directory create a file called README.md
  path_example <- file.path(path, "src", "example")
  
  yml_path <- file.path(path_example, "orderly.yml")
  minimal_yml <- readLines(yml_path)
  extended_yml <- c(minimal_yml, sprintf("%s: %s", "has_readme", "TRUE"))
  writeLines(extended_yml, yml_path)
  
  
  expect_error(orderly_run("example", config = path, echo = FALSE),
               "README.md does not exist")
})

test_that("bad yaml copy README.md ",  {
  path <- prepare_orderly_example("minimal")
  ## in report directory create a file called README.md
  path_example <- file.path(path, "src", "example")
  
  yml_path <- file.path(path_example, "orderly.yml")
  minimal_yml <- readLines(yml_path)
  extended_yml <- c(minimal_yml, sprintf("%s: %s", "has_readme", "README.md"))
  writeLines(extended_yml, yml_path)
  
  file.create(file.path(path_example, "README.md"))
  
  expect_error(orderly_run("example", config = path, echo = FALSE),
               "must be logical")
})