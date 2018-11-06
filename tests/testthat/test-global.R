context("globals")

test_that("global", {
  path <- prepare_orderly_example("global")
  tmp <- tempfile()
  expect_error(
    orderly_run("example", config = path, id_file = tmp, echo = FALSE),
    NA # expect no errors
  )
})

test_that("missing global file", {
  path <- prepare_orderly_example("global")
  # now we break the report yaml
  path_example <- file.path(path, "src", "example")
  path_yaml <- file.path(path_example, "orderly.yml")
  # the final line of the yaml is globa file, so we change that
  config_lines <- readLines(path_yaml)
  config_lines[11] <- "  - none.csv"
  writeLines(config_lines, path_yaml)
  
  expected_error <- "Global resources in '.+/global' does not exist: 'none.csv'"

  tmp <- tempfile()
  expect_error(
    orderly_run("example", config = path, id_file = tmp, echo = FALSE),
                expected_error)
})
