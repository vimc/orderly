context("globals")

test_that("global", {
  path <- prepare_orderly_example("global")
  tmp <- tempfile()
  expect_error(
    orderly_run("example", config = path, id_file = tmp, echo = FALSE),
    NA # expect no errors
  )
})

test_that("no global folder", {
  path <- prepare_orderly_example("global")
  # now we break the orderly_config.yml
  path_config <- file.path(path, "orderly_config.yml")
  config_lines = readLines(path_config, -1)
  # we know the final line (line six) is the global resource folder
  # so set it to something invalid
  config_lines[6] <- "  invalid_path"
  writeLines(config_lines, path_config)
  
  tmp <- tempfile()
  expect_error(
     orderly_run("example", config = path, id_file = tmp, echo = FALSE),
     paste("Global resource folder does not exist", 
           "(expected: invalid_path)"),
     fixed = TRUE
     )
})

test_that("missing global file", {
  path <- prepare_orderly_example("global")
  # now we break the report yaml
  path_example <- file.path(path, "src", "example")
  path_yaml <- file.path(path_example, "orderly.yml")
  # the final line of the yaml is globa file, so we change that
  config_lines = readLines(path_yaml, -1)
  config_lines[11] <- "  - none.csv"
  writeLines(config_lines, path_yaml)
  
  tmp <- tempfile()
  expect_error(
    orderly_run("example", config = path, id_file = tmp, echo = FALSE),
    "Error copying files", fixed = TRUE
  )
})