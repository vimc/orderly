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


test_that("global resources end up in db", {
  path <- prepare_orderly_example("global")
  tmp <- tempfile()
  id <- orderly_run("example", config = path, id_file = tmp, echo = FALSE)
  orderly_commit(id, config = path)
  con <- orderly_db("destination", config = path)
  d <- DBI::dbReadTable(con, "file_input")
  DBI::dbDisconnect(con)
  i <- d$file_purpose == "global"
  expect_equal(sum(i), 1)

  tmp <- d[i, ]
  expect_equal(d$report_version[i], id)
  expect_equal(d$file_hash[i],
               hash_files(file.path(path, "global", "data.csv"), FALSE))
  expect_equal(d$filename[i], "data.csv")
})
