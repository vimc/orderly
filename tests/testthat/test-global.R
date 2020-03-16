context("globals")

test_that("global", {
  path <- prepare_orderly_example("global", testing = TRUE)
  tmp <- tempfile()
  expect_error(
    orderly_run("example", root = path, id_file = tmp, echo = FALSE),
    NA # expect no errors
  )
})

test_that("missing global file", {
  path <- prepare_orderly_example("global", testing = TRUE)
  # now we break the report yaml
  path_example <- file.path(path, "src", "example")
  path_yaml <- file.path(path_example, "orderly.yml")
  # the final line of the yaml is globa file, so we change that
  config_lines <- readLines(path_yaml)
  config_lines[11] <- "  data.csv: none.csv"
  writeLines(config_lines, path_yaml)

  expected_error <- "Global resources in '.+/global' does not exist: 'none.csv'"

  tmp <- tempfile()
  expect_error(
    orderly_run("example", root = path, id_file = tmp, echo = FALSE),
                expected_error)
})


test_that("global resources end up in db", {
  skip_on_cran_windows()
  path <- prepare_orderly_example("global", testing = TRUE)
  tmp <- tempfile()
  id <- orderly_run("example", root = path, id_file = tmp, echo = FALSE)
  orderly_commit(id, root = path)
  con <- orderly_db("destination", root = path)
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


## We can relax this once VIMC-2961 is resolved
test_that("directories of global resources are forbidden", {
  path <- prepare_orderly_example("global", testing = TRUE)
  p_global <- file.path(path, "global", "dir")
  dir.create(p_global)

  p_orderly <- file.path(path, "src", "example", "orderly.yml")
  d <- yaml_read(p_orderly)
  d$global_resources <- list("dir" = "dir")
  yaml_write(d, p_orderly)

  expect_error(
    orderly_run("example", root = path),
    "global resources cannot yet be directories")
})


test_that("global resource from a subdir", {
  skip_on_cran_windows()
  path <- prepare_orderly_example("global", testing = TRUE)
  dir.create(file.path(path, "global", "dir"))
  file.rename(file.path(path, "global", "data.csv"),
              file.path(path, "global", "dir", "data.csv"))

  p_orderly <- file.path(path, "src", "example", "orderly.yml")
  d <- yaml_read(p_orderly)
  d$global_resources <- list("data.csv" = "dir/data.csv")
  yaml_write(d, p_orderly)

  id <- orderly_run("example", root = path, echo = FALSE)
  p <- orderly_commit(id, root = path)

  con <- orderly_db("destination", root = path)
  on.exit(DBI::dbDisconnect(con))

  sql <- c("SELECT file_input.*, file_input_global.filename as orig",
           "FROM file_input LEFT JOIN file_input_global",
           "ON file_input.id = file_input_global.file_input")
  input <- DBI::dbGetQuery(con, paste(sql, collapse = " "))

  expect_equal(nrow(input), 3)
  i <- input$file_purpose == "global"
  expect_true(all(is.na(input$orig[!i])))
  expect_equal(input$orig[i], "dir/data.csv")
  expect_equal(input$filename[i], "data.csv")
})


test_that("rename global resource on import, into new dir", {
  skip_on_cran_windows()
  path <- prepare_orderly_example("global", testing = TRUE)
  dir.create(file.path(path, "global", "dir"))
  file.rename(file.path(path, "global", "data.csv"),
              file.path(path, "global", "dir", "globaldata.csv"))

  p_orderly <- file.path(path, "src", "example", "orderly.yml")
  d <- yaml_read(p_orderly)
  d$global_resources <- list("my/data.csv" = "dir/globaldata.csv")
  yaml_write(d, p_orderly)

  p_src <- file.path(path, "src", "example", "script.R")
  txt <- readLines(p_src)
  writeLines(sub("data.csv", "my/data.csv", txt, fixed = TRUE), p_src)

  id <- orderly_run("example", root = path, echo = FALSE)
  p <- orderly_commit(id, root = path)

  expect_true(file.exists(file.path(p, "my", "data.csv")))

  con <- orderly_db("destination", root = path)
  on.exit(DBI::dbDisconnect(con))

  sql <- c("SELECT file_input.*, file_input_global.filename as orig",
           "FROM file_input LEFT JOIN file_input_global",
           "ON file_input.id = file_input_global.file_input")
  input <- DBI::dbGetQuery(con, paste(sql, collapse = " "))

  expect_equal(nrow(input), 3)
  i <- input$file_purpose == "global"
  expect_true(all(is.na(input$orig[!i])))
  expect_equal(input$orig[i], "dir/globaldata.csv")
  expect_equal(input$filename[i], "my/data.csv")
})
