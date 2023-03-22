context("info")

test_that("can retrieve info from successful run", {
  path <- orderly_example("minimal")
  id <- orderly_run("example", root = path, echo = FALSE)

  info <- orderly_info(id, "example", path)
  expect_equal(info$name, "example")
  expect_equal(info$id, id)
  expect_null(info$parameters)
  expect_null(info$git, "master")
  expect_null(info$logfile)
  expect_null(info$error)
})

test_that("can retrieve parameter info", {
  path <- orderly_example("demo")
  id <- orderly_run("other", parameters = list(nmin = 0.1), root = path,
                    echo = FALSE)

  info <- orderly_info(id, "other", path)
  expect_equal(info$name, "other")
  expect_equal(info$id, id)
  expect_equal(info$parameters, list(nmin = 0.1))
  expect_null(info$git)
  expect_null(info$logfile)
  expect_null(info$error)
})

test_that("can retrieve info from failed run", {
  path <- test_prepare_orderly_git_example()

  append_lines('stop("some error")',
               file.path(path[["local"]], "src", "minimal", "script.R"))
  expect_error(orderly_run("minimal", root = path[["local"]], echo = FALSE),
               "some error")
  drafts <- orderly_list_drafts(root = path[["local"]], include_failed = TRUE)
  expect_equal(nrow(drafts), 1)

  info <- orderly_info(drafts[["id"]], "minimal", path[["local"]])
  expect_equal(info$name, "minimal")
  expect_equal(info$id, drafts$id)
  expect_null(info$parameters)
  expect_equal(info$git$branch, "master")
  expect_match(info$git$ref, "[0-9a-f]{7}")
  expect_null(info$logfile)
  expect_equal(info$error$message, "some error")
  expect_true(length(info$error$trace) > 5)
  expect_match(info$error$trace[length(info$error$trace)], "some error")
})

test_that("will return path to logfile if exists", {
  path <- test_prepare_orderly_example("minimal")

  id <- orderly_run_internal("example", root = path, echo = FALSE,
                             capture_log = TRUE, commit = TRUE)
  info <- orderly_info(id, "example", path)
  expect_true(file.exists(info$logfile))
  expect_true(is_absolute_path(info$logfile))
  log <- readLines(info$logfile)
  expect_true("[ name       ]  example" %in% log)
  expect_true(any(grepl("^\\[ commit     \\]  .*", log)))
})

test_that("info errors if fails to find report rds", {
  path <- orderly_example("minimal")
  id <- orderly_run("example", root = path, echo = FALSE)

  unlink(path_orderly_run_rds(file.path(path, "draft", "example", id)))

  expect_error(
    orderly_info(id, "example", path), sprintf(
      "Failed to retrieve info for report example:%s, rds does not exist",
      id),
    fixed = TRUE)
})

test_that("can retrieve package info", {
  path <- test_prepare_orderly_example("packages", testing = TRUE)
  packages <- orderly_packages(path)
  expect_length(packages, 3)
  expect_equal(class(packages), "list")
  expect_equal(packages[[1]], "dplyr")
  expect_equal(packages[[2]], "readr")
  expect_equal(packages[[3]], "stringr")
})

test_that("can retrieve empty package info", {
  path <- test_prepare_orderly_example("changelog", testing = TRUE)
  packages <- orderly_packages(path)
  expect_length(packages, 0)
  expect_equal(class(packages), "list")
})
