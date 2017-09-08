context("main")

test_that("run", {
  path <- prepare_orderly_example("minimal")
  args <- c("--root", path, "run", "example")
  res <- main_args(args)
  expect_equal(res$command, "run")
  expect_equal(res$args, "example")
  expect_null(res$options$parameters)
  expect_false(res$options$no_commit)
  expect_false(res$options$print_log)
  expect_identical(res$target, main_do_run)

  capture.output(res$target(res))
  expect_equal(orderly_list(path), "example")
  expect_equal(nrow(orderly_list_archive(path)), 1)
})

test_that("run: id-file", {
  path <- prepare_orderly_example("minimal")
  id_file <- tempfile()
  args <- c("--root", path, "run", "--id-file", id_file, "example")
  res <- main_args(args)

  expect_equal(res$command, "run")
  expect_equal(res$args, "example")
  expect_null(res$options$parameters)
  expect_false(res$options$no_commit)
  expect_false(res$options$print_log)
  expect_equal(res$options$id_file, id_file)
  expect_identical(res$target, main_do_run)

  capture.output(res$target(res))
  expect_equal(orderly_list(path), "example")

  expect_true(file.exists(id_file))
  id <- readLines(id_file)
  expect_equal(id, orderly_list_archive(path)$id)
})

test_that("commit", {
  path <- prepare_orderly_example("minimal")
  id <- orderly_run("example", config = path, echo = FALSE)
  args <- c("--root", path, "commit", id)
  res <- main_args(args)
  expect_equal(res$command, "commit")
  expect_equal(res$args, id)
  expect_identical(res$target, main_do_commit)

  res$target(res)
  expect_equal(nrow(orderly_list_archive(path)), 1)
})

test_that("publish", {
  path <- prepare_orderly_example("minimal")
  id <- orderly_run("example", config = path, echo = FALSE)
  p <- orderly_commit(id, config = path)

  args <- c("--root", path, "publish", id)
  res <- main_args(args)
  expect_equal(res$command, "publish")
  expect_equal(res$args, id)
  expect_false(res$options$unpublish)
  expect_identical(res$target, main_do_publish)

  res$target(res)
  file <- path_orderly_published_yml(p)
  expect_true(file.exists(file))
  expect_equal(yaml_read(file), list(published = TRUE))
})

test_that("latest", {
  path <- prepare_orderly_example("minimal")
  id1 <- orderly_run("example", config = path, echo = FALSE)
  Sys.sleep(0.1)
  id2 <- orderly_run("example", config = path, echo = FALSE)

  args <- c("--root", path, "latest", "--draft", "example")
  res <- main_args(args)
  expect_equal(res$command, "latest")
  expect_equal(res$args, "example")
  expect_true(res$options$draft)
  expect_identical(res$target, main_do_latest)

  expect_output(main_do_latest(res), id2)

  args <- c("--root", path, "latest", "--value-if-missing", "NONE", "example")
  res <- main_args(args)
  expect_output(main_do_latest(res), "NONE")
})

test_that("help", {
  expect_error(capture.output(main_args("--help")),
               "Aborting as help requested")
  expect_output(try(main_args("--help"), silent = TRUE),
                "The <command> argument must be one of", fixed = TRUE)

  for (cmd in names(main_args_commands)) {
    expect_output(try(main_args(c(cmd, "--help")), silent = TRUE),
                  sprintf("[--root=ROOT] %s", cmd),
                  fixed = TRUE)
  }
})

test_that("list", {
  path <- prepare_orderly_example("minimal")

  args <- c("--root", path, "list")
  res <- main_args(args)

  expect_output(res$target(res), "^example$")

  expect_equal(main_args(c("--root", path, "list", "names"))$args, "names")
  expect_equal(main_args(c("--root", path, "list", "drafts"))$args, "drafts")
  expect_equal(main_args(c("--root", path, "list", "archive"))$args, "archive")
  expect_error(main_args(c("--root", path, "list", "foo")),
               "argument to list must be one of")
})
