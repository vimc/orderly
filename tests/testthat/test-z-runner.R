context("orderly_runner")

test_that("run: basic", {
  path <- prepare_orderly_example("interactive")

  runner <- orderly_runner(path)
  name <- "interactive"
  id <- runner$run(name)

  expect_equal(runner$status(name, id), list(status = "running", output = NULL))

  wait_for_path(file.path(path, "draft", name, id, "started"))
  dat <- runner$status(name, id, TRUE)

  ## We have sensible output
  expect_equal(names(dat$out), c("stderr", "stdout"))
  expect_match(dat$out$stderr, paste0("\\[ id +\\]  ", id),
               all = FALSE)
  expect_match(dat$out$stdout, "Sys.sleep", fixed = TRUE, all = FALSE)

  p <- file.path(path, "draft", name, id, "resume")
  writeLines("continue", p)
  key <- sprintf("%s/%s", name, id)
  wait_for_process_termination(runner$running[[key]]$process)

  res <- runner$status(name, id)

  expect_equal(runner$status(name, id), list(status = "archive", output = NULL))
  expect_equal(runner$status(name, id), list(status = "archive", output = NULL))
  dat2 <- runner$status(name, id, TRUE)

  expect_equal(dat2$out$stdout[seq_along(dat$out$stdout)],
               dat$out$stdout)
  expect_equal(dat2$out$stderr[seq_along(dat$out$stderr)],
               dat$out$stderr)
  expect_true(all(lengths(dat2$out) > lengths(dat$out)))
})

test_that("run: error", {
  path <- prepare_orderly_example("interactive")
  runner <- orderly_runner(path)
  name <- "interactive"
  id <- runner$run(name)

  p <- file.path(path, "draft", name, id, "resume")
  wait_for_path(dirname(p))

  writeLines("error", p)
  key <- sprintf("%s/%s", name, id)
  wait_for_process_termination(runner$running[[key]]$process)

  dat <- runner$status(name, id, TRUE)
  expect_equal(dat$status, "error")
})

test_that("run: draft", {
  path <- prepare_orderly_example("interactive")
  runner <- orderly_runner(path)
  name <- "interactive"
  id <- runner$run(name, commit = FALSE)

  p <- file.path(path, "draft", name, id, "resume")
  wait_for_path(dirname(p))

  writeLines("continue", p)
  key <- sprintf("%s/%s", name, id)
  wait_for_process_termination(runner$running[[key]]$process)

  dat <- runner$status(name, id, TRUE)
  expect_equal(dat$status, "draft")
})

test_that("commit", {
  path <- prepare_orderly_example("minimal")
  runner <- orderly_runner(path)

  name <- "example"
  id <- orderly_run(name, config = path, echo = FALSE)
  res <- runner$commit(name, id)
  expect_error(runner$commit(name, id), "Did not find draft report")
})

test_that("publish", {
  path <- prepare_orderly_example("minimal")
  runner <- orderly_runner(path)

  name <- "example"
  id <- orderly_run(name, config = path, echo = FALSE)
  res <- runner$commit(name, id)
  res <- runner$publish(name, id)

  path_yml <- path_orderly_published_yml(file.path(path, "archive", name, id))
  expect_true(file.exists(path_yml))
  expect_equal(yaml_read(path_yml), list(published = TRUE))

  res <- runner$publish(name, id, FALSE)
  expect_equal(yaml_read(path_yml), list(published = FALSE))
})

test_that("rebuild", {
  path <- prepare_orderly_example("minimal")
  runner <- orderly_runner(path)

  name <- "example"
  id <- orderly_run(name, config = path, echo = FALSE)
  res <- runner$commit(name, id)

  path_db <- file.path(path, "orderly.sqlite")
  file.remove(path_db)
  expect_silent(runner$rebuild())
  expect_true(file.exists(path_db))
})
