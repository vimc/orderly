context("orderly_runner")

test_that("runner queue", {
  queue <- runner_queue()
  expect_equal(queue$status(""), list(state = "unknown", id = NA_character_))
  expect_null(queue$next_queued())

  key1 <- queue$insert("a")
  expect_equal(queue$status(key1), list(state = "queued", id = NA_character_))
  key2 <- queue$insert("b", "parameters")
  key3 <- queue$insert("c", ref = "ref")
  key4 <- queue$insert("d", timeout = 200)

  d <- queue$next_queued()
  expect_equal(d, list(key = key1, state = "queued", name = "a",
                       parameters = NA_character_, ref = NA_character_,
                       id = NA_character_, timeout = 600))

  expect_true(queue$set_state(key1, "running"))
  expect_equal(queue$status(key1), list(state = "running", id = NA_character_))

  d <- queue$next_queued()
  expect_equal(d, list(key = key2, state = "queued", name = "b",
                       parameters = "parameters", ref = NA_character_,
                       id = NA_character_, timeout = 600))

  expect_true(queue$set_state(key2, "running", new_report_id()))

  d <- queue$next_queued()
  expect_equal(d, list(key = key3, state = "queued", name = "c",
                       parameters = NA_character_, ref = "ref",
                       id = NA_character_, timeout = 600))

  expect_true(queue$set_state(key3, "running", new_report_id()))

  d <- queue$next_queued()
  expect_equal(d, list(key = key4, state = "queued", name = "d",
                       parameters = NA_character_, ref = NA_character_,
                       id = NA_character_, timeout = 200))
  expect_true(queue$set_state(key4, "running", new_report_id()))

  expect_null(queue$next_queued())
})

test_that("run: success", {
  skip_on_appveyor()
  path <- prepare_orderly_example("interactive")

  expect_false(file.exists(file.path(path, "orderly.sqlite")))
  runner <- orderly_runner(path)
  expect_true(file.exists(file.path(path, "orderly.sqlite")))
  name <- "interactive"
  key <- runner$queue(name)

  expect_equal(runner$status(key),
               list(key = key,
                    status = "queued",
                    id = NA_character_,
                    output = NULL))

  tmp <- runner$poll()
  expect_equal(tmp, structure("create", key = key))
  id <- wait_for_id(runner, key)

  st <- runner$status(key)
  expect_is(st$id, "character")
  expect_equal(st$status, "running")

  dat <- runner$status(key, TRUE)
  expect_equal(names(dat$output), c("stderr", "stdout"))
  expect_match(dat$output$stderr, paste0("\\[ id +\\]  ", id),
               all = FALSE)

  wait_for_path(file.path(path, "draft", name, id, "started"))
  writeLines("continue", file.path(path, "draft", name, id, "resume"))
  wait_while_running(runner)

  dat2 <- runner$status(key, TRUE)
  expect_equal(dat2$status, "success")

  expect_equal(dat2$output$stdout[seq_along(dat$output$stdout)],
               dat$output$stdout)
  expect_equal(dat2$output$stderr[seq_along(dat$output$stderr)],
               dat$output$stderr)
  expect_true(all(lengths(dat2$output) > lengths(dat$output)))
})

test_that("run: error", {
  skip_on_appveyor()
  path <- prepare_orderly_example("interactive")
  runner <- orderly_runner(path)
  dat <- runner_start_interactive(runner)

  writeLines("error", file.path(path, "draft", dat$name, dat$id, "resume"))
  wait_for_process_termination(runner$process$px)

  res <- runner$status(dat$key, TRUE)
  expect_equal(res$status, "running")

  expect_equal(runner$poll(), structure("finish", key = dat$key))
  res <- runner$status(dat$key, TRUE)
  expect_equal(res$status, "error")
})

test_that("publish", {
  path <- prepare_orderly_example("minimal")
  runner <- orderly_runner(path)

  name <- "example"
  id <- orderly_run(name, config = path, echo = FALSE)
  orderly_commit(id, name, config = path)

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
  orderly_commit(id, name, config = path)

  path_db <- file.path(path, "orderly.sqlite")
  file.remove(path_db)
  expect_silent(runner$rebuild())
  expect_true(file.exists(path_db))
})

test_that("run in branch (local)", {
  skip_on_appveyor()
  path <- unzip_git_demo()
  runner <- orderly_runner(path)

  pars <- as.character(jsonlite::toJSON(list(nmin = 0), auto_unbox = TRUE))
  key <- runner$queue("other", parameters = pars, ref = "other")
  runner$poll()
  id <- wait_for_id(runner, key)
  wait_while_running(runner)

  d <- orderly_list_archive(path)
  expect_equal(nrow(d), 1L)
  expect_equal(d$name, "other")
  expect_equal(d$id, id)
})

test_that("fetch / detach / pull", {
  path <- prepare_orderly_git_example()
  path1 <- path[["origin"]]
  path2 <- path[["local"]]

  sha <- git_ref_to_sha("master", path2)
  expect_equal(git_ref_to_sha("origin/master", path2), sha)

  runner <- orderly_runner(path2)
  res <- runner$git_fetch()
  expect_equal(res$code, 0)
  expect_is(res$output, "character")

  sha2 <- git_ref_to_sha("HEAD", path1)
  expect_true(sha != sha2)
  expect_equal(git_ref_to_sha("origin/master", path2), sha2)
  expect_equal(git_ref_to_sha("HEAD", path2), sha)

  res <- runner$git_pull()
  expect_equal(res$code, 0)
  expect_is(res$output, "character")

  expect_equal(git_ref_to_sha("origin/master", path2), sha2)
  expect_equal(git_ref_to_sha("HEAD", path2), sha2)

  res <- runner$git_status()
  expect_equal(res$output, character(0))
  expect_equal(res$clean, TRUE)
  expect_equal(res$branch, "master")
  expect_equal(res$hash, sha2)
})

test_that("prevent git change", {
  path <- unzip_git_demo()
  runner <- orderly_runner(path, FALSE)
  expect_error(runner$queue("other", ref = "other"),
               "Reference switching is disabled in this runner")
})

test_that("Can't git change", {
  path <- prepare_orderly_example("interactive")
  runner <- orderly_runner(path)
  expect_error(runner$queue("other", ref = "other"),
               "Reference switching is disabled in this runner")
})

test_that("kill", {
  path <- prepare_orderly_example("interactive")
  runner <- orderly_runner(path)
  name <- "interactive"
  key <- runner$queue(name)
  runner$poll()
  id <- wait_for_id(runner, key)
  expect_true(runner$kill(key))
  expect_error(runner$kill(key), "Can't kill")
  expect_null(runner$process)
  expect_equal(runner$poll(), "idle")
})

test_that("kill - wrong process", {
  path <- prepare_orderly_example("interactive")
  runner <- orderly_runner(path)
  name <- "interactive"
  key <- runner$queue(name)
  runner$poll()
  id <- wait_for_id(runner, key)

  key2 <- "virtual_plant"
  expect_error(runner$kill(key2),
               sprintf("Can't kill '%s' - currently running '%s'", key2, key))
  runner$kill(key)
})

test_that("kill - no process", {
  path <- prepare_orderly_example("interactive")
  runner <- orderly_runner(path)
  key <- "virtual_plant"
  expect_error(runner$kill(key),
               "Can't kill 'virtual_plant' - not currently running a report")
})

test_that("timeout", {
  path <- prepare_orderly_example("interactive")
  runner <- orderly_runner(path)
  name <- "interactive"
  key <- runner$queue(name, timeout = 0)
  runner$poll()
  id <- wait_for_id(runner, key)
  expect_equal(runner$poll(), structure("timeout", key = key))
  expect_equal(runner$poll(), "idle")
})

test_that("queue_status", {
  path <- prepare_orderly_example("interactive")
  runner <- orderly_runner(path)

  expect_equal(
    runner$queue_status(NULL),
    list(status = "idle", queue = runner_queue()$get_df(), current = NULL))

  name <- "interactive"
  key1 <- runner$queue(name)
  key2 <- runner$queue(name)

  expect_equal(
    runner$queue_status(NULL),
    list(status = "idle", queue = runner$data$get_df(), current = NULL))

  runner$poll()
  res <- runner$queue_status()
  expect_equal(res$status, "running")
  expect_equal(res$queue, runner$data$get_df())
  expect_equal(nrow(res$queue), 2)
  expect_equal(res$current$key, key1)
  expect_equal(res$current$name, "interactive")
  expect_is(res$current$start_at, "POSIXt")
  expect_is(res$current$kill_at, "POSIXt")

  expect_equal(res$current$elapsed + res$current$remaining, 600)
  expect_null(res$current$output)

  res <- runner$queue_status(TRUE)
  expect_is(res$current$output$stderr, "character")
  expect_is(res$current$output$stdout, "character")

  res <- runner$queue_status(limit = 1)
  expect_equal(nrow(res$queue), 1L)
})
