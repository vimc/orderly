context("remote: path")

test_that("Reject impossible remotes", {
  remote <- tempfile()
  dir.create(remote)
  expect_error(orderly_remote_path(remote),
               "Does not look like an orderly repository")
})


test_that("pull report", {
  path1 <- create_orderly_demo()
  path2 <- prepare_orderly_example("demo")

  remote <- orderly_remote_path(path1)

  pull_archive("multifile-artefact", "latest", path2, remote = remote)

  d <- orderly_list_archive(path2)
  expect_equal(d$name, "multifile-artefact")
  expect_true(d$id %in% orderly_list_archive(path1)$id)
})


test_that("pull report: error not found", {
  path1 <- prepare_orderly_example("minimal")
  path2 <- prepare_orderly_example("minimal")

  remote <- orderly_remote_path(path1)

  expect_error(
    pull_archive("example", new_report_id(), path2, remote = remote),
    "Did not find archived report at")
})


test_that("pull report: already done", {
  path1 <- prepare_orderly_example("minimal")
  path2 <- prepare_orderly_example("minimal")

  id <- orderly_run("example", config = path1, echo = FALSE)
  orderly_commit(id, config = path1)

  remote <- orderly_remote_path(path1)

  expect_message(
    pull_archive("example", id, path2, remote = remote),
    sprintf("\\[ pull\\s+ \\]  example:%s\\s*$", id))

  expect_message(
    pull_archive("example", id, path2, remote = remote),
    sprintf("\\[ pull\\s+ \\]  example:%s already exists", id))
})


test_that("push report: already done", {
  path1 <- prepare_orderly_example("minimal")
  path2 <- prepare_orderly_example("minimal")

  id <- orderly_run("example", config = path1, echo = FALSE)
  orderly_commit(id, config = path1)

  remote <- orderly_remote_path(path2)

  expect_message(
    push_archive("example", id, path1, remote = remote),
    sprintf("\\[ push\\s+ \\]  example:%s\\s*$", id))

  expect_message(
    push_archive("example", id, path1, remote = remote),
    sprintf("\\[ push\\s+ \\]  example:%s already exists", id))
})
