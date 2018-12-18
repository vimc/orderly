context("remote: path")

test_that("Reject impossible remotes", {
  remote <- tempfile()
  dir.create(remote)
  expect_error(orderly_remote_path(remote),
               "Does not look like an orderly repository")
})


test_that("remote_name", {
  path <- prepare_orderly_example("minimal")
  expect_identical(orderly_remote_path(path)$name,
                   normalizePath(path, "/"))
})


test_that("get remote", {
  path1 <- prepare_orderly_example("minimal")
  path2 <- prepare_orderly_example("minimal")

  obj <- get_remote(path1, orderly_config(path2))
  expect_is(obj, "orderly_remote_path")
  expect_equal(obj$name, path1)
})


test_that("pull report", {
  path1 <- prepare_orderly_example("demo")
  id <- orderly_run("multifile-artefact", config = path1, echo = FALSE)
  orderly_commit(id, config = path1)

  path2 <- prepare_orderly_example("demo")
  pull_archive("multifile-artefact", "latest", path2, remote = path1)

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
    "Unknown report")
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


test_that("push report (path)", {
  ours <- prepare_orderly_example("demo")
  id <- orderly_run("multifile-artefact", config = ours, echo = FALSE)
  orderly_commit(id, config = ours)

  theirs <- prepare_orderly_example("demo")

  remote <- orderly_remote_path(theirs)
  push_archive("multifile-artefact", "latest", ours, remote = remote)

  d <- orderly_list_archive(theirs)
  expect_equal(d$name, "multifile-artefact")
  expect_true(d$id %in% orderly_list_archive(ours)$id)
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


test_that("remote_report_names", {
  dat <- prepare_orderly_remote_example(FALSE)
  remote_path <- orderly_remote_path(dat$path_remote)
  expect_equal(remote_report_names(dat$config, remote = dat$remote),
               c("depend", "depend2", "example"))
})


test_that("remote_report_versions", {
  dat <- prepare_orderly_remote_example(FALSE)
  remote_path <- orderly_remote_path(dat$path_remote)

  expect_equal(
    remote_report_versions("example", dat$config, remote = dat$remote),
    c(dat$id1, dat$id2))
  expect_equal(
    remote_report_versions("unknown", dat$config, remote = dat$remote),
    character(0))
})


test_that("orderly_run", {
  dat <- prepare_orderly_remote_example(FALSE)
  expect_error(
    orderly_run_remote("example", config = dat$config, remote = dat$remote),
    "'orderly_remote_path' remotes do not run")
})


test_that("orderly_publish", {
  remote <- prepare_orderly_example("minimal")
  id <- orderly_run("example", config = remote, echo = FALSE)
  p <- orderly_commit(id, config = remote)

  local <- prepare_orderly_example("minimal")
  orderly_publish_remote("example", id, config = local, remote = remote)
  expect_equal(yaml_read(path_orderly_published_yml(p)),
               list(published = TRUE))
})


test_that("set_default", {
  dat <- prepare_orderly_remote_example(FALSE)
  v <- set_default_remote(dat$path_remote, dat$config)
  expect_is(v, "orderly_remote_path")
  expect_equal(v, dat$remote)
  expect_identical(v$name, dat$remote$name)
  expect_identical(get_default_remote(dat$config), v)
})


test_that("use configuration", {
  path_remote <- prepare_orderly_example("minimal")
  id <- orderly_run("example", config = path_remote, echo = FALSE)
  orderly_commit(id, config = path_remote)

  path_local <- prepare_orderly_example("minimal")

  cfg <- yaml_read(path_orderly_config_yml(path_local))
  cfg$remote <- list(
    example = list(
      driver = "orderly_remote_path",
      args = list(path = path_remote)))
  yaml_write(cfg, path_orderly_config_yml(path_local))

  config <- orderly_config(path_local)
  remote <- get_remote("example", config)
  expect_is(remote, "orderly_remote_path")
  expect_equal(remote$name, "example")
  expect_equal(remote$config$path, path_remote)

  pull_archive("example", config = path_local, remote = "example")
  expect_true(is_directory(file.path(path_local, "archive", "example", id)))
})
