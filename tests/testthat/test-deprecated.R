context("deprecated functions")

test_that("pull_archive", {
  path1 <- prepare_orderly_example("demo")
  id <- orderly_run("multifile-artefact", root = path1, echo = FALSE)
  orderly_commit(id, root = path1)
  path2 <- prepare_orderly_example("demo")
  expect_warning(
    pull_archive("multifile-artefact", "latest", path2, remote = path1),
    "orderly_pull_archive")
  d <- orderly_list_archive(path2)
  expect_equal(d$name, "multifile-artefact")
  expect_true(d$id %in% orderly_list_archive(path1)$id)
})


test_that("pull dependencies", {
  dat <- prepare_orderly_remote_example()
  expect_warning(
    pull_dependencies("depend", root = dat$config, remote = dat$remote),
    "orderly_pull_dependencies")
  expect_equal(orderly_list_archive(dat$config)$id, dat$id2)
})


test_that("default_remote_set", {
  path <- prepare_orderly_example("minimal")
  expect_warning(
    set_default_remote(NULL, path),
    "orderly_default_remote_set")
})


test_that("unzip_archive", {
  path <- prepare_orderly_example("minimal")
  id <- orderly_run("example", root = path, echo = FALSE)
  p <- orderly_commit(id, root = path)
  zip <- zip_dir(p)
  root <- tempfile()
  expect_warning(
    unzip_archive(zip, root, "example", id),
    "orderly_unzip_archive")
  res <- file.path(root, "archive", "example")
  expect_equal(dir(res), id)
})
