context("remote")


test_that("defaults: null", {
  path <- prepare_orderly_example("minimal")
  expect_null(set_default_remote(NULL, path))
  expect_error(get_default_remote(path, FALSE),
               "default remote has not been set yet")
  expect_error(get_remote(NULL, path),
               "default remote has not been set yet")
})


test_that("get_remote type failure", {
  dat <- prepare_orderly_remote_example()
  expect_error(get_remote(1, dat$config),
               "Unknown remote type")
  expect_error(get_remote("extra", dat$config),
               "Unknown remote 'extra'")
})


test_that("pull_archive with wrong version", {
  dat <- prepare_orderly_remote_example()

  expect_error(
    pull_archive("example", new_report_id(), root = dat$config,
                 remote = dat$remote),
    paste0("Version '.+?' not found at '.+?': valid versions are:.+",
           dat$id1))
})


test_that("pull dependencies", {
  dat <- prepare_orderly_remote_example()

  expect_message(
    pull_dependencies("depend", root = dat$config, remote = dat$remote),
    "\\[ pull\\s+ \\]  example:")
  expect_equal(orderly_list_archive(dat$config),
               data_frame(name = "example", id = dat$id2))

  ## and update
  id3 <- orderly_run("example", root = dat$path_remote, echo = FALSE)
  orderly_commit(id3, root = dat$path_remote)
  expect_message(
    pull_dependencies("depend", root = dat$config, remote = dat$remote),
    "\\[ pull\\s+ \\]  example:")
  expect_equal(orderly_list_archive(dat$config),
               data_frame(name = "example", id = c(dat$id2, id3)))
})
