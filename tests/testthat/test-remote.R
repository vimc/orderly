context("remote")


test_that("defaults: null", {
  path <- prepare_orderly_example("minimal")
  expect_null(set_default_remote(NULL))
  expect_error(get_default_remote(path, FALSE),
               "default remote has not been set yet")
  expect_error(get_remote(NULL, path),
               "default remote has not been set yet")
})


test_that("defaults: envvar", {
  path <- prepare_orderly_example("minimal")
  tmp <- tempfile()
  dir.create(tmp)
  writeLines("", path_orderly_config_yml(tmp))
  withr::with_envvar(
    c("ORDERLY_DEFAULT_REMOTE_PATH" = tmp),
    expect_equal(get_default_remote(path, FALSE),
                 orderly_remote_path(tmp)))
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
