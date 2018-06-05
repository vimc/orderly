context("remote")


test_that("defaults: null", {
  path <- prepare_orderly_example("minimal")
  expect_null(set_default_remote(NULL))
  expect_error(get_default_remote(path, FALSE),
               "default remote has not been set yet")
  expect_error(get_remote(NULL, path),
               "default remote has not been set yet")
})


test_that("get_remote", {
  config <- list(api_server = list(foo = "foo", bar = "bar"))
  class(config) <- "orderly_config"
  expect_equal(get_remote(NULL, config), "foo")
  expect_equal(get_remote("foo", config), "foo")
  expect_equal(get_remote("bar", config), "bar")
  expect_error(get_remote("other", config),
               "Unknown remote 'other'",
               fixed = TRUE)

  p <- prepare_orderly_example("minimal")
  expect_equal(get_remote(p, config), orderly_remote_path(p))

  expect_error(get_remote(TRUE, config),
               "Unknown remote type 'logical'",
               fixed = TRUE)
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


test_that("unpack report failure: corrupt download", {
  skip_on_cran()
  bytes <- as.raw(c(0x50, 0x4b, 0x05, 0x06, rep(0x00, 18L)))
  zip <- tempfile()
  writeBin(bytes, zip)
  ## This test might be platform dependent as a sane unzip function
  ## would have caught this.
  expect_error(suppressWarnings(unzip_archive(zip, tempfile(), NULL, NULL)),
               "Corrupt zip file? No files extracted",
               fixed = TRUE)
})


test_that("unpack failure: not an orderly archive", {
  tmp <- file.path(tempfile(), "parent")
  dir.create(tmp, FALSE, TRUE)
  file.create(file.path(tmp, c("a", "b")))
  zip <- tempfile(fileext = ".zip")
  withr::with_dir(tmp, zip(zip, dir(), extras = "-q"))
  expect_error(unzip_archive(zip, tempfile(), NULL, NULL),
               "Invalid orderly archive")
})


test_that("unpack failure: not expected id", {
  id <- new_report_id()
  tmp <- file.path(tempfile(), id)
  dir.create(tmp, FALSE, TRUE)
  dir.create(file.path(tmp, "orderly.yml"))
  zip <- zip_dir(tmp)
  expect_error(unzip_archive(zip, tempfile(), NULL, "other"),
               sprintf("This is archive '%s' but expected 'other'", id),
               fixed = TRUE)
})


test_that("unpack failure: missing files", {
  id <- new_report_id()
  tmp <- file.path(tempfile(), id)
  dir.create(tmp, FALSE, TRUE)
  dir.create(file.path(tmp, "orderly.yml"))
  zip <- zip_dir(tmp)
  expect_error(unzip_archive(zip, tempfile(), NULL, id),
               "Invalid orderly archive: missing files orderly_run.yml",
               fixed = TRUE)
})
          
test_that("push report (path)", {
  ours <- create_orderly_demo()
  theirs <- prepare_orderly_example("demo")

  remote <- orderly_remote_path(theirs)
  push_archive("multifile-artefact", "latest", ours, remote = remote)

  d <- orderly_list_archive(theirs)
  expect_equal(d$name, "multifile-artefact")
  expect_true(d$id %in% orderly_list_archive(ours)$id)
})
