context("remote: api")

test_that("remote_report_names", {
  dat <- prepare_orderly_remote_example(TRUE)
  remote_path <- orderly_remote_path(dat$path_remote)
  reports <- c("depend", "depend2", "example")

  mockery::stub(remote_report_names_api,
                "montagu::montagu_reports_list",
                function(remote)
                  list(name = remote_report_names_path(remote_path)))
  expect_equal(remote_report_names_api(dat$path_remote), reports)

  mockery::stub(remote_report_names,
                "remote_report_names_api",
                function(remote) remote_report_names_path(remote_path))
  expect_equal(remote_report_names(dat$config), reports)
})


test_that("remote_report_versions", {
  dat <- prepare_orderly_remote_example(TRUE)
  remote_path <- orderly_remote_path(dat$path_remote)
  versions <- c(dat$id1, dat$id2)

  mockery::stub(remote_report_versions_api,
                "montagu::montagu_reports_report_versions",
                function(name, error_if_missing, location)
                  remote_report_versions_path(name, remote_path))
  expect_equal(remote_report_versions_api("example", dat$path_remote),
               versions)

  mockery::stub(remote_report_versions,
                "remote_report_versions_api",
                function(name, remote)
                  remote_report_versions_path(name, remote_path))
  expect_equal(remote_report_versions("example", dat$config), versions)

  expect_equal(remote_report_versions("unknown", dat$config), character(0))
})


test_that("pull_archive", {
  dat <- prepare_orderly_remote_example(TRUE)
  remote_path <- orderly_remote_path(dat$path_remote)

  mockery::stub(remote_report_pull_archive_api,
                "montagu::montagu_reports_report_download",
                function(name, id, ..., location)
                  zip_dir(file.path(dat$path_remote, "archive", name, id)))

  capture.output(
    remote_report_pull_archive_api("example", dat$id1, dat$config, NULL))
  dest <- file.path(dat$path_local, "archive", "example", dat$id1)
  expect_true(file.exists(dest))

  ## Then the level above:
  mockery::stub(
    pull_archive,
    "remote_report_pull_archive_api",
    function(name, id, config, remote)
      remote_report_pull_archive_path(name, id, config, remote_path))
  mockery::stub(pull_archive,
                "remote_report_versions",
                c(dat$id1, dat$id2))
  pull_archive("example", dat$id2, dat$config)
})


test_that("push_archive", {
  dat <- prepare_orderly_remote_example(TRUE)
  remote_path <- orderly_remote_path(dat$path_remote)

  id <- orderly_run("example", config = dat$config, echo = FALSE)
  orderly_commit(id, config = dat$config)

  mockery::stub(push_archive,
                "remote_report_versions",
                c(dat$id1, dat$id2))

  expect_error(
    push_archive("example", config = dat$config, remote = dat$remote),
    "'montagu_server' remotes do not support push")
})


test_that("set default", {
  dat <- prepare_orderly_remote_example(TRUE)
  v <- set_default_remote("default", dat$config)
  expect_is(v, "montagu_server")
  expect_identical(v, dat$config$api_server$default$server)
  expect_identical(get_default_remote(dat$config), v)
})


test_that("run", {
  dat <- prepare_orderly_remote_example(TRUE)
  remote_path <- orderly_remote_path(dat$path_remote)
  fake_run <- function(name, ...) {
    id <- orderly_run(name, config = dat$path_remote, echo = FALSE)
    orderly_commit(id, config = dat$path_remote)
    list(name = name, id = id)
  }

  mockery::stub(orderly_run_remote_api,
                "montagu::montagu_reports_run",
                fake_run)

  res <- orderly_run_remote_api("example", config = dat$config,
                                remote = dat$remote)
  expect_equal(res$name, "example")
  expect_match(res$id, VERSION_ID_RE)
  expect_true(file.exists(
    file.path(dat$path_remote, "archive", "example", res$id)))

  mockery::stub(orderly_run_remote,
                "orderly_run_remote_api",
                fake_run)
  res <- orderly_run_remote("example", config = dat$config,
                            remote = dat$remote)
  expect_equal(res$name, "example")
  expect_match(res$id, VERSION_ID_RE)
  expect_true(file.exists(
    file.path(dat$path_remote, "archive", "example", res$id)))

  ## And one check that will certainly move elsewhere later
  remote <- dat$remote
  remote$name <- "production"
  expect_error(
    orderly_run_remote_api("example", ref = "foo", config = dat$config,
                           remote = dat$remote),
    "Can't specify 'ref' on production")
})


test_that("publish", {
  dat <- prepare_orderly_remote_example(TRUE)
  remote_path <- orderly_remote_path(dat$path_remote)
  fake_publish <- function(name, id, value) {
    orderly_publish(id, value, name, dat$path_remote)
  }

  p <- path_orderly_published_yml(
    file.path(dat$path_remote, "archive", "example", c(dat$id1, dat$id2)))
  expect_true(all(!file.exists(p)))

  mockery::stub(orderly_publish_remote_api,
                "montagu::montagu_reports_publish",
                function(name, version, value = NULL, location = NULL)
                  fake_publish(name, version, value))
  res <- orderly_publish_remote_api("example", dat$id1,
                                    value = TRUE,
                                    config = dat$config,
                                    remote = dat$remote)
  expect_equal(yaml_read(p[[1]]), list(published = TRUE))

  mockery::stub(orderly_publish_remote,
                "orderly_publish_remote_api",
                function(name, id, config, value = TRUE, remote = NULL)
                  fake_publish(name, id, value))
  orderly_publish_remote("example", dat$id2, config = dat$config,
                         remote = dat$remote)
  expect_equal(yaml_read(p[[2]]), list(published = TRUE))
})
