context("remote: api")

test_that("remote_report_names", {
  dat <- prepare_orderly_remote_example(TRUE)
  remote_path <- orderly_remote_path(dat$path_remote)

  mockery::stub(remote_report_names_api,
                "montagu::montagu_reports_list",
                function(remote)
                  list(name = remote_report_names_path(remote_path)))
  expect_equal(remote_report_names_api(dat$path_remote), "example")

  mockery::stub(remote_report_names,
                "remote_report_names_api",
                function(remote) remote_report_names_path(remote_path))
  expect_equal(remote_report_names(dat$config), "example")
})


test_that("remote_report_versions", {
  dat <- prepare_orderly_remote_example(TRUE)
  remote_path <- orderly_remote_path(dat$path_remote)
  versions <- c(dat$id1, dat$id2)

  mockery::stub(remote_report_versions_api,
                "montagu::montagu_reports_report_versions",
                function(name, remote)
                  remote_report_versions_path(name, remote_path))
  expect_equal(remote_report_versions_api("example", dat$path_remote),
               versions)

  mockery::stub(remote_report_versions,
                "remote_report_versions_api",
                function(name, remote)
                  remote_report_versions_path(name, remote_path))
  expect_equal(remote_report_versions("example", dat$config), versions)
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
  mockery::stub(pull_archive,
                "remote_report_pull_archive_api",
                function(name, id, config, remote)
                  remote_report_pull_archive_path(name, id, config, remote_path))
  mockery::stub(pull_archive,
                "remote_report_versions",
                c(dat$id1, dat$id2))
  pull_archive("example", dat$id2, dat$config)
})
