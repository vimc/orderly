context("remote: api (real)")

test_that("end-to-end", {
  skip_on_cran()
  skip("manually run only for now")
  path <- "montagu-reports"
  if (!file.exists(path)) {
    skip("montagu-reports is not present")
  }

  remote <- "uat"
  name <- "internal-2017-population-TUV-MHL"
  id <- "20170823-113855-5091025f"

  v <- remote_report_names(config = path, remote = remote)
  expect_true(name %in% v)

  expect_equal(
    remote_report_versions("unknown", config = path, remote = remote),
    character(0))

  v <- remote_report_versions(name, config = path, remote = remote)
  expect_true(id %in% v)

  dest <- file.path(path, "archive", name, id)
  unlink(dest, recursive = TRUE)
  pull_archive(name, id, config = path, remote = remote)
  expect_true(file.exists(dest))

  orderly_run_remote(name, config = path, remote = remote)
})
