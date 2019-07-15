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

  v <- remote_report_names(root = path, remote = remote)
  expect_true(name %in% v)

  expect_equal(
    remote_report_versions("unknown", root = path, remote = remote),
    character(0))

  v <- remote_report_versions(name, root = path, remote = remote)
  expect_true(id %in% v)

  dest <- file.path(path, "archive", name, id)
  unlink(dest, recursive = TRUE)
  orderly_pull_archive(name, id, root = path, remote = remote)
  expect_true(file.exists(dest))

  orderly_run_remote(name, root = path, remote = remote)
})
