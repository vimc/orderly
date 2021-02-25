context("testing")

test_that("orderly_example can be quiet", {
  skip_on_cran_windows()
  expect_log_message(
    orderly_example("minimal", run_demo = TRUE, quiet = FALSE),
    "[ success", fixed = TRUE)
  expect_silent(
    orderly_example("minimal", run_demo = TRUE, quiet = TRUE))
})


test_that("create_orderly_demo can create a git repo", {
  skip_on_cran()
  path <- create_orderly_demo(quiet = TRUE, git = TRUE)
  expect_true(file.exists(file.path(path, ".git")))
  expect_true(same_path(gert::git_pull(repo = path), path))
})
