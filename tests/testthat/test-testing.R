context("testing")

test_that("orderly_example can be quiet", {
  skip_on_cran_windows()
  expect_log_message(
    orderly_example("minimal", run_demo = TRUE, quiet = FALSE),
    "[ success", fixed = TRUE)
  expect_silent(
    orderly_example("minimal", run_demo = TRUE, quiet = TRUE))
})
