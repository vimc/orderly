context("testing")

test_that("ordrly_example can be quiet", {
  expect_message(
    orderly_example("minimal", run_demo = TRUE, quiet = FALSE),
    "[ success", fixed = TRUE)
  expect_silent(
    orderly_example("minimal", run_demo = TRUE, quiet = TRUE))
})
