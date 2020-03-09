context("logging")

test_that("on/off", {
  oo <- options(orderly.nolog = NULL, crayon.enabled = FALSE)
  on.exit(options(oo))

  expect_true(orderly_log_on())
  expect_true(orderly_log_on())
  expect_null(getOption("orderly.nolog"))
  expect_message(orderly_log("subject", "value"),
                 "[ subject    ]  value", fixed = TRUE)

  expect_true(orderly_log_off())
  expect_false(orderly_log_off())
  expect_silent(orderly_log("subject", "value"))
})


test_that("colour", {
  res <- withr::with_options(
    list(crayon.enabled = TRUE),
    testthat::evaluate_promise(
      orderly_log("topic", c("a", "a"))))
  expect_true(crayon::has_style(res$messages))

  s <- strsplit(res$messages, "\n")[[1]]
  expect_equal(length(s), 2)
  ## same style
  expect_equal(sub("...  ", "topic", s[[2]], fixed = TRUE), s[[1]])

  res <- withr::with_options(
    list(crayon.enabled = FALSE),
    testthat::evaluate_promise(
      orderly_log("topic", c("a", "a"))))
  expect_false(crayon::has_style(res$messages))
})


test_that("warning colourtion", {
  expect_equal(orderly_log_style("warning"), "alert")
  expect_equal(orderly_log_style("unexpected"), "alert")

  expect_equal(orderly_log_style("depends"), "highlight")
  expect_equal(orderly_log_style("anythingelse"), "highlight")
})
