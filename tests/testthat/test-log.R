context("logging")

test_that("on/off", {
  oo <- options(orderly.nolog = NULL) # default setting to start
  on.exit(options(oo))

  expect_true(orderly_log_on())
  expect_true(orderly_log_on())
  expect_null(getOption("orderly.nolog"))
  expect_message(orderly_log("subject", "value"),
                 "[ subject     ]  value", fixed = TRUE)

  expect_true(orderly_log_off())
  expect_false(orderly_log_off())
  expect_silent(orderly_log("subject", "value"))
})
