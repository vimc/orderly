context("demo")

test_that("orderly_demo", {
  path <- create_orderly_demo()
  expect_true(file.exists(path))
})
