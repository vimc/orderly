context("file store")

test_that("basic (rds)", {
  st <- file_store_rds(tempfile())
  expect_is(st, "file_store")

  expect_equal(st$list(), character(0))
  h <- st$set(mtcars)
  expect_equal(h, digest::digest(mtcars))
  expect_identical(st$get(h), mtcars)
  expect_true(st$exists(h))
  expect_equal(st$list(), h)

  expect_true(st$del(h))
  expect_equal(st$list(), character(0))
})
