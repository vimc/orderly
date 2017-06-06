context("util (assert)")

test_that("assert_scalar", {
  expect_error(assert_scalar(NULL), "must be a scalar")
  expect_error(assert_scalar(numeric(0)), "must be a scalar")
  expect_error(assert_scalar(1:2), "must be a scalar")
})

test_that("assert_character", {
  expect_error(assert_character(1), "must be character")
  expect_error(assert_character(TRUE), "must be character")
})

test_that("assert_numeric", {
  expect_error(assert_numeric("one"), "must be numeric")
  expect_error(assert_numeric(TRUE), "must be numeric")
})

test_that("assert_logical", {
  expect_error(assert_logical("one"), "must be logical")
  expect_error(assert_logical(1), "must be logical")
})

test_that("assert_hash", {
  expect_error(assert_hash("one"), "must be a hash")
  expect_silent(assert_hash(strrep("a", 32)))
})

test_that("assert_named", {
  expect_error(assert_named(1), "must be named")
  expect_error(assert_named(setNames(1:2, c("a", "a")), TRUE),
               "must have unique names")
  expect_silent(assert_named(setNames(1:2, c("a", "a")), FALSE))
})

test_that("assert_is", {
  expect_error(assert_is("x", "foo"), "must be a foo")
  expect_silent(assert_is(structure("x", class = "foo"), "foo"))
})

test_that("match_value", {
  expect_error(match_value("foo", letters), "must be one of")
  expect_silent(match_value("a", letters))
})

test_that("assert_type", {
  expect_silent(assert_type(TRUE, "logical"))
  expect_silent(assert_type(1, "numeric"))
  expect_silent(assert_type("a", "character"))

  expect_error(assert_type("one", "logical"), "must be logical")
  expect_error(assert_type("one", "numeric"), "must be numeric")
  expect_error(assert_type(1, "character"), "must be character")
})
