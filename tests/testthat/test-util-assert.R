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

test_that("assert_file_exists", {
  path <- tempfile()
  expect_error(assert_file_exists(path), "File does not exist")
  writeLines(character(0), path)
  expect_silent(assert_file_exists(path))
})


test_that("assert_file_exists: error in case", {
  mockery::stub(assert_file_exists, "file_exists",
                structure(c(TRUE, FALSE, FALSE),
                          incorrect_case = c(FALSE, TRUE, FALSE),
                          correct_case = c("FOO" = "foo")))
  expect_error(assert_file_exists(c("bar", "FOO", "gaz")),
               "File does not exist: 'FOO' (should be 'foo'), 'gaz'",
               fixed = TRUE)
})


test_that("assert_is_directory", {
  path <- tempfile()
  expect_error(assert_is_directory(path), "File does not exist")
  file.create(path)
  expect_error(assert_is_directory(path), "File exists but is not a directory")
  expect_silent(assert_is_directory("."))
})
