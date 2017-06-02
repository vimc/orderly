context("utils")

test_that("yaml_read throws nicely", {
  expect_error(suppressWarnings(yaml_read("foo")), "while reading 'foo'")
})

test_that("string symbol parse", {
  expect_equal(check_symbol_from_str("a::b"), c("a", "b"))
  expect_error(check_symbol_from_str("a", "name"),
               "Expected fully qualified name for name")
  expect_error(check_symbol_from_str("a::b::c", "name"),
               "Expected fully qualified name for name")
})

test_that("Descend failure", {
  path <- tempfile()
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE))
  expect_null(find_file_descend(".orderly_foobar", tempdir(), path))
  expect_null(find_file_descend(".orderly_foobar", "/", path))
  expect_null(find_file_descend(".orderly_foobar", "/", "/"))
})
