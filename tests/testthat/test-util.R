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

test_that("copy failure", {
  path1 <- tempfile()
  path2 <- tempfile()
  writeLines("a", path1)
  writeLines("b", path2)
  on.exit(file.remove(path1, path2))
  expect_error(file_copy(path1, path2), "Error copying files")
  expect_equal(readLines(path2), "b")
})

test_that("resolve_env", {
  set.seed(1)
  v <- paste(sample(c(LETTERS, 0:9, "_"), 20, replace = TRUE), collapse = "")
  vv <- paste0("$", v)
  expect_identical(resolve_env(v), v)
  expect_error(resolve_env(vv),
               sprintf("Environment variable '%s' is not set", v))
  expect_identical(withr::with_envvar(setNames("value", v), resolve_env(vv)),
                   "value")
})

test_that("val_to_bytes", {
  x <- sort(runif(10, 0.1, 0.15)) + 100
  y <- vcapply(x, val_to_bytes, 2)
  expect_equal(sort_c(y), y)
  expect_true(all(nchar(y)), 4)
})

test_that("new_report_id", {
  t <- Sys.time()
  i1 <- new_report_id(t)
  i2 <- new_report_id(t)
  i3 <- t + (1 / 256^2)
  expect_identical(substr(i1, 1, 20), substr(i2, 1, 20))
  expect_false(substr(i1, 1, 20) == substr(i3, 1, 20))
})

## This can't be tested until I get things with vault working; I need
## a test vault really.
test_that("secrets", {
  skip("This is not really testable, yet")
  x <- list(name = "foo",
            password = "VAULT:secret/database/users/readonly:password")
  resolve_secrets(x)
})
