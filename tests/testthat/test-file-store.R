context("file store")

test_that("basic (rds)", {
  skip_on_cran_windows()
  st <- file_store_rds(tempfile())
  expect_is(st, "file_store")

  expect_equal(st$list(), character(0))
  h <- st$set(mtcars)
  expect_equal(h, digest::digest(mtcars))
  expect_identical(st$get(h), mtcars)
  expect_true(st$exists(h))
  expect_equal(st$list(), h)

  filename <- st$filename(h)
  expect_equal(basename(filename), sprintf("%s.rds", h))
  expect_equal(readRDS(filename), st$get(h))

  expect_true(st$del(h))
  expect_equal(st$list(), character(0))
})


test_that("basic (csv)", {
  skip_on_cran_windows()
  st <- file_store_csv(tempfile())
  expect_is(st, "file_store")

  expect_equal(st$list(), character(0))
  h <- st$set(mtcars)
  expect_equal(h, digest::digest(mtcars))
  expect_equivalent(st$get(h), mtcars)
  expect_true(st$exists(h))
  expect_equal(st$list(), h)

  filename <- st$filename(h)
  expect_equal(basename(filename), sprintf("%s.csv", h))
  expect_equal(read_csv(filename), st$get(h))

  expect_true(st$del(h))
  expect_equal(st$list(), character(0))
})

test_that("destroy", {
  path <- tempfile()
  st <- file_store_rds(path)
  expect_is(st, "file_store")
  expect_true(file.exists(path))
  expect_true(is_directory(path))

  expect_equal(dir(path), character(0))
  st$set(mtcars)
  h <- st$list()
  expect_equal(length(h), 1)
  expect_equal(dir(path), paste0(h, ".rds"))

  st$destroy()
  expect_false(file.exists(path))
})

test_that("get missing hash", {
  path <- tempfile()
  st <- file_store_rds(path)
  expect_error(st$get(ids::random_id()),
               "hash '[[:xdigit:]]{32}' not found",
               class = "hash_error")
})

test_that("mget", {
  skip_on_cran_windows()
  path <- tempfile()
  st <- file_store_rds(path)

  h1 <- st$set(mtcars)
  h2 <- st$set(iris)
  h3 <- ids::random_id()

  ## Corner case
  expect_equal(st$mget(character(0)), list())

  ## Reasonable cases:
  expect_equal(st$mget(h1), list(mtcars))
  expect_equal(st$mget(c(h1, h2)), list(mtcars, iris))
  expect_equal(st$mget(c(h1, h1)), list(mtcars, mtcars))

  ## Missing
  expect_equal(st$mget(h3), structure(list(NULL), missing = 1L))
  expect_equal(st$mget(c(h3, h3)),
               structure(list(NULL, NULL), missing = c(1L, 2L)))
  expect_equal(st$mget(c(h1, h3)),
               structure(list(mtcars, NULL), missing = 2L))

  expect_equal(st$mget(c(h1, h3), list("a")),
               structure(list(mtcars, list("a")), missing = 2L))

  ## Named
  expect_equal(st$mget(c(a = h1, b = h2)),
               list(a = mtcars, b = iris))
  expect_equal(st$mget(c(a = h1, h2)),
               list(a = mtcars, iris))
})
