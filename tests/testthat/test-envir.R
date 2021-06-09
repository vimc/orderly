context("envir")

test_that("set env", {
  path <- test_prepare_orderly_example("minimal")
  cfg <- c("database:",
           "  source:",
           "    driver: RSQLite::SQLite",
           "    args:",
           "      dbname: source.sqlite",
           "      user: $MY_USER")
  writeLines(cfg, file.path(path, "orderly_config.yml"))

  config <- orderly_config_$new(path)

  expect_error(orderly_db_args(config$database$source, config, "loc"),
               "Environment variable 'MY_USER' is not set")

  writeLines(c("MY_USER: foo"), path_orderly_envir_yml(path))
  x <- orderly_db_args(config$database$source, config)
  expect_equal(x$args$user, "foo")

  writeLines(c("MY_USER: bar"), path_orderly_envir_yml(path))
  x <- orderly_db_args(config$database$source, config)
  expect_equal(x$args$user, "bar")
})

test_that("read env", {
  path <- tempfile()
  dir.create(path)

  filename <- path_orderly_envir_yml(path)

  writeLines(c("foo: 1", "foo: 2"), filename)
  expect_error(orderly_envir_read(path))

  writeLines(c("- foo: 1", "- foo: 2"), filename)
  expect_error(orderly_envir_read(path),
               "must be named")

  writeLines(c("foo: 1", "bar: [2, 3]"), filename)
  expect_error(
    orderly_envir_read(path),
    "Expected all elements of orderly_envir.yml to be scalar (check 'bar')",
    fixed = TRUE)
})

test_that("non-character data is OK", {
  path <- tempfile()
  dir.create(path)
  filename <- path_orderly_envir_yml(path)
  writeLines(c("foo: 1", "bar: 2"), filename)
  dat <- orderly_envir_read(path)
  expect_equal(dat, c(foo = "1", bar = "2"))
})


test_that("remove null values", {
  path <- tempfile()
  dir.create(path)
  filename <- path_orderly_envir_yml(path)
  writeLines(c("foo: ~", "bar: 2"), filename)
  dat <- orderly_envir_read(path)
  expect_equal(dat, c(bar = "2"))
})
