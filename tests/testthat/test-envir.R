context("envir")

test_that("set env", {
  path <- prepare_orderly_example("minimal")
  cfg <- c("source:",
           "  driver: RSQLite::SQLite",
           "  dbname: source.sqlite",
           "  user: $MY_USER")
  writeLines(cfg, file.path(path, "orderly_config.yml"))
  expect_error(orderly_config(path),
               "Environment variable 'MY_USER' is not set")

  writeLines(c("MY_USER: foo"), path_orderly_envir_yml(path))
  cfg <- orderly_config(path)
  expect_equal(cfg$source$args$user, "foo")

  writeLines(c("MY_USER: bar"), path_orderly_envir_yml(path))
  cfg <- orderly_config(path)
  expect_equal(cfg$source$args$user, "bar")
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
