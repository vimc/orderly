context("query")

test_that("empty", {
  path <- tempfile()
  orderly_init(path, quiet = TRUE)
  file_copy("minimal_config.yml", file.path(path, "orderly_config.yml"),
            overwrite = TRUE)
  expect_equal(orderly_list(path), character(0))
})

test_that("non-empty", {
  path <- prepare_minimal()
  expect_equal(orderly_list(path), "example")
})
