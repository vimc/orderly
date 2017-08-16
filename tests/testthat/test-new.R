context("new")

test_that("template", {
  path <- prepare_orderly_example("minimal")
  name <- "foo"
  expect_message(orderly_new(name, path), "Edit the file")
  expect_true(name %in% orderly_list(path))
  expect_true(file.exists(file.path(path, "src", name, "orderly.yml")))
})

test_that("don't overwrite", {
  path <- prepare_orderly_example("minimal")
  expect_error(orderly_new("example", path),
               "A report already exists called 'example'")
})

test_that("don't allow spaces", {
  path <- prepare_orderly_example("minimal")
  expect_error(orderly_new("my report", path),
               "'name' cannot contain spaces")
})
