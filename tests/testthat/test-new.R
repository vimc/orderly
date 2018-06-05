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

test_that("custom fields", {
  path <- prepare_orderly_example("minimal")
  yml <- file.path(path, "orderly_config.yml")
  txt <- readLines(yml)
  fields <- c("fields:",
              "  foo:",
              "    type: character",
              "    required: true",
              "    description: >-",
              "      A field that stores foos.  This is a long",
              "      description designed to flow to several lines because",
              "      the wrapping will need some testing",
              "  bar:",
              "    type: numeric",
              "    required: false",
              "    description: A field that stores bars")
  writeLines(c(txt, fields), yml)

  name <- "test"
  dest <- orderly_new(name, path)

  yml <- file.path(dest, "orderly.yml")
  txt <- readLines(yml)
  expect_equal(sum(grepl("A field that stores foos", txt)), 1)
  expect_equal(sum(grepl("A field that stores bars", txt)), 1)

  dat <- yaml_read(yml)
  expect_equal(dat["foo"], list(foo = NULL))
  expect_false("bar" %in% names(dat))
})


test_that("custom template is copied", {
  path <- prepare_orderly_example("minimal")
  dir.create(file.path(path, "orderly"))
  content <- "testing"
  writeLines(content, file.path(path, "orderly", "template.yml"))

  orderly_new("testing", path, quiet = TRUE)

  expect_identical(readLines(file.path(path, "src", "testing", "orderly.yml")),
                   content)
})
