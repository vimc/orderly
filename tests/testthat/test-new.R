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
  p <- file.path(path, "template", "default")
  dir.create(p, FALSE, TRUE)

  content <- "testing"
  writeLines(content, file.path(p, "orderly.yml"))

  orderly_new("testing", path, quiet = TRUE)
  expect_identical(readLines(file.path(path, "src", "testing", "orderly.yml")),
                   content)
})


test_that("custom template can be overriden by system", {
  path <- prepare_orderly_example("minimal")
  p <- file.path(path, "template", "default")
  dir.create(p, FALSE, TRUE)

  content <- "testing"
  writeLines(content, file.path(p, "orderly.yml"))

  orderly_new("testing", path, quiet = TRUE, template = "system")
  expect_identical(readLines(file.path(path, "src", "testing", "orderly.yml")),
                   readLines(orderly_file("orderly_example.yml")))
})


test_that("custom template copies all files", {
  path <- prepare_orderly_example("minimal")

  p <- file.path(path, "template", "foo")
  dir.create(p, FALSE, TRUE)
  content <- "testing"
  writeLines(content, file.path(p, "orderly.yml"))
  dir.create(file.path(p, "R"))
  writeLines("some content", file.path(p, "R", "functions.R"))
  writeLines("hidden content", file.path(p, ".hidden"))

  orderly_new("testing", path, quiet = TRUE, template = "foo")

  list_files <- function(...) {
    sort(dir(file.path(...), all.files = TRUE, recursive = TRUE))
  }
  hash_files <- function(...) {
    files <- list_files(...)
    set_names(tools::md5sum(file.path(file.path(...), files)), files)
  }

  expect_equal(list_files(path, "src", "testing"),
               list_files(p))
  expect_equal(hash_files(path, "src", "testing"),
               hash_files(p))
})


test_that("missing templates are an error", {
  path <- prepare_orderly_example("minimal")
  expect_error(
    orderly_new("testing", path = path, template = "foo"),
    "Did not find file 'template/foo/orderly.yml' within orderly root")
  expect_false(file.exists(file.path(path, "src", "testing")))
})
