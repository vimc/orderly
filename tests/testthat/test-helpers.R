context("helpers")

test_that("can add resource to an orderly without resources", {
  path <- test_prepare_orderly_example("minimal")
  file.create(file.path(path, "src", "example", "new.txt"))

  res <- orderly_use_resource("new.txt", root = path, name = "example",
                              show = FALSE, prompt = FALSE)

  config <- orderly_config_$new(path)
  info <- orderly_recipe$new("example", config)
  expect_equal(info$resources, "new.txt")
})


test_that("can add resource to an orderly with resources", {
  path <- test_prepare_orderly_example("demo")
  p <- file.path(path, "src", "use_resource", "orderly.yml")
  prev <- yaml_read(p)$resources

  file.create(file.path(path, "src", "use_resource",
                        c("a.txt", "b.txt", "c.txt")))

  res <- orderly_use_resource("a.txt", root = path, name = "use_resource",
                              show = FALSE, prompt = FALSE)
  expect_equal(yaml_read(p)$resources, c(prev, "a.txt"))

  res <- orderly_use_resource(c("b.txt", "c.txt"), root = path,
                              name = "use_resource",
                              show = FALSE, prompt = FALSE)
  expect_equal(yaml_read(p)$resources, c(prev, "a.txt", "b.txt", "c.txt"))
})


test_that("can add resource non-block resources", {
  path <- test_prepare_orderly_example("minimal")
  file.create(file.path(path, "src", "example", c("a.txt", "b.txt")))
  p <- file.path(path, "src", "example", "orderly.yml")
  text <- readLines(p)
  dat <- yaml_block_info("data", text)
  writeLines(filediff(text, dat$end, "resources: a.txt")$result, p)

  res <- orderly_use_resource("b.txt", root = path, name = "example",
                              show = FALSE, prompt = FALSE)

  expect_equal(res$result[seq_len(3) + dat$end],
               c("resources:", "  - a.txt", "  - b.txt"))
  expect_equal(yaml_read(p)$resources, c("a.txt", "b.txt"))
})


test_that("require added resources to exist", {
  path <- test_prepare_orderly_example("minimal")
  expect_error(
    orderly_use_resource("new.txt", root = path, name = "example",
                         show = FALSE, prompt = FALSE),
    "Resource does not exist: 'new.txt'")
  expect_error(
    orderly_use_resource(c("a.txt", "b"), root = path, name = "example",
                         show = FALSE, prompt = FALSE),
    "Resource does not exist: 'a.txt', 'b'")
})


test_that("require added resources to not already be declared", {
  path <- test_prepare_orderly_example("minimal")
  p <- file.path(path, "src", "example", "orderly.yml")
  file.create(file.path(path, "src", "example",
                        c("a.txt", "b.txt", "c.txt")))
  res <- orderly_use_resource(c("a.txt", "b.txt", "c.txt"),
                              root = path, name = "example",
                              show = FALSE, prompt = FALSE)
  expect_error(
    orderly_use_resource(c("a.txt", "b.txt", "c.txt"),
                         root = path, name = "example",
                         show = FALSE, prompt = FALSE),
    "Resource already declared: 'a.txt', 'b.txt', 'c.txt'")
})


test_that("require added resources to be unique", {
  path <- test_prepare_orderly_example("minimal")
  p <- file.path(path, "src", "example", "orderly.yml")
  file.create(file.path(path, "src", "example",
                        c("a.txt", "b.txt", "c.txt")))
  expect_error(
    orderly_use_resource(c("a.txt", "b.txt", "a.txt"),
                         root = path, name = "example",
                         show = FALSE, prompt = FALSE),
    "Resource duplicated: 'a.txt'")
})


## This uses the same codepath as above, so I've just done a very
## rough job of testing here.
test_that("Add source (minimal test)", {
  path <- test_prepare_orderly_example("minimal")
  file.create(file.path(path, "src", "example", "new.R"))
  res <- orderly_use_source("new.R", root = path, name = "example",
                            show = FALSE, prompt = FALSE)
  config <- orderly_config_$new(path)
  info <- orderly_recipe$new("example", config)
  expect_equal(info$sources, "new.R")
  expect_error(orderly_use_source("new.R", root = path, name = "example",
                                  show = FALSE, prompt = FALSE),
               "Source already declared: 'new.R'")
})


test_that("Add packages (minimal test)", {
  path <- test_prepare_orderly_example("minimal")
  res <- orderly_use_package("knitr", root = path, name = "example",
                             show = FALSE, prompt = FALSE)
  config <- orderly_config_$new(path)
  info <- orderly_recipe$new("example", config)
  expect_equal(info$packages, "knitr")
  expect_error(orderly_use_package("knitr", root = path, name = "example",
                                   show = FALSE, prompt = FALSE),
               "Package already declared: 'knitr'")
})


## Test for #177
test_that("Add packages to malformed packages section", {
  path <- test_prepare_orderly_example("minimal")
  p <- file.path(path, "src", "example", "orderly.yml")
  txt <- readLines(p)
  writeLines(c(txt, "packages:"), p)
  res <- orderly_use_package("knitr", name = "example", root = path,
                             show = FALSE, prompt = FALSE)
  expect_equal(tail(res$result, 2),
               c("packages:", "  - knitr"))
})


test_that("Add resource to incomplete orderly.yml", {
  path <- test_prepare_orderly_example("minimal")
  p <- orderly_new("partial", root = path, quiet = TRUE)
  res <- orderly_use_package("knitr", name = "partial", root = path,
                             show = FALSE, prompt = FALSE)
  expect_equal(tail(res$result, 2),
               c("packages:", "  - knitr"))
})


test_that("Create gitignore", {
  path <- test_prepare_orderly_example("minimal")
  res <- orderly_use_gitignore(path, prompt = FALSE, show = FALSE)

  contents <- readLines(file.path(path, ".gitignore"))
  expect_equal(
    contents,
    readLines(orderly_file("init/gitignore")))
  expect_equal(contents, res$value)
  expect_true(res$create)
  expect_is(res, "filediff")

  ## And again:
  expect_message(
    res <- orderly_use_gitignore(path, prompt = FALSE, show = FALSE),
    "No changes to make to '.*/\\.gitignore'")
  expect_equal(res$changed, integer(0))
  expect_false(res$create)
})


test_that("update existing gitignore", {
  path <- test_prepare_orderly_example("minimal")
  writeLines(c("data", "*.sqlite", "something"),
             file.path(path, ".gitignore"))
  res <- orderly_use_gitignore(path, prompt = FALSE, show = FALSE)

  ## All the "interesting" lines in the gitignore
  cmp <- readLines(orderly_file("init/gitignore"))
  cmp <- cmp[grepl("^[^#[:space:]]", cmp)]

  ## Check what we updated:
  expect_true(all(cmp %in% res$result))
  expect_setequal(res$value, setdiff(cmp, c("data", "*.sqlite")))

  ## And again:
  expect_message(
    res <- orderly_use_gitignore(path, prompt = FALSE, show = FALSE),
    "No changes to make to '.*/\\.gitignore'")
  expect_equal(res$changed, integer(0))
  expect_false(res$create)
})
