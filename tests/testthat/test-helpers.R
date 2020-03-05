context("helpers")

test_that("can add resource to an orderly without resources", {
  path <- prepare_orderly_example("minimal")
  file.create(file.path(path, "src", "example", "new.txt"))

  res <- orderly_use_resource("new.txt", root = path, name = "example",
                              show = FALSE, prompt = FALSE)

  config <- orderly_config(path)
  info <- recipe_read(file.path(path, "src", "example"), config)
  expect_equal(info$resources, "new.txt")
})


test_that("can add resource to an orderly with resources", {
  path <- prepare_orderly_example("demo")
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
  path <- prepare_orderly_example("minimal")
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
  path <- prepare_orderly_example("minimal")
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
  path <- prepare_orderly_example("minimal")
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
  path <- prepare_orderly_example("minimal")
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
  path <- prepare_orderly_example("minimal")
  file.create(file.path(path, "src", "example", "new.R"))
  res <- orderly_use_source("new.R", root = path, name = "example",
                            show = FALSE, prompt = FALSE)
  config <- orderly_config(path)
  info <- recipe_read(file.path(path, "src", "example"), config)
  expect_equal(info$sources, "new.R")
  expect_error(orderly_use_source("new.R", root = path, name = "example",
                                  show = FALSE, prompt = FALSE),
               "Source already declared: 'new.R'")
})


test_that("Add packages (minimal test)", {
  path <- prepare_orderly_example("minimal")
  res <- orderly_use_package("knitr", root = path, name = "example",
                             show = FALSE, prompt = FALSE)
  config <- orderly_config(path)
  info <- recipe_read(file.path(path, "src", "example"), config)
  expect_equal(info$packages, "knitr")
  expect_error(orderly_use_package("knitr", root = path, name = "example",
                                   show = FALSE, prompt = FALSE),
               "Package already declared: 'knitr'")
})


## Test for #177
test_that("Add packages to malformed packages section", {
  path <- prepare_orderly_example("minimal")
  p <- file.path(path, "src", "example", "orderly.yml")
  txt <- readLines(p)
  writeLines(c(txt, "packages:"), p)
  res <- orderly_use_package("knitr", name = "example", root = path,
                             show = FALSE, prompt = FALSE)
  expect_equal(tail(res$result, 2),
               c("packages:", "  - knitr"))
})


test_that("Add resource to incomplete orderly.yml", {
  path <- prepare_orderly_example("minimal")
  p <- orderly_new("partial", root = path, quiet = TRUE)
  res <- orderly_use_package("knitr", name = "partial", root = path,
                             show = FALSE, prompt = FALSE)
  expect_equal(tail(res$result, 2),
               c("packages:", "  - knitr"))
})


test_that("Create gitignore", {
  path <- prepare_orderly_example("minimal")
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
  path <- prepare_orderly_example("minimal")
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


test_that("Add a dependency", {
  path <- prepare_orderly_example("minimal")
  p <- orderly_new("use", root = path)
  res <- orderly_use_dependency("example", "mygraph.png",
                                name = "use", root = path, prompt = FALSE,
                                show = FALSE)
  expected <- c("depends:",
                 "  example:",
                "    id: latest",
                "    use:",
                "      mygraph.png: mygraph.png")
  expect_is(res, "filediff")
  expect_equal(length(res$changed), length(expected))
  expect_false(res$create)
  expect_equal(res$value, expected)
  expect_equal(readLines(file.path(p, "orderly.yml")), res$result)

  ## Check that all put together we do manage to get the files into
  ## the directory.  Doing this well requires an upstream report to exist.
  skip_on_cran_windows()
  id <- orderly_run("example", root = path, echo = FALSE)
  p <- orderly_develop_start("use", root = path, use_draft = TRUE)
  expect_setequal(dir(p), c("orderly.yml", "mygraph.png"))

  expect_equal(
    hash_files(file.path(p, "mygraph.png"), FALSE),
    hash_files(file.path(path, "draft", "example", id, "mygraph.png"), FALSE))
})


test_that("Add and rename dependency", {
  path <- prepare_orderly_example("minimal")
  p <- orderly_new("use", root = path)
  res <- orderly_use_dependency("example", "mygraph.png", "imported.png",
                                name = "use", root = path, prompt = FALSE,
                                show = FALSE)
  expected <- c("depends:",
                 "  example:",
                "    id: latest",
                "    use:",
                "      imported.png: mygraph.png")
  expect_equal(length(res$changed), length(expected))
})


test_that("Add multiple dependencies", {
  path <- prepare_orderly_example("depends", testing = TRUE)
  p <- orderly_new("use", root = path)
  res <- orderly_use_dependency("depend",
                                c("mygraph.png", "output.rds"),
                                c("upstream.png", "upstream.rds"),
                                name = "use", root = path, prompt = FALSE,
                                show = FALSE)
  expected <- c("depends:",
                 "  example:",
                "    id: latest",
                "    use:",
                "      upstream.png: mygraph.png",
                "      upstream.rds: output.rds")
  expect_equal(length(res$changed), length(expected))
})


test_that("validation when adding a dependency", {
  path <- prepare_orderly_example("depends", testing = TRUE)
  p <- orderly_new("use", root = path)

  ## Reduce noise below:
  f <- function(..., name = "use", root = path, prompt = FALSE, show = FALSE) {
    orderly_use_dependency(
      ..., name = name, root = root, prompt = prompt, show = show)
  }

  expect_error(
    f("missing", "data.csv"),
    "Report source directory does not exist")

  ## Check names:
  expect_error(
    f("example", "data.csv"),
    paste("Requested filename not an artefact of 'example': 'data.csv'",
          "Valid options: 'data.rds'", sep = "\n"),
    fixed = TRUE)
  expect_error(
    f("example", c("a", "b")),
    paste("Requested filename not an artefact of 'example': 'a', 'b'",
          "Valid options: 'data.rds'", sep = "\n"),
    fixed = TRUE)
  expect_error(
    f("example", c("a", "data.rds", "b")),
    paste("Requested filename not an artefact of 'example': 'a', 'b'",
          "Valid options: 'data.rds'", sep = "\n"),
    fixed = TRUE)

  expect_error(
    f("example", "data.rds", id = "previous"),
    "id must be a valid report id or 'latest'")

  expect_error(
    f("example", c("data.rds", "data.rds"), as = c("a", "a")),
    "Duplicates are not allowed in 'as' (found 'a')",
    fixed = TRUE)

  expect_error(
    f("example", c("data.rds", "data.rds"), as = c("a")),
    "'as' must have the same length as 'filename' (2)",
    fixed = TRUE)
})
