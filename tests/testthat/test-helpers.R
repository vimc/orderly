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
