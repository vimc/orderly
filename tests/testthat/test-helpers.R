context("helpers")

test_that("can add resource", {
  path <- prepare_orderly_example("minimal")
  file.create(file.path(path, "src", "example", "new.txt"))

  res <- orderly_use_resource("new.txt", root = path, name = "example",
                              show = FALSE, prompt = FALSE)

  config <- orderly_config(path)
  info <- recipe_read(file.path(path, "src", "example"), config)
  expect_equal(info$resources, "new.txt")
})
