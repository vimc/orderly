context("cleanup")

test_that("cleanup nothing", {
  path <- prepare_orderly_example("minimal")
  expect_null(orderly_cleanup(config = path))
  expect_equal(orderly_list(config = path), "example")
})

test_that("cleanup draft", {
  path <- prepare_orderly_example("minimal")
  id <- orderly_run("example", config = path, echo = FALSE)
  expect_equal(nrow(orderly_list_drafts(config = path)), 1)
  expect_equal(length(orderly_db("csv", path)$list()), 1)
  orderly_cleanup(config = path)
  expect_equal(nrow(orderly_list_drafts(config = path)), 0)
  expect_equal(length(orderly_db("csv", path)$list()), 0)
})

test_that("cleanup with archive", {
  path <- prepare_orderly_example("minimal")
  id1 <- orderly_run("example", config = path, echo = FALSE)
  id2 <- orderly_run("example", config = path, echo = FALSE)

  expect_equal(nrow(orderly_list_drafts(config = path)), 2)
  expect_equal(length(orderly_db("csv", path)$list()), 1)

  orderly_commit(id2, config = path)

  orderly_cleanup(config = path)
  expect_equal(nrow(orderly_list_drafts(config = path)), 0)
  expect_equal(nrow(orderly_list_archive(config = path)), 1)
  expect_equal(length(orderly_db("csv", path)$list()), 1)
})
