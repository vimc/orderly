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

test_that("cleanup keeps draft data", {
  path <- prepare_orderly_example("minimal")
  id <- orderly_run("example", config = path, echo = FALSE)
  rds <- orderly_db("rds", config = path)
  h <- rds$list()
  orderly_cleanup(config = path, draft = FALSE)
  expect_identical(rds$list(), h)
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

test_that("cleanup failed", {
  path <- prepare_orderly_example("minimal")
  id1 <- orderly_run("example", config = path, echo = FALSE)
  id2 <- orderly_run("example", config = path, echo = FALSE)

  append_text(file.path(path, "src", "example", "script.R"),
              "stop('this is an error')")
  id3 <- tryCatch(orderly_run("example", config = path, echo = FALSE),
                  error = function(e) NULL)
  expect_null(id3)
  d <- orderly_list_drafts("example", config = path)
  expect_equal(nrow(d), 3)

  orderly_cleanup(config = path, failed_only = TRUE)
  d <- orderly_list_drafts("example", config = path)
  expect_equal(nrow(d), 2)
  expect_true(all(c(id1, id2) %in% d$id))
})

test_that("cleanup by name", {
  path <- prepare_orderly_example("demo")
  id1 <- orderly_run("minimal", config = path, echo = FALSE)
  id2 <- orderly_run("other", list(nmin = 0), config = path, echo = FALSE)
  orderly_cleanup("minimal", path)
  expect_equal(orderly_list_drafts(path),
               data.frame(name = "other", id = id2, stringsAsFactors = FALSE))
})
