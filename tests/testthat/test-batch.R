context("batch")

test_that("reports can be batch run", {
  path <- test_prepare_orderly_example("parameters", testing = TRUE)

  params <- data_frame(
    a = c("one", "two", "three"),
    b = c(1, 2, 3)
  )
  batch_id <- ids::random_id()
  mockery::stub(orderly_batch, "ids::random_id", batch_id)
  output <- orderly_batch("example", parameters = params,
                       root = path, echo = FALSE)
  expect_setequal(colnames(output), c("id", "success", "a", "b"))
  expect_equal(output[, c("a", "b")], params)
  expect_equal(output$success, rep(TRUE, 3))
  data <- lapply(output$id, function(id) {
    readRDS(path_orderly_run_rds(file.path(path, "draft", "example", id)))
  })
  invisible(lapply(data, function(d) {
    expect_equal(d$meta$batch_id, batch_id)
  }))
})

test_that("batch running with a single param retains name", {
  path <- test_prepare_orderly_example("demo")

  params <- data.frame(nmin = c(0.2, 0.25))
  batch_id <- ids::random_id()
  mockery::stub(orderly_batch, "ids::random_id", batch_id)
  output <- orderly_batch("other", params, root = path, echo = FALSE)
  expect_setequal(colnames(output), c("id", "success", "nmin"))
  expect_equal(output[, "nmin", drop = FALSE], params)
  expect_equal(output$success, rep(TRUE, 2))
  data <- lapply(output$id, function(id) {
    readRDS(path_orderly_run_rds(file.path(path, "draft", "other", id)))
  })
  invisible(lapply(data, function(d) {
    expect_equal(d$meta$batch_id, batch_id)
  }))
})

test_that("return useful error if params passed without names", {
  path <- test_prepare_orderly_example("parameters", testing = TRUE)

  params <- data_frame(
    c("one", "two", "three"),
    c(1, 2, 3)
  )
  expect_error(
    orderly_batch("example", parameters = params, root = path, echo = FALSE),
    "Missing parameters: 'a', 'b'",
    fixed = TRUE
  )
})

test_that("return useful error if no parameters are passed", {
  path <- test_prepare_orderly_example("minimal")
  expect_error(
    orderly_batch("example", NULL),
    "Parameters for a batch must be a data frame with at least one row")
})

test_that("failure report in batch run does not fail subsequent report runs", {
  path <- test_prepare_orderly_example("batch", testing = TRUE)

  params <- data_frame(
    a = c("one", "two", "three"),
    b = c(1, "2", 3)
  )
  batch_id <- ids::random_id()
  mockery::stub(orderly_batch, "ids::random_id", batch_id)
  output <- orderly_batch("example", parameters = params,
                       root = path, echo = FALSE)
  expect_setequal(colnames(output), c("id", "success", "a", "b"))
  expect_equal(output[, c("a", "b")], params)
  expect_equal(output$success, c(TRUE, FALSE, TRUE))
  ## test something about success and batch runs too
  success_ids <- output[output$success, ]
  data <- lapply(success_ids, function(id) {
    readRDS(path_orderly_run_rds(file.path(path, "draft", "example", id)))
  })
  invisible(lapply(data, function(d) {
    expect_equal(d$meta$batch_id, batch_id)
  }))
})
