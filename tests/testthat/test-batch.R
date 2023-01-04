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
  colnames(params) <- NULL
  expect_error(
    orderly_batch("example", parameters = params, root = path, echo = FALSE),
    "'parameters' must be named",
    fixed = TRUE
  )
})

test_that("return useful error if params are missing", {
  path <- test_prepare_orderly_example("parameters", testing = TRUE)

  params <- data_frame(
    param1 = c("one", "two", "three"),
    param2 = c(1, 2, 3)
  )
  expect_error(
    orderly_batch("example", parameters = params, root = path, echo = FALSE,
                  continue_on_error = FALSE),
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
    b = c(1, 2, 3)
  )
  batch_id <- ids::random_id()
  mockery::stub(orderly_batch, "ids::random_id", batch_id)
  output <- orderly_batch("example", parameters = params,
                       root = path, echo = FALSE)
  expect_setequal(colnames(output), c("id", "success", "a", "b"))
  expect_equal(output[, c("a", "b")], params)
  expect_equal(output$success, c(TRUE, FALSE, TRUE))

  success_ids <- output[output$success, "id"]
  data <- lapply(success_ids, function(id) {
    readRDS(path_orderly_run_rds(file.path(path, "draft", "example", id)))
  })
  invisible(lapply(data, function(d) {
    expect_equal(d$meta$batch_id, batch_id)
  }))

  failed_ids <- output[!output$success, "id"]
  data <- lapply(failed_ids, function(id) {
    readRDS(path_orderly_fail_rds(file.path(path, "draft", "example", id)))
  })
  invisible(lapply(data, function(d) {
    expect_equal(d$meta$batch_id, batch_id)
  }))
})

test_that("orderly_batch attempts subsequent report runs even when id unknown", {
  dat <- prepare_orderly_query_example()
  root <- dat$root

  config <- orderly_config_$new(root)

  p <- file.path(root, "src", "use_dependency", "orderly.yml")
  txt <- readLines(p)
  txt <- sub("latest", "latest(parameter:nmin == x)", txt, fixed = TRUE)
  txt <- c(txt, c("parameters:",
                  "  x:",
                  "    default: 0.25"))
  writeLines(txt, p)

  params <- data_frame(
    x = c(0.2, 0.9)
  )
  output <- orderly_batch("use_dependency", parameters = params,
                          root = root, echo = FALSE)
  expect_setequal(colnames(output), c("id", "success", "x"))
  expect_equal(output[, "x", drop = FALSE], params)
  expect_equal(output$success, c(TRUE, FALSE))
  expect_equal(output$id[2], NA_character_)
})

test_that("erroring report stops batch if continue_on_error is FLASE", {
  path <- test_prepare_orderly_example("batch", testing = TRUE)

  params <- data_frame(
    a = c("one", "two", "three"),
    b = c(1, 2, 3)
  )
  expect_error(orderly_batch("example", parameters = params,
                          continue_on_error = FALSE,
                          root = path, echo = FALSE),
               "b cannot be 2")
})
