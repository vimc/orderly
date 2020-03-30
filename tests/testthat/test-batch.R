context("batch")

test_that("reports can be batch run", {
  path <- prepare_orderly_example("parameters", testing = TRUE)
  
  params <- data_frame(
    a = c("one", "two", "three"),
    b = c(1, 2, 3)
  )
  batch_id <- ids::random_id()
  mockery::stub(orderly_batch, "ids::random_id", batch_id)
  ids <- orderly_batch("example", parameters = params, 
                       root = path, echo = FALSE)
  data <- lapply(ids, function(id) {
    readRDS(path_orderly_run_rds(file.path(path, "draft", "example", id)))
  })
  invisible(lapply(data, function(d) {
    expect_equal(d$meta$batch_id, batch_id)
  }))
})

test_that("return useful error if params passed without names", {
  path <- prepare_orderly_example("parameters", testing = TRUE)
  
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