context("watermark")

test_that("csv", {
  path <- tempfile(fileext = ".csv")
  write.csv(mtcars, path)
  d <- read.csv(path)
  expect_false(watermark_exists(path))
  expect_error(watermark_read(path),
               "Did not find orderly watermark")

  id <- new_report_id()
  watermark_write(path, id)
  expect_identical(watermark_read(path), id)
  expect_identical(read.csv(path, skip = 1), d)
})

test_that("rds", {
  for (compress in c(TRUE, FALSE)) {
    path <- tempfile(fileext = ".rds")
    saveRDS(mtcars, path, compress = compress)
    d <- readRDS(path)
    expect_false(watermark_exists(path))
    expect_error(watermark_read(path),
                 "Did not find orderly watermark")

    id <- new_report_id()
    watermark_write(path, id)
    expect_identical(watermark_read(path), id)
    expect_identical(readRDS(path), d)
  }
})

test_that("png", {
  if (is.null(cache$exiftool)) {
    skip("no exiftool")
  }
  skip_if_not_installed("processx")
  path <- tempfile(fileext = ".png")
  png(path, width = 100, height = 100)
  par(mar = rep(0, 4))
  plot(1:10)
  dev.off()

  if (requireNamespace("png", quietly = TRUE)) {
    img <- png::readPNG(path)
  } else {
    img <- NULL
  }
  expect_false(watermark_exists(path))
  expect_error(watermark_read(path),
               "Did not find orderly watermark")

  id <- new_report_id()
  watermark_write(path, id)
  expect_identical(watermark_read(path), id)

  if (!is.null(img)) {
    expect_identical(png::readPNG(path), img)
  }
})

test_that("pdf", {
  if (is.null(cache$exiftool)) {
    skip("no exiftool")
  }
  skip_if_not_installed("processx")
  path <- tempfile(fileext = ".pdf")
  pdf(path, width = 100, height = 100)
  par(mar = rep(0, 4))
  plot(1:10)
  plot(runif(10))
  dev.off()

  expect_false(watermark_exists(path))
  expect_error(watermark_read(path),
               "Did not find orderly watermark")

  id <- new_report_id()
  watermark_write(path, id)
  expect_identical(watermark_read(path), id)
})
