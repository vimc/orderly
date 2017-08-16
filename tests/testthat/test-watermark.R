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

test_that("image", {
  path <- tempfile(fileext = ".png")
  png(path)
  plot(1:10)
  dev.off()

  img <- magick::image_read(path)
  expect_false(watermark_exists(path))
  expect_error(watermark_read(path),
               "Did not find orderly watermark")

  id <- new_report_id()
  watermark_write(path, id)
  expect_identical(watermark_read(path), id)
  cmp <- magick::image_read(path)
  expect_identical(as.raster(img), as.raster(cmp))
})
