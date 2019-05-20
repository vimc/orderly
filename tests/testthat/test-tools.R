test_that("unpack archive", {
  path <- prepare_orderly_example("minimal")
  id <- orderly_run("example", path = path, echo = FALSE)
  p <- orderly_commit(id, path = path)

  zip <- zip_dir(p)

  root <- tempfile()
  unzip_archive(zip, root, "example", id)
  res <- file.path(root, "archive", "example")
  expect_equal(dir(res), id)
  expect_equal(sort(dir(file.path(res, id), recursive = TRUE)),
               sort(dir(p, recursive = TRUE)))
})


test_that("unpack report failure: corrupt download", {
  skip_on_cran()
  bytes <- as.raw(c(0x50, 0x4b, 0x05, 0x06, rep(0x00, 18L)))
  zip <- tempfile()
  writeBin(bytes, zip)
  ## This test might be platform dependent as a sane unzip function
  ## would have caught this.
  expect_error(suppressWarnings(unzip_archive(zip, tempfile(), NULL, NULL)),
               "Corrupt zip file? No files extracted",
               fixed = TRUE)
})


test_that("unpack failure: not an orderly archive", {
  tmp <- file.path(tempfile(), "parent")
  dir.create(tmp, FALSE, TRUE)
  file.create(file.path(tmp, c("a", "b")))
  zip <- tempfile(fileext = ".zip")
  withr::with_dir(tmp, zip(zip, dir(), extras = "-q"))
  expect_error(unzip_archive(zip, tempfile(), NULL, NULL),
               "Invalid orderly archive")
})


test_that("unpack failure: not expected id", {
  id <- new_report_id()
  tmp <- file.path(tempfile(), id)
  dir.create(tmp, FALSE, TRUE)
  dir.create(file.path(tmp, "orderly.yml"))
  zip <- zip_dir(tmp)
  expect_error(unzip_archive(zip, tempfile(), NULL, "other"),
               sprintf("This is archive '%s' but expected 'other'", id),
               fixed = TRUE)
})


test_that("unpack failure: missing files", {
  id <- new_report_id()
  tmp <- file.path(tempfile(), id)
  dir.create(tmp, FALSE, TRUE)
  dir.create(file.path(tmp, "orderly.yml"))
  zip <- zip_dir(tmp)
  expect_error(unzip_archive(zip, tempfile(), NULL, id),
               "Invalid orderly archive: missing files orderly_run.rds",
               fixed = TRUE)
})
