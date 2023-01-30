context("bundles")

test_that("pack bundle", {
  path <- test_prepare_orderly_example("minimal")
  on.exit(unlink(path, recursive = TRUE))

  path_bundles <- tempfile()

  res <- orderly_bundle_pack(path_bundles, "example", root = path)
  expect_equal(dir(path_bundles), basename(res$path))
  expect_equal(basename(res$path), paste0(res$id, ".zip"))

  ## Move the orderly root to prevent any file references being valid:
  path2 <- paste0(path, "-moved")
  file.rename(path, path2)

  workdir <- tempfile()
  zip <- orderly_bundle_run(res$path, workdir, echo = FALSE)
  expect_equal(dir(workdir), basename(zip$path))
  expect_equal(dir(workdir), zip$filename)
  orderly_bundle_import(zip$path, root = path2)

  expect_equal(orderly_list_archive(path2),
               data_frame(name = "example", id = res$id))

  ## Check that the data gets properly dealt with through this
  ## process:
  con <- orderly_db("destination", root = path2)
  on.exit(DBI::dbDisconnect(con))

  db_rds <- orderly_db("rds", root = path2)

  rvd <- DBI::dbReadTable(con, "report_version_data")
  expect_equal(nrow(rvd), 1)
  expect_equal(rvd$report_version, res$id)
  expect_equal(rvd$database, "source")
  expect_equal(rvd$query, "SELECT name, number FROM thing")
  expect_equal(rvd$hash, db_rds$list())
  d <- db_rds$get(rvd$hash)
  expect_is(d, "data.frame")
  expect_equal(names(d), c("name", "number"))
})


test_that("can run a bundle in place if wanted", {
  path <- test_prepare_orderly_example("minimal")
  on.exit(unlink(path, recursive = TRUE))

  path_bundles <- tempfile()

  res <- orderly_bundle_pack(path_bundles, "example", root = path)
  expect_equal(dir(path_bundles), basename(res$path))
  expect_equal(basename(res$path), paste0(res$id, ".zip"))

  l1 <- orderly_bundle_list(path_bundles)
  expect_equal(l1$id, res$id)
  expect_equal(l1$status, "incomplete")
  expect_equal(l1$name, "example")
  expect_equal(l1$parameters, I(list(NULL)))

  zip <- orderly_bundle_run(res$path, path_bundles, echo = FALSE)
  expect_true(same_path(zip$path, res$path))
  expect_equal(zip$filename, basename(zip$path))

  l2 <- orderly_bundle_list(path_bundles)
  l1$status <- "complete"
  expect_equal(l1, l2)
})


test_that("pack a bundle that requires parameters", {
  path_src <- test_prepare_orderly_example("demo")
  path_bundles <- tempfile()
  path_workdir <- tempfile()

  res <- orderly_bundle_pack(
    path_bundles, "other", parameters = list(nmin = 0.5), root = path_src)
  info <- orderly_bundle_info(res$path)
  expect_equal(info$parameters, list(nmin = 0.5))
  expect_true(all(info$data$data$extract$number >= 0.5))

  zip <- orderly_bundle_run(res$path, path_workdir, echo = FALSE)
  orderly_bundle_import(zip$path, root = path_src)

  dat <- readRDS(path_orderly_run_rds(
    file.path(path_src, "archive", "other", res$id)))
  expect_match(dat$meta$data$query, "number > 0.5", fixed = TRUE)
  expect_equal(dat$meta$parameters, list(nmin = 0.5))
})


test_that("list a directory of bundles", {
  path <- test_prepare_orderly_example("demo")
  on.exit(unlink(path, recursive = TRUE))

  path_bundles <- tempfile()
  path_work <- tempfile()

  ## If we export a number at once, we should avoid collisions, but
  ## this will be slow enough on almost any computer to avoid that:
  res1 <- orderly_bundle_pack(
    path_bundles, "other", parameters = list(nmin = 0), root = path)
  res2 <- orderly_bundle_pack(
    path_bundles, "other", parameters = list(nmin = 0.5), root = path)

  info1 <- orderly_bundle_list(path_bundles)
  expect_equal(info1$id, c(res1$id, res2$id))
  expect_equal(info1$status, rep("incomplete", 2))
  expect_equal(info1$name, rep("other", 2))
  expect_equal(info1$parameters, I(list(list(nmin = 0), list(nmin = 0.5))))
  expect_is(info1$time, "POSIXt")

  ## This fails for reasons....
  zip1 <- orderly_bundle_run(res1$path, path_bundles, echo = FALSE)
  info2 <- orderly_bundle_list(path_bundles)
  info1$status[[1]] <- "complete"
  expect_equal(info2, info1)
})


test_that("can't run a bundle twice", {
  path <- test_prepare_orderly_example("minimal")
  on.exit(unlink(path, recursive = TRUE))

  path_bundles <- tempfile()

  res <- orderly_bundle_pack(path_bundles, "example", root = path)
  expect_equal(dir(path_bundles), basename(res$path))
  expect_equal(basename(res$path), paste0(res$id, ".zip"))

  zip <- orderly_bundle_run(res$path, path_bundles, echo = FALSE)
  expect_error(orderly_bundle_run(res$path, path_bundles, echo = FALSE),
               sprintf("Bundle '%s' has already been run", res$id))
})


test_that("Can't import a bundle twice", {
  path <- test_prepare_orderly_example("minimal")
  on.exit(unlink(path, recursive = TRUE))

  path_bundles <- tempfile()

  res <- orderly_bundle_pack(path_bundles, "example", root = path)
  expect_equal(dir(path_bundles), basename(res$path))
  expect_equal(basename(res$path), paste0(res$id, ".zip"))

  zip <- orderly_bundle_run(res$path, path_bundles, echo = FALSE)
  orderly_bundle_import(zip$path, root = path)
  expect_error(
    orderly_bundle_import(zip$path, root = path),
    sprintf("example:%s already exists", res$id))
})


test_that("Can't extract a bundle onto itself", {
  path <- test_prepare_orderly_example("minimal")
  on.exit(unlink(path, recursive = TRUE))
  path_bundles <- tempfile()
  res <- orderly_bundle_pack(path_bundles, "example", root = path)
  dir.create(file.path(path_bundles, res$id))
  expect_error(
    orderly_bundle_run(res$path, path_bundles, echo = FALSE),
    sprintf("Can't unpack bundle '%s' here; it has already been extracted",
            res$id))
})


test_that("can run a bundle with dependencies", {
  path <- orderly::orderly_example("demo")
  orderly_run_internal("other", parameters = list(nmin = 0),
                       root = path, commit = TRUE, echo = FALSE)

  path_bundles <- tempfile()
  path_work <- tempfile()

  res1 <- orderly_bundle_pack(path_bundles, "use_dependency", root = path)
  expect_true(
    file.path(res1$id, "pack/incoming.csv") %in%
    zip::zip_list(res1$path)$filename)

  res2 <- orderly_bundle_run(res1$path, path_work, echo = FALSE)

  ## We can run this and import into the db:
  orderly_bundle_import(res2$path, root = path)

  con <- orderly_db("destination", root = path)
  on.exit(DBI::dbDisconnect(con))
  depends <- DBI::dbReadTable(con, "depends")
  expect_equal(depends$report_version, res2$id)
  expect_equal(depends$as, "incoming.csv")
})


test_that("can run a bundle with global file dependencies", {
  path <- orderly::orderly_example("demo")
  path_bundles <- tempfile()
  path_work <- tempfile()

  res1 <- orderly_bundle_pack(path_bundles, "global", root = path)
  expect_true(
    file.path(res1$id, "pack/data.csv") %in% zip::zip_list(res1$path)$filename)

  res2 <- orderly_bundle_run(res1$path, path_work, echo = FALSE)
  orderly_bundle_import(res2$path, root = path)

  con <- orderly_db("destination", root = path)
  on.exit(DBI::dbDisconnect(con))
  fig <- DBI::dbReadTable(con, "file_input_global")
  expect_equal(fig$filename, "data.csv")
})


test_that("can't cope with connections", {
  path <- orderly::orderly_example("demo")
  path_bundles <- tempfile()
  expect_error(
    orderly_bundle_pack(path_bundles, "connection", root = path),
    "Cannot use 'connection:' with a bundle")
})


test_that("can't import an unrun bundle", {
  path <- orderly::orderly_example("minimal")
  path_bundles <- tempfile()
  res <- orderly_bundle_pack(path_bundles, "example", root = path)
  expect_error(
    orderly_bundle_import(res$path, root = path),
    "This does not look like a complete bundle (one that has been run)",
    fixed = TRUE)
})


test_that("sensible error when given junk input", {
  path <- orderly::orderly_example("minimal")
  tmp <- tempfile(fileext = ".zip")
  zip_dir(file.path(path, "src"), tmp)
  expect_error(
    orderly_bundle_import(tmp, root = path),
    "Failed to extract bundle info from '.*'")
})


test_that("can run a bundle from a relative path", {
  path <- test_prepare_orderly_example("minimal")
  on.exit(unlink(path, recursive = TRUE))

  writeLines(
    "readme",
    file.path(path, "src", "example", "README.md"))

  path_bundles <- tempfile()
  res <- orderly_bundle_pack(path_bundles, "example", root = path)

  workdir <- tempfile()
  withr::with_dir(dirname(workdir),
                  orderly_bundle_run(res$path, basename(workdir),
                                     echo = FALSE))
  ## Just ensure that we run without error
  expect_equal(length(dir(workdir)), 1)
})


test_that("Can rename a bundle before import", {
  path <- test_prepare_orderly_example("minimal")
  on.exit(unlink(path, recursive = TRUE))

  path_bundles <- tempfile()
  res <- orderly_bundle_pack(path_bundles, "example", root = path)
  ans <- orderly_bundle_run(res$path, echo = FALSE)
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  file_copy(ans$path, tmp)
  expect_true(orderly_bundle_import(tmp, root = path))
  expect_equal(orderly_list_archive(path),
               data_frame(name = "example", id = res$id))
})

test_that("failed bundle run writes out failed rds", {
  path <- test_prepare_orderly_example("minimal")
  on.exit(unlink(path, recursive = TRUE))

  append_lines(
    c("f <- function() g()",
      "g <- function() h()",
      "h <- function() stop('some error')",
      "f()"),
    file.path(path, "src", "example", "script.R"))

  path_bundles <- tempfile()

  res <- orderly_bundle_pack(path_bundles, "example", root = path)
  expect_equal(dir(path_bundles), basename(res$path))
  expect_equal(basename(res$path), paste0(res$id, ".zip"))

  ## Move the orderly root to prevent any file references being valid:
  path2 <- paste0(path, "-moved")
  file.rename(path, path2)

  workdir <- tempfile()
  expect_error(orderly_bundle_run(res$path, workdir, echo = FALSE),
               "some error")

  ## Failed rds has been saved into pack
  id <- list.files(workdir)
  path_rds <- path_orderly_fail_rds(file.path(workdir, id, "pack"))
  expect_true(file.exists(path_rds))

  failed_rds <- readRDS(path_rds)
  expect_equal(names(failed_rds),
               c("session_info", "time", "env", "error", "meta",
                 "archive_version"))
  expect_equal(failed_rds$error$error$message, "some error")
  expect_true(length(failed_rds$error$trace) > 5)
  expect_match(failed_rds$error$trace[length(failed_rds$error$trace) - 3],
               "f()")
  expect_match(failed_rds$error$trace[length(failed_rds$error$trace) - 2],
               "g()")
  expect_match(failed_rds$error$trace[length(failed_rds$error$trace) - 1],
               "h()")
  expect_match(failed_rds$error$trace[length(failed_rds$error$trace)],
               'stop\\("some error"\\)')
})

test_that("Failure output written if a bundle fails", {
  path <- test_prepare_orderly_example("demo")
  on.exit(unlink(path, recursive = TRUE))

  # Make parameter less than zero cause a crash

  test_script <- file.path(path, "src", "other", "script.R")
  script <- readLines(test_script)
  script <- c("if (nmin < 0) stop('Invalid parameter')", script)
  writeLines(script, test_script)

  # Prepare to run two jobs, one fails, one succeeds

  path_bundles <- file.path(path, "bundles")
  bundle <- orderly::orderly_bundle_pack(path_bundles, "other",
                                          parameters = list(nmin = -1),
                                         root = path)


  bundle_path <- file.path(path, "bundles", basename(bundle$path))

  tryCatch({
    orderly::orderly_bundle_run(bundle_path, "output")
  }, error = function(e) {
    invisible()
  })

  expect_true(file.exists(file.path(getwd(), "output", bundle$id, "pack",
                                    "orderly_fail.rds")))
})

test_that("zip list helper safely lists", {
  skip_on_cran()
  path <- test_prepare_orderly_example("minimal")
  on.exit(unlink(path, recursive = TRUE))
  path_bundles <- tempfile()
  res <- orderly_bundle_pack(path_bundles, "example", root = path)
  l1 <- zip::zip_list(res$path)$filename
  l2 <- zip_list2(res$path)$filename
  l3 <- zip_list_base(res$path)$filename

  expect_equal(l1, l2)
  expect_setequal(l3, l1)
})


test_that("fall back error handling", {
  skip_on_cran()
  skip_if_not_installed("mockery")

  d <- data.frame(filename = "x")
  mock_zip_zip_list <- mockery::mock(
    d,
    stop("some zip error"),
    stop("some zip error"))
  mock_base_zip_list <- mockery::mock(
    d,
    stop("some base error"))

  mockery::stub(zip_list2, "zip::zip_list", mock_zip_zip_list)
  mockery::stub(zip_list2, "zip_list_base", mock_base_zip_list)

  expect_equal(zip_list2("path"), d)
  mockery::expect_called(mock_zip_zip_list, 1)
  mockery::expect_called(mock_base_zip_list, 0)

  expect_equal(zip_list2("path"), d)
  mockery::expect_called(mock_zip_zip_list, 2)
  mockery::expect_called(mock_base_zip_list, 1)

  expect_error(zip_list2("path"), "some zip error")
  mockery::expect_called(mock_zip_zip_list, 3)
  mockery::expect_called(mock_base_zip_list, 2)

  expect_equal(mockery::mock_args(mock_zip_zip_list),
               rep(list(list("path")), 3))
  expect_equal(mockery::mock_args(mock_base_zip_list),
               rep(list(list("path")), 2))
})
