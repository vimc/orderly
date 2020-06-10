context("bundles")

test_that("pack bundle", {
  path <- prepare_orderly_example("minimal")
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
  orderly_bundle_import(zip$path, root = path2)

  expect_equal(orderly_list_archive(path2),
               data_frame(name = "example", id = res$id))

  ## Check that the data gets properly dealt with through this
  ## process:
  con <- orderly_db("destination", root = path2)
  on.exit(DBI::dbDisconnect(con))

  db_rds <- orderly_db("rds", root = path2)
  db_rds$list()

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
  path <- prepare_orderly_example("minimal")
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

  l2 <- orderly_bundle_list(path_bundles)
  l1$status <- "complete"
  expect_equal(l1, l2)
})


test_that("pack a bundle that requires parameters", {
  path_src <- prepare_orderly_example("demo")
  path_bundles <- tempfile()
  path_workdir <- tempfile()

  res <- orderly_bundle_pack(path_bundles, "other", parameters = list(nmin = 0.5),
                           root = path_src)
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
  path <- prepare_orderly_example("demo")
  on.exit(unlink(path, recursive = TRUE))

  path_bundles <- tempfile()
  path_work <- tempfile()

  ## If we export a number at once, we should avoid collisions, but
  ## this will be slow enough on almost any computer to avoid that:
  res1 <- orderly_bundle_pack(path_bundles, "other", parameters = list(nmin = 0),
                            root = path)
  res2 <- orderly_bundle_pack(path_bundles, "other", parameters = list(nmin = 0.5),
                            root = path)

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
  path <- prepare_orderly_example("minimal")
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
  path <- prepare_orderly_example("minimal")
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
  path <- prepare_orderly_example("minimal")
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
  tmp <- tempfile()
  file.create(tmp)
  expect_error(
    orderly_bundle_import(tmp, root = path),
    "Failed to extract bundle info from '.*'")
})
