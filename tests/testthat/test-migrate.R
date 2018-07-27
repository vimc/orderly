context("migrations")


test_that("0.3.2 -> 0.3.3", {
  path <- unpack_reference("0.3.2")
  cmp <- unpack_reference("0.4.8")
  orderly_migrate(path, to = "0.3.3")

  d <- orderly_list_archive(path)
  p <- file.path(d$name, d$id)

  i <- which(d$name == "depend")[[1]]
  m1 <- readRDS(file.path(path_archive(path), path_orderly_run_rds(p[[i]])))
  m2 <- readRDS(file.path(path_archive(cmp),  path_orderly_run_rds(p[[i]])))
  expect_equal(m1, m2)

  pp <- file.path(path_archive(path), p[[i]])
  expect_true(file.exists(path_orderly_run_rds_backup(pp, "0.3.3")))
  expect_equal(readLines(path_orderly_version(path)), "0.3.3")
})


test_that("roll back migration", {
  path <- unpack_reference("0.3.2")
  hash <- hash_files(list.files(path, recursive = TRUE, full.names = TRUE))
  orderly_migrate(path, to = "0.3.3")
  migrate_rollback(path, "0.3.3", "0.0.0")

  expect_equal(readLines(path_orderly_version(path)), "0.0.0")
  unlink(path_orderly_version(path))
  expect_equal(
    hash_files(list.files(path, recursive = TRUE, full.names = TRUE)),
    hash)
})


test_that("failed migrations are rolled back", {
  path <- unpack_reference("0.3.2")
  hash <- hash_files(list.files(path, recursive = TRUE, full.names = TRUE))

  counter <- 0L
  fun <- function(data, path) {
    counter <<- counter + 1L
    if (counter >= 3L) {
      stop("some sort of migration failure")
    }
    ## any old bit to indicate a change:
    data$updated <- TRUE
    list(changed = TRUE, data = data)
  }

  expect_error(migrate_apply(path, "0.3.3", fun),
               "some sort of migration failure")

  cmp <- hash_files(list.files(path, recursive = TRUE, full.names = TRUE))
  expect_equal(cmp[basename(names(cmp)) != "orderly_version"], hash)
})
