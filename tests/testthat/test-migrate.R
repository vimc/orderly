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
  expect_equal(readLines(path_orderly_archive_version(path)), "0.3.3")
})


test_that("roll back migration", {
  path <- unpack_reference("0.3.2")
  hash <- hash_files(list.files(path, recursive = TRUE, full.names = TRUE))
  orderly_migrate(path, to = "0.3.3")
  migrate_rollback(path, "0.3.3", "0.0.0")

  expect_equal(readLines(path_orderly_archive_version(path)), "0.0.0")
  unlink(path_orderly_archive_version(path))
  expect_equal(
    hash_files(list.files(path, recursive = TRUE, full.names = TRUE)),
    hash)
})


test_that("failed migrations are rolled back", {
  path <- unpack_reference("0.3.2")
  hash <- hash_files(list.files(path, recursive = TRUE, full.names = TRUE))

  counter <- 0L
  fun <- function(data, path, config) {
    counter <<- counter + 1L
    if (counter >= 3L) {
      stop("some sort of migration failure")
    }
    ## any old bit to indicate a change:
    data$updated <- TRUE
    list(changed = TRUE, data = data)
  }

  config <- orderly_config(path)
  expect_error(migrate_apply(path, "0.3.3", fun, config, FALSE, FALSE),
               "some sort of migration failure")

  cmp <- hash_files(list.files(path, recursive = TRUE, full.names = TRUE))
  expect_equal(cmp[basename(names(cmp)) != "orderly_archive_version"], hash)
})


test_that("dry run", {
  path <- unpack_reference("0.3.2")
  hash <- hash_files(list.files(path, recursive = TRUE, full.names = TRUE))
  orderly_migrate(path, to = "0.3.3", dry_run = TRUE)
  expect_equal(
    hash_files(list.files(path, recursive = TRUE, full.names = TRUE)),
    hash)
})


test_that("migrate_plan default is used", {
  path <- unpack_reference("0.3.2")
  expect_equal(migrate_plan(path), available_migrations())
  expect_equal(migrate_plan(path, to = "0.0.1"),
               set_names(character(), character()))
})


test_that("mixed migration", {
  path <- unpack_reference("0.3.2")

  curr <- as.character(cache$current_archive_version)

  ## Need to work around an intentional assertion
  writeLines(curr, path_orderly_archive_version(path))
  id <- orderly_run("example", config = path, echo = FALSE)
  orderly_commit(id, config = path)

  unlink(path_orderly_archive_version(path))

  msg <- capture_messages(
    orderly_migrate(path, to = curr, verbose = TRUE, dry_run = TRUE))
  expect_true(
    any(grepl(sprintf("[ ok         ]  example/%s", id), msg, fixed = TRUE)))
})


test_that("require migration", {
  path <- unpack_reference("0.3.2")
  expect_error(orderly_run("example", config = path, echo = FALSE),
               "orderly archive needs migrating from 0.0.0 =>", fixed = TRUE)
})


test_that("can't commit old version", {
  path <- unpack_reference("0.3.2")
  contents <- orderly_list_archive(path)

  id <- contents$id[contents$name == "depend"][[1L]]
  file.rename(file.path(path, "archive", "depend", id),
              file.path(path, "draft", "depend", id))
  orderly_migrate(path)
  orderly_rebuild(path)
  expect_error(
    orderly_commit(id, config = path),
    "This report was built with an old version of orderly; please rebuild",
    fixed = TRUE)
})


test_that("don't migrate new orderly", {
  path <- prepare_orderly_example("minimal")
  p <- path_orderly_archive_version(path)
  unlink(p)
  check_orderly_archive_version(orderly_config(path))
  expect_true(file.exists(p))
  expect_equal(read_orderly_archive_version(path),
               as.character(cache$current_archive_version))
})
