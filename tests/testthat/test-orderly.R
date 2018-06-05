context("orderly")

test_that("init: existing file", {
  path <- tempfile()
  on.exit(unlink(path))
  writeLines(character(0), path)
  expect_error(orderly_init(path),
               "must be an empty directory")
})

test_that("init: non-empty directory", {
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))
  dir.create(file.path(path, "foo"), FALSE, TRUE)
  expect_error(orderly_init(path),
               "must be an empty directory")
})

test_that("init", {
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))

  expect_message(x <- orderly_init(path),
                 "Now, edit the file", fixed = TRUE)
  expect_identical(x, path)

  expect_true(file.exists(file.path(path, "orderly_config.yml")))
  expect_true(file.exists(file.path(path, "README.md")))
  expect_true(file.exists(file.path(path, "src", "README.md")))
  expect_true(file.exists(file.path(path, "data", "README.md")))
  expect_true(file.exists(file.path(path, "archive", "README.md")))
})

test_that("init - no doc", {
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))

  expect_message(x <- orderly_init(path, FALSE),
                 "Now, edit the file", fixed = TRUE)
  expect_identical(x, path)

  expect_true(file.exists(file.path(path, "orderly_config.yml")))
  expect_false(file.exists(file.path(path, "README.md")))
  expect_false(file.exists(file.path(path, "src", "README.md")))
  expect_false(file.exists(file.path(path, "data", "README.md")))
  expect_false(file.exists(file.path(path, "archive", "README.md")))
  expect_true(file.exists(file.path(path)))
  expect_true(file.exists(file.path(path, "src")))
  expect_true(file.exists(file.path(path, "data")))
  expect_true(file.exists(file.path(path, "archive")))
})


test_that("orderly_run_info reports on artefacts", {
  path <- prepare_orderly_example("depends")
  id1 <- orderly_run("example", config = path, echo = FALSE)
  id2 <- orderly_run("depend", config = path, echo = FALSE)

  d <- readRDS(file.path(path, "draft", "depend", id2, "output.rds"))
  expect_equal(d$depends$id, id1)
})


test_that("orderly_run_info errors when not running", {
  expect_error(orderly_run_info(),
               "Not currently running an orderly report")
})
