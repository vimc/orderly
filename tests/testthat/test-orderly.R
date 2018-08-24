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


test_that("orderly_run_info is usable from test_start", {
  path <- prepare_orderly_example("depends")
  id1 <- orderly_run("example", config = path, echo = FALSE)
  id2 <- orderly_test_start("depend", config = path)
  on.exit(orderly_test_end())
  info <- orderly_run_info()
  expect_equal(info$depends$id, id1)
  expect_is(info$depends$time, "POSIXt")
  expect_true(info$depends$is_latest)
  orderly_test_end()
  on.exit()
  expect_error(orderly_run_info(),
               "Not currently running an orderly report")
})


test_that("orderly_run_info: is_latest detects latest version", {
  path <- prepare_orderly_example("depends")
  id1 <- orderly_run("example", config = path, echo = FALSE)
  id2 <- orderly_run("example", config = path, echo = FALSE)

  f <- function() {
    orderly_test_start("depend", config = path)
    on.exit(orderly_test_end())
    info <- orderly_run_info()
    info
  }

  p <- file.path(path, "src", "depend", "orderly.yml")
  txt1 <- sub("latest$", id1, readLines(p))
  txt2 <- sub("latest$", id2, readLines(p))

  info0 <- f()
  writeLines(txt1, p)
  info1 <- f()
  writeLines(txt2, p)
  info2 <- f()

  expect_true(info0$depends$is_latest)
  expect_false(info1$depends$is_latest)
  expect_true(info2$depends$is_latest)
})


test_that("orderly_test_start failure resets working directory", {
  path <- prepare_orderly_example("minimal")
  p <- file.path(path, "src", "example", "orderly.yml")
  txt <- c(readLines(p), "packages: nonexistantpackage")
  writeLines(txt, p)
  wd <- getwd()
  expect_error(orderly_test_start("example", config = path),
               "nonexistantpackage")
  expect_equal(getwd(), wd)
})


test_that("can't depend on non artefacts", {
  path <- prepare_orderly_example("depends")
  id <- orderly_run("example", config = path, echo = FALSE)

  path_yml <- file.path(path, "src", "depend", "orderly.yml")
  d <- yaml_read(path_yml)
  d$depends$example$use <- c(d$depends$example$use,
                             list("other.R" = "script.R"))
  yaml_write(d, path_yml)

  expect_error(
    orderly_run("depend", config = path, echo = FALSE),
    "Dependency file not an artefact of example/.*:\n- 'script.R'")
})


test_that("dependency dir can be used", {
  path <- prepare_orderly_example("demo")
  id <- orderly_run("use_resource_dir", config = path)
  options(error = recover)
  p <- orderly_commit(id, config = path)
  con <- orderly_db("destination", path)
  on.exit(DBI::dbDisconnect(con))

  tmp <- DBI::dbGetQuery(
    con,
    "SELECT * FROM file_input JOIN file ON file_hash = hash")
  expect_setequal(
    tmp$filename,
    c("meta/another.csv", "meta/data.csv", "script.R", "orderly.yml"))
})
