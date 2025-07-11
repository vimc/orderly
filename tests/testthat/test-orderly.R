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
  path <- test_prepare_orderly_example("depends", testing = TRUE)
  id1 <- orderly_run("example", root = path, echo = FALSE)
  orderly_commit(id1, root = path)

  id2 <- orderly_run("depend", root = path, echo = FALSE)

  d <- readRDS(file.path(path, "draft", "depend", id2, "output.rds"))
  expect_equal(d$depends$id, id1)
})


test_that("orderly_run_info errors when not running", {
  expect_error(orderly_run_info(),
               "Not currently running an orderly report")
})


test_that("orderly_run_info is usable from test_start", {
  path <- test_prepare_orderly_example("depends", testing = TRUE)
  id1 <- orderly_run("example", root = path, echo = FALSE)
  p <- orderly_test_start("depend", root = path, use_draft = TRUE)

  expect_error(
    orderly_run_info(),
    "Not currently running an orderly report")
  info <- orderly_run_info(p)
  expect_equal(info$depends$id, id1)
  expect_is(info$depends$time, "POSIXt")
  expect_true(info$depends$is_latest)
})


test_that("orderly_run_info: is_latest detects latest version", {
  skip_on_cran_windows()
  path <- test_prepare_orderly_example("depends", testing = TRUE)
  id1 <- orderly_run("example", root = path, echo = FALSE)
  id2 <- orderly_run("example", root = path, echo = FALSE)

  f <- function() {
    p <- orderly_test_start("depend", root = path, use_draft = TRUE)
    orderly_run_info(p)
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
  # if logging is on this test will wait for user input, so turn off
  orderly_log_off()
  on.exit(orderly_log_on())
  path <- test_prepare_orderly_example("minimal")
  p <- file.path(path, "src", "example", "orderly.yml")
  txt <- c(readLines(p), "packages: nonexistantpackage")
  writeLines(txt, p)
  wd <- getwd()
  expect_error(orderly_test_start("example", root = path),
               "nonexistantpackage")
  expect_equal(getwd(), wd)
})


test_that("can't depend on non artefacts", {
  path <- test_prepare_orderly_example("depends", testing = TRUE)
  id <- orderly_run("example", root = path, echo = FALSE)

  path_yml <- file.path(path, "src", "depend", "orderly.yml")
  d <- yaml_read(path_yml)
  d$depends$example$use <- c(d$depends$example$use,
                             list("other.R" = "script.R"))
  yaml_write(d, path_yml)

  expect_error(
    orderly_run("depend", root = path, echo = FALSE, use_draft = TRUE),
    "Dependency file not an artefact of example/.*:\n- 'script.R'")
})


test_that("dependency dir can be used", {
  skip_on_cran_windows()
  path <- test_prepare_orderly_example("demo")
  id <- orderly_run("use_resource_dir", root = path, echo = FALSE)
  p <- orderly_commit(id, root = path)
  con <- orderly_db("destination", path)
  on.exit(DBI::dbDisconnect(con))

  tmp <- DBI::dbGetQuery(
    con,
    "SELECT * FROM file_input JOIN file ON file_hash = hash")
  expect_setequal(
    tmp$filename,
    c("meta/another.csv", "meta/data.csv", "script.R", "orderly.yml"))
})


test_that("commit out of order", {
  skip_on_cran_windows()
  path <- test_prepare_orderly_example("minimal")

  id1 <- orderly_run("example", root = path, echo = FALSE)
  id2 <- orderly_run("example", root = path, echo = FALSE)

  orderly_commit(id2, root = path)
  expect_error(orderly_commit(id1, root = path), NA)

  expect_equal(orderly_latest("example", root = path), id2)

  id3 <- orderly_run("example", root = path, echo = FALSE)
  expect_error(orderly_commit(id3, root = path), NA)
  expect_equal(orderly_latest("example", root = path), id3)
})


test_that("archive directory is created when needed", {
  path <- test_prepare_orderly_example("minimal")
  unlink(file.path(path, "archive"), recursive = TRUE)
  orderly_run("example", root = path, echo = FALSE)
  expect_true(file.exists(path_orderly_archive_version(path)))
})


test_that("onload can be rerun", {
  v <- cache$current_archive_version
  cache$current_archive_version <- NULL
  orderly1:::.onLoad()
  expect_equal(cache$current_archive_version, v)
})


test_that("default parameter values are used", {
  path <- test_prepare_orderly_example("parameters", testing = TRUE)

  id <- orderly_run("example", list(a = 1, b = 2), root = path, echo = FALSE)
  d <- readRDS(path_orderly_run_rds(file.path(path, "draft", "example", id)))
  expect_equal(d$meta$parameters, list(a = 1, b = 2, c = 1))

  id <- orderly_run("example", list(a = 1, b = 2, c = 3),
                    root = path, echo = FALSE)
  d <- readRDS(path_orderly_run_rds(file.path(path, "draft", "example", id)))
  expect_equal(d$meta$parameters, list(a = 1, b = 2, c = 3))
})


test_that("store random seed", {
  skip_on_cran_windows()
  path <- test_prepare_orderly_example("minimal")
  set.seed(1)
  rs <- .Random.seed
  id <- orderly_run("example", root = path, echo = FALSE)
  d <- readRDS(path_orderly_run_rds(file.path(path, "draft", "example", id)))
  expect_true(identical(d$meta$random_seed, rs))
})


test_that("run with dependencies from remote", {
  dat <- prepare_orderly_remote_example()

  id1 <- orderly_run("depend", root = dat$path_local, remote = "default",
                     echo = FALSE)
  p1 <- file.path(dat$path_local, "draft", "depend", id1)
  d1 <- readRDS(path_orderly_run_rds(p1))$meta
  expect_equal(d1$depends$id, dat$id2)
  expect_true(d1$depends$is_latest)

  id_example3 <- orderly_run("example", root = dat$path_local, echo = FALSE)
  orderly_commit(id_example3, root = dat$path_local)

  id2 <- orderly_run("depend", root = dat$path_local, remote = "default",
                     echo = FALSE)
  p2 <- file.path(dat$path_local, "draft", "depend", id2)
  d2 <- readRDS(path_orderly_run_rds(p2))$meta
  expect_equal(d2$depends$id, dat$id2)
  expect_true(d2$depends$is_latest)

  id3 <- orderly_run("depend", root = dat$path_local, echo = FALSE)
  p3 <- file.path(dat$path_local, "draft", "depend", id3)
  d3 <- readRDS(path_orderly_run_rds(p3))$meta
  expect_equal(d3$depends$id, id_example3)
  expect_true(d3$depends$is_latest)
})
