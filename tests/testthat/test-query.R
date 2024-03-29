context("query")

test_that("empty", {
  path <- tempfile()
  orderly_init(path, quiet = TRUE)
  file_copy(orderly_file("examples/minimal/orderly_config.yml"),
            file.path(path, "orderly_config.yml"),
            overwrite = TRUE)
  expect_equal(orderly_list(path), character(0))
  expect_equal(orderly_list_drafts(path),
               data.frame(name = character(0),
                          id = character(0),
                          stringsAsFactors = FALSE))
})

test_that("non-empty", {
  path <- test_prepare_orderly_example("minimal")
  expect_equal(orderly_list(path), c("example", "example2"))
})

test_that("query through lifecycle", {
  skip_on_cran_windows()
  path <- test_prepare_orderly_example("minimal")
  expect_equal(orderly_list(root = path), c("example", "example2"))

  empty <- data.frame(name = character(0), id = character(0),
                      stringsAsFactors = FALSE)
  expect_equal(orderly_list_drafts(path), empty)
  expect_equal(orderly_list_archive(path), empty)

  id <- orderly_run("example", root = path, echo = FALSE)

  expect_null(
    orderly_find_report(id, "example", draft = FALSE, config = path))
  p <- orderly_find_report(id, "example", draft = TRUE, config = path)
  expect_true(file.exists(p))
  expect_equal(basename(dirname(p)), "example")
  expect_equal(basename(dirname(dirname(p))), "draft")

  r <- orderly_list_drafts(path)
  expect_equal(r$name, "example")
  expect_equal(names(r), c("name", "id"))
  expect_equal(r$id, id)
  expect_equal(dir(file.path(path_draft(path), "example")), r$id)
  expect_equal(orderly_list_archive(path), empty)

  expect_equal(orderly_find_name(id, path, FALSE, TRUE, TRUE), "example")

  expect_error(orderly_find_name(id, path, FALSE, FALSE, TRUE),
               "Did not find archive report")
  expect_null(orderly_find_name(id, path, FALSE, FALSE, FALSE))

  expect_error(orderly_find_name("id", path, FALSE, TRUE, TRUE),
               "Did not find draft report")
  expect_null(orderly_find_name("id", path, FALSE, TRUE, FALSE))

  orderly_commit(id, root = path)

  expect_null(
    orderly_find_report(id, "example", draft = TRUE, config = path))
  p <- orderly_find_report(id, "example", draft = FALSE, config = path)
  expect_true(file.exists(p))
  expect_equal(basename(dirname(p)), "example")
  expect_equal(basename(dirname(dirname(p))), "archive")

  expect_equal(orderly_list_drafts(path), empty)
  expect_equal(orderly_list_archive(path), r)
})

test_that("latest_ids", {
  ## There is a 1/6554 chance of collision here in the test where
  ## generate 5 ids with the same leading component (down to the ms);
  ## one collision seen now on travis (see 1 / pbirthday(5, 256^2))
  skip_on_cran()
  skip_on_cran_windows()
  expect_equal(latest_id(character(0)), NA_character_)

  t <- Sys.time()
  t <- structure(as.numeric(t) %/% 1, class = class(t))

  id <- new_report_id(t)
  expect_identical(latest_id(id), id)
  expect_identical(latest_id(c(id, id)), id)

  ## Differ at the second level
  id_s <- vcapply(t - 5:0, new_report_id)
  expect_identical(latest_id(id_s), last(id_s))
  expect_identical(latest_id(sample(id_s)), last(id_s))

  ## Differ at the subsecond level
  id_ms <- vcapply(t + (1:9) / 10, new_report_id)
  expect_identical(latest_id(id_ms), last(id_ms))
  expect_identical(latest_id(sample(id_ms)), last(id_ms))

  ## Differ below the subsecond level
  id_same <- sort_c(replicate(5, new_report_id(t + 1)))
  expect_identical(latest_id(id_same), last(id_same))
  expect_identical(latest_id(sample(id_same)), last(id_same))

  id_both <- c(id_s, id_ms)
  expect_identical(latest_id(id_both), last(id_both))
  expect_identical(latest_id(sample(id_both)), last(id_both))

  id_all <- c(id_both, id_same)
  expect_identical(latest_id(id_all), last(id_same))
  expect_identical(latest_id(sample(id_all)), last(id_same))

  ## Unexpected inputs
  expect_error(latest_id(c(id, "other")),
               "Invalid report id: 'other'")
})

test_that("latest", {
  skip_on_cran_windows()
  path <- test_prepare_orderly_example("minimal")

  expect_equal(orderly_latest("example", root = path, must_work = FALSE),
               NA_character_)
  expect_equal(orderly_latest("example", root = path, must_work = FALSE,
                              draft = TRUE),
               NA_character_)
  expect_error(orderly_latest("example", root = path),
               "Did not find any archive reports for example")
  expect_error(orderly_latest("example", root = path, draft = TRUE),
               "Did not find any draft reports for example")

  expect_equal(orderly_latest(NULL, root = path, must_work = FALSE),
               NA_character_)

  id1 <- orderly_run("example", root = path, echo = FALSE)
  expect_equal(orderly_latest(NULL, root = path, draft = TRUE),
               id1)
  expect_equal(orderly_latest(NULL, root = path, draft = FALSE,
                              must_work = FALSE), NA_character_)
  Sys.sleep(0.1)
  id2 <- orderly_run("example", root = path, echo = FALSE)
  expect_equal(orderly_latest("example", root = path, draft = TRUE), id2)
  expect_equal(orderly_latest(NULL, root = path, draft = TRUE),
               id2)
  expect_equal(orderly_latest(NULL, root = path, draft = FALSE,
                              must_work = FALSE), NA_character_)

  orderly_commit(id2, root = path)
  expect_equal(orderly_latest("example", root = path, draft = TRUE), id1)
  expect_equal(orderly_latest("example", root = path), id2)
})

test_that("latest works with draft always, never, newer", {
  skip_on_cran_windows()
  path <- test_prepare_orderly_example("minimal")
  id1 <- orderly_run("example", root = path, echo = FALSE)
  id2 <- orderly_run("example", root = path, echo = FALSE)
  orderly_commit(id1, root = path)
  expect_equal(orderly_latest("example", root = path, draft = "never"), id1)
  expect_equal(orderly_latest("example", root = path, draft = "always"), id2)
  expect_equal(orderly_latest("example", root = path, draft = "newer"), id2)
})


test_that("Behaviour with rogue files", {
  testthat::skip_on_cran()
  path <- test_prepare_orderly_example("minimal")
  id1 <- orderly_run("example", root = path, echo = FALSE)
  id2 <- orderly_run("example", root = path, echo = FALSE)
  p1 <- orderly_commit(id1, root = path)
  p2 <- orderly_commit(id2, root = path)
  expect_equal(orderly_latest("example", root = path), id2)

  zip_dir(file.path(path, "archive", "example", id2))

  expect_error(
    orderly_latest("example", root = path),
    "Unexpected files within orderly directory '.*archive/example': '.*\\.zip'")
})


test_that("orderly_latest does not find failed drafts", {
  testthat::skip_on_cran()
  path <- test_prepare_orderly_example("minimal")
  id1 <- orderly_run("example", root = path, echo = FALSE)
  id2 <- orderly_run("example", root = path, echo = FALSE)
  file.remove(file.path(path, "draft", "example", id2, "orderly_run.rds"))

  expect_setequal(
    orderly_list_dir(file.path(path, "draft", "example")),
    c(id1, id2))
  expect_setequal(
    orderly_list_dir(file.path(path, "draft", "example"), TRUE),
    id1)

  expect_equal(orderly_latest("example", draft = TRUE, root = path), id1)
  expect_equal(orderly_list_drafts("example", root = path)$id, id1)
})


test_that("orderly_find_report", {
  skip_on_cran_windows()
  path <- test_prepare_orderly_example("minimal")
  id1 <- orderly_run("example", root = path, echo = FALSE)
  id2 <- orderly_run("example", root = path, echo = FALSE)

  p <- orderly_find_report("latest", "example", config = path)
  expect_equal(normalizePath(p),
               normalizePath(file.path(path, "draft", "example", id2)))
  p <- orderly_find_report(id1, "example", config = path)
  expect_equal(normalizePath(p),
               normalizePath(file.path(path, "draft", "example", id1)))

  expect_null(
    orderly_find_report(new_report_id(), "example", config = path))
  expect_error(
    orderly_find_report(new_report_id(), "example", config = path,
                        must_work = TRUE),
    "Did not find draft report example:")
  expect_error(
    orderly_find_report(new_report_id(), "example", config = path,
                        must_work = TRUE, draft = FALSE),
    "Did not find archive report example:")
})
