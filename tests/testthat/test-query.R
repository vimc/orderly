context("query")

test_that("empty", {
  path <- tempfile()
  orderly_init(path, quiet = TRUE)
  file_copy("minimal_config.yml", file.path(path, "orderly_config.yml"),
            overwrite = TRUE)
  expect_equal(orderly_list(path), character(0))
  expect_equal(orderly_list_drafts(path),
               data.frame(name = character(0),
                          id = character(0),
                          stringsAsFactors = FALSE))
})

test_that("non-empty", {
  path <- prepare_minimal()
  expect_equal(orderly_list(path), "example")
})

test_that("query through lifecycle", {
  orderly_log_start()
  path <- prepare_minimal()
  expect_equal(orderly_list(config = path), "example")

  empty <- data.frame(name = character(0), id = character(0),
                      stringsAsFactors = FALSE)
  expect_equal(orderly_list_drafts(path), empty)
  expect_equal(orderly_list_archive(path), empty)

  id <- orderly_run("example", config = path, echo = FALSE)

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

  orderly_commit(id, config = path)

  expect_equal(orderly_list_drafts(path), empty)
  expect_equal(orderly_list_archive(path), r)
})
