context("publish")

test_that("included example", {
  read_published <- function(path) {
    con <- orderly_db("destination", root = path)
    on.exit(DBI::dbDisconnect(con))
    d <- DBI::dbReadTable(con, "report_version")
    d$published
  }

  path <- prepare_orderly_example("example")
  expect_equal(read_published(path), numeric(0))

  id <- orderly_run("example", list(cyl = 4), root = path, echo = FALSE)
  p <- orderly_commit(id, root = path)

  yml <- path_orderly_published_yml(p)
  expect_false(file.exists(yml))
  expect_equal(read_published(path), 0)

  orderly_publish(id, root = path)
  expect_equal(read_published(path), 1)
  expect_true(file.exists(yml))
  expect_equal(yaml_read(yml), list(published = TRUE))

  expect_message(orderly_publish(id, root = path),
                 "Report is already published")
  expect_equal(read_published(path), 1)
  expect_true(file.exists(yml))
  expect_equal(yaml_read(yml), list(published = TRUE))

  orderly_publish(id, value = FALSE, root = path)
  expect_equal(read_published(path), 0)
  expect_true(file.exists(yml))
  expect_equal(yaml_read(yml), list(published = FALSE))

  expect_message(orderly_publish(id, value = FALSE, root = path),
                 "Report is already unpublished")
  expect_equal(read_published(path), 0)
  expect_true(file.exists(yml))
  expect_equal(yaml_read(yml), list(published = FALSE))
})
