context("demo")

test_that("orderly_demo", {
  path <- create_orderly_demo()
  expect_true(file.exists(path))

  con <- orderly_db("destination", root = path)
  on.exit(DBI::dbDisconnect(con))
  ## displayname and description are the only two nullable columns
  ## (VIMC-2357)
  sql <- paste("SELECT id, report, displayname, description",
               "FROM report_version")
  d <- DBI::dbGetQuery(con, sql)

  expect_false(any(is.na(d$displayname[d$report == "other"])))
  expect_false(any(is.na(d$description[d$report == "other"])))
  expect_true(all(is.na(d$displayname[d$report == "minimal"])))
  expect_true(all(is.na(d$description[d$report == "minimal"])))

  ## Ensure that the time manipulation affects the changelog too
  expect_true(nrow(DBI::dbReadTable(con, "changelog")) > 0)
})


test_that("git demo", {
  path1 <- prepare_orderly_git_example(run_report = FALSE)
  capture.output(path2 <- prepare_orderly_git_example(run_report = TRUE))

  expect_equal(
    nrow(orderly_list2(root = path1[["local"]], draft = FALSE)), 0)
  expect_equal(
    nrow(orderly_list2(root = path2[["local"]], draft = FALSE)), 1)
})


test_that("demo infrastructure", {
  path <- prepare_orderly_example("demo")
  file.remove(file.path(path, "before.R"))
  expect_error(run_orderly_demo(path),
               "function .* not found in before\\.R")
})
