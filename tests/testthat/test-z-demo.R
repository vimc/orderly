context("demo")

test_that("orderly_demo", {
  skip_on_cran_windows()
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
  expect_true(nrow(DBI::dbReadTable(con, "report_version_tag")) > 0)
})


test_that("git demo", {
  testthat::skip_on_cran()
  path1 <- test_prepare_orderly_git_example(run_minimal = FALSE)
  capture.output(path2 <- test_prepare_orderly_git_example(run_minimal = TRUE))
  capture.output(path3 <- test_prepare_orderly_git_example(run_other = TRUE))

  expect_equal(
    nrow(orderly_list2(root = path1[["local"]], draft = FALSE)), 0)
  expect_equal(
    nrow(orderly_list2(root = path2[["local"]], draft = FALSE)), 1)
  expect_true(
    nrow(orderly_list2(root = path3[["local"]], draft = FALSE)) > 20)
  expect_true(all(
    c("minimal", "other", "global", "use_dependency", "changelog",
      "use_resource", "multi-artefact", "multifile-artefact", "html",
      "interactive", "use_resource_dir", "connection", "spaces", "view") %in%
      orderly_list2(root = path3[["local"]], draft = FALSE)$name))
})


test_that("demo infrastructure", {
  path <- test_prepare_orderly_example("demo")
  file.remove(file.path(path, "before.R"))
  expect_error(run_orderly_demo(path),
               "function .* not found in before\\.R")
})
