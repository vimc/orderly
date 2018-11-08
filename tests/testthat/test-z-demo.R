context("demo")

test_that("orderly_demo", {
  path <- create_orderly_demo()
  expect_true(file.exists(path))

  con <- orderly_db("destination", config = path)
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
})
