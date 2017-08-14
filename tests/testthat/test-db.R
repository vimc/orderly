context("db")

test_that("invalid db type", {
  expect_error(orderly_db("xxx", "example"),
               "Invalid db type 'xxx'")
})

test_that("custom fields", {
  path <- tempfile()
  on.exit({
    if (exists("con")) {
      ## flush any SQLite connections:
      rm(con)
      gc()
    }
    unlink(path, recursive = TRUE)
  })

  orderly_init(path)
  file.copy("example_config.yml", file.path(path, "orderly_config.yml"),
            overwrite = TRUE)
  con <- orderly_db("destination", path)
  expect_equal(DBI::dbListTables(con), "orderly")
  expect_true(DBI::dbExistsTable(con, "orderly"))

  ## TODO: should the db initialisation here check that the custom
  ## fields are all OK?  But that will happen rather a lot and that's
  ## not great either.  But then performance probably does not matter.

  config <- orderly_config_get(path)
  expect_error(report_db_init(con, config, TRUE),
               "Table 'orderly' already exists")

  d <- DBI::dbReadTable(con, "orderly")
  d <- d[setdiff(names(d), "author")]
  DBI::dbWriteTable(con, "orderly", d, overwrite = TRUE)
  expect_error(report_db_init(con, config, FALSE),
               "custom fields 'author' not present in existing database")

  config$fields <- NULL
  expect_error(report_db_init(con, config, FALSE),
               "custom fields 'requester', 'comments' in database")
})

test_that("rebuild empty database", {
  skip_if_not_installed("RSQLite")
  path <- tempfile()
  orderly_init(path)
  file.copy("example_config.yml", file.path(path, "orderly_config.yml"),
            overwrite = TRUE)
  orderly_rebuild(path)

  con <- orderly_db("destination", path)
  on.exit(DBI::dbDisconnect(con))
  expect_equal(DBI::dbListTables(con), "orderly")
})
