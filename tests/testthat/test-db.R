context("db")

test_that("invalid db type", {
  expect_error(orderly_db("xxx", "example"),
               "Invalid db type 'xxx'")
})

test_that("custom fields", {
  path <- tempfile()

  orderly_init(path)
  file_copy("example_config.yml", file.path(path, "orderly_config.yml"),
            overwrite = TRUE)
  con <- orderly_db("destination", path)
  on.exit(DBI::dbDisconnect(con))

  expect_true(DBI::dbExistsTable(con, "orderly"))
  expect_true(DBI::dbExistsTable(con, "orderly_schema"))

  ## TODO: should the db initialisation here check that the custom
  ## fields are all OK?  But that will happen rather a lot and that's
  ## not great either.  But then performance probably does not matter.

  config <- orderly_config_get(path)
  expect_error(report_db_init(con, config, TRUE),
               "Table 'orderly' already exists")
  expect_error(report_db2_init(con, config, TRUE),
               "Table 'orderly_schema' already exists")

  d <- DBI::dbReadTable(con, "orderly")
  d <- d[setdiff(names(d), "author")]
  DBI::dbWriteTable(con, "orderly", d, overwrite = TRUE)

  d <- DBI::dbReadTable(con, "report_version")
  d <- d[setdiff(names(d), "author")]
  DBI::dbWriteTable(con, "report_version", d, overwrite = TRUE)

  expect_error(report_db_init(con, config, FALSE),
               "custom fields 'author' not present in existing database")
  expect_error(report_db2_init(con, config, FALSE),
               "custom fields 'author' not present in existing database")

  config$fields <- NULL
  expect_error(report_db_init(con, config, FALSE),
               "custom fields 'requester', 'comments' in database")
  expect_error(report_db2_init(con, config, FALSE),
               "custom fields 'requester', 'comments' in database")
})

test_that("rebuild empty database", {
  path <- tempfile()
  orderly_init(path)
  file_copy("example_config.yml", file.path(path, "orderly_config.yml"),
            overwrite = TRUE)
  orderly_rebuild(path)

  con <- orderly_db("destination", path)
  on.exit(DBI::dbDisconnect(con))
  expect_true(DBI::dbExistsTable(con, "orderly"))
})

test_that("rebuild nonempty database", {
  path <- prepare_orderly_example("minimal")
  id <- orderly_run("example", config = path, echo = FALSE)
  orderly_commit(id, config = path)
  file.remove(file.path(path, "orderly.sqlite"))
  orderly_rebuild(path)
  orderly_rebuild(path)
  con <- orderly_db("destination", path)
  on.exit(DBI::dbDisconnect(con))
  expect_equal(nrow(DBI::dbReadTable(con, "orderly")), 1)
})

test_that("no transient db", {
  config <- list(destination = list(
                   driver = c("RSQLite", "SQLite"),
                   args = list(dbname = ":memory:")),
                 path = ".")
  expect_error(orderly_db_args("destination", config = config),
               "Cannot use a transient SQLite database with orderly")
})


test_that("db includes parameters", {
  path <- prepare_orderly_example("example")
  id <- orderly_run("example", parameters = list(cyl = 4), config = path,
                    echo = FALSE)
  orderly_commit(id, config = path)
  con <- orderly_db("destination", config = path)
  d <- DBI::dbReadTable(con, "parameters")
  DBI::dbDisconnect(con)
  expect_equal(d, data_frame(id = 1,
                             report_version = id,
                             name = "cyl",
                             type = "number",
                             value = "4"))
})


test_that("different parameter types are stored correctly", {
  path <- prepare_orderly_example("parameters")
  id <- orderly_run("example", parameters = list(a = 1, b = TRUE, c = "one"),
                    config = path, echo = FALSE)
  orderly_commit(id, config = path)
  con <- orderly_db("destination", config = path)
  d <- DBI::dbReadTable(con, "parameters")
  DBI::dbDisconnect(con)
  expect_equal(d, data_frame(id = 1:3,
                             report_version = id,
                             name = c("a", "b", "c"),
                             type = c("number", "boolean", "text"),
                             value = c("1", "true", "one")))
})


test_that("avoid unserialisable parameters", {
  path <- prepare_orderly_example("parameters")
  t <- Sys.Date()
  id <- orderly_run("example", parameters = list(a = t, b = TRUE, c = "one"),
                    config = path, echo = FALSE)
  expect_error(orderly_commit(id, config = path),
               "Unsupported parameter type")
  expect_error(report_db2_parameter_type(t), "Unsupported parameter type")
  expect_error(report_db2_parameter_serialise(t), "Unsupported parameter type")
})
