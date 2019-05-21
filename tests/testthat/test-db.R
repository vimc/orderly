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

  expect_true(DBI::dbExistsTable(con, "orderly_schema"))

  config <- orderly_config_get(path)
  expect_error(report_db_init(con, config, TRUE),
               "Table 'orderly_schema' already exists")

  DBI::dbExecute(con, "DELETE FROM custom_fields WHERE id = 'author'")
  expect_error(report_db_init(con, config, FALSE),
               "custom fields 'author' not present in existing database")

  config$fields <- NULL
  expect_error(report_db_init(con, config, FALSE),
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
  expect_true(DBI::dbExistsTable(con, "orderly_schema"))
})

test_that("rebuild nonempty database", {
  path <- prepare_orderly_example("minimal")
  id <- orderly_run("example", root = path, echo = FALSE)
  orderly_commit(id, root = path)
  file.remove(file.path(path, "orderly.sqlite"))
  orderly_rebuild(path)
  orderly_rebuild(path)
  con <- orderly_db("destination", path)
  on.exit(DBI::dbDisconnect(con))
  expect_equal(nrow(DBI::dbReadTable(con, "report_version")), 1)
})

test_that("no transient db", {
  config <- list(destination = list(
                   driver = c("RSQLite", "SQLite"),
                   args = list(dbname = ":memory:")),
                 root = ".")
  expect_error(orderly_db_args(config$destination, config = config),
               "Cannot use a transient SQLite database with orderly")
})


test_that("db includes parameters", {
  path <- prepare_orderly_example("example")
  id <- orderly_run("example", parameters = list(cyl = 4), root = path,
                    echo = FALSE)
  orderly_commit(id, root = path)
  con <- orderly_db("destination", root = path)
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
                    root = path, echo = FALSE)
  orderly_commit(id, root = path)
  con <- orderly_db("destination", root = path)
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
                    root = path, echo = FALSE)
  expect_error(orderly_commit(id, root = path),
               "Unsupported parameter type")
  expect_error(report_db_parameter_type(t), "Unsupported parameter type")
  expect_error(report_db_parameter_serialise(t), "Unsupported parameter type")
})


test_that("dialects", {
  skip_on_cran() # likely platform dependent
  s <- report_db_schema_read(NULL, "sqlite")
  p <- report_db_schema_read(NULL, "postgres")
  expect_false(isTRUE(all.equal(s, p)))

  path <- prepare_orderly_example("minimal")
  config <- orderly_config(path)
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  expect_error(report_db_init_create(con, config, "postgres"),
               "syntax error")

  expect_silent(report_db_init_create(con, config, "sqlite"))

  expect_equal(report_db_dialect(con), "sqlite")
  expect_equal(report_db_dialect(structure(TRUE, class = "PqConnection")),
               "postgres")
  expect_error(report_db_dialect(structure(TRUE, class = "other")),
               "Can't determine SQL dialect")
})


test_that("sources are listed in db", {
  path <- prepare_orderly_example("demo")
  id <- orderly_run("other", root = path, parameters = list(nmin = 0),
                    echo = FALSE)
  orderly_commit(id, root = path)

  con <- orderly_db("destination", root = path)
  on.exit(DBI::dbDisconnect(con))

  p <- path_orderly_run_rds(file.path(path, "archive", "other", id))
  expect_equal(readRDS(p)$meta$hash_sources,
               list("functions.R" = "cceb0c1c68beaa96266c6f2e3445b423"))
  d <- DBI::dbGetQuery(
    con, "SELECT * from file_input WHERE report_version = $1", id)
  expect_false("resource" %in% d$file_purpose)
  expect_true("source" %in% d$file_purpose)
})


test_that("backup", {
  path <- create_orderly_demo()
  expect_message(
    orderly_backup(path),
    "orderly.sqlite => backup/db/orderly.sqlite",
    fixed = TRUE)

  dest <- path_db_backup(path, "orderly.sqlite")
  expect_true(file.exists(dest))

  dat_orig <- with_sqlite(file.path(path, "orderly.sqlite"), function(con)
    DBI::dbReadTable(con, "report_version"))
  dat_backup <- with_sqlite(dest, function(con)
    DBI::dbReadTable(con, "report_version"))
  expect_equal(dat_orig, dat_backup)
})
