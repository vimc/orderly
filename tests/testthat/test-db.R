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
  info <- readRDS(p)$meta$file_info_inputs

  expect_equal(info$filename[info$file_purpose == "source"], "functions.R")
  expect_equal(info$file_hash[info$file_purpose == "source"],
               "cceb0c1c68beaa96266c6f2e3445b423")

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


test_that("db includes custom fields", {
  path <- prepare_orderly_example("demo")
  id <- orderly_run("minimal", root = path, echo = FALSE)
  orderly_commit(id, root = path)
  con <- orderly_db("destination", root = path)
  on.exit(DBI::dbDisconnect(con))
  d <- DBI::dbReadTable(con, "report_version_custom_fields")
  expect_equal(d$report_version, rep(id, 3))
  v <- c("requester", "author", "comment")
  expect_setequal(d$key, v)
  expect_equal(d$value[match(v, d$key)],
               c("Funder McFunderface",
                 "Researcher McResearcherface",
                 "This is a comment"))
})

test_that("db includes file information", {
  path <- prepare_orderly_example("demo")
  id <- orderly_run("multifile-artefact", root = path, echo = FALSE)
  p <- orderly_commit(id, root = path)

  con <- orderly_db("destination", root = path)
  on.exit(DBI::dbDisconnect(con))

  file_input <- DBI::dbReadTable(con, "file_input")
  expect_equal(
    file_input,
    data_frame(id = 1:2,
               report_version = id,
               file_hash = c("26f10ce8e0dba5993709b8bc6262fb6f",
                             "eda0ed142005488307e065831ad66f72"),
               filename = c("orderly.yml", "script.R"),
               file_purpose = c("orderly_yml", "script")))

  info <- readRDS(path_orderly_run_rds(p))$meta$file_info_artefacts
  artefact_hash <- info$file_hash

  ## Artefacts:
  file_artefact <- DBI::dbReadTable(con, "file_artefact")
  expect_equal(
    file_artefact,
    data_frame(id = 1:2,
               artefact = 1,
               file_hash = artefact_hash,
               filename = c("mygraph.png", "mygraph.pdf")))

  report_version_artefact <- DBI::dbReadTable(con, "report_version_artefact")
  expect_equal(
    report_version_artefact,
    data_frame(id = 1,
               report_version = id,
               format = "staticgraph",
               description = "A graph of things",
               order = 1))

  file <- DBI::dbReadTable(con, "file")
  expect_equal(file,
               data_frame(hash = c("26f10ce8e0dba5993709b8bc6262fb6f",
                                   "eda0ed142005488307e065831ad66f72",
                                   artefact_hash),
                          size = c(269L, 175L, 37853L, 4926L)))
})
