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

  config <- orderly_config(path)
  expect_error(report_db_init(con, config, TRUE),
               "Table 'orderly_schema' already exists")

  DBI::dbExecute(con, "DELETE FROM custom_fields WHERE id = 'author'")
  expect_error(report_db_init(con, config, FALSE),
               "custom fields 'author' not present in existing database")

  unlockBinding(quote(fields), config)
  config$fields <- NULL
  expect_error(report_db_init(con, config, FALSE),
               "custom fields 'requester', 'comments' in database")
})

test_that("rebuild empty database", {
  skip_on_cran_windows()
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
  skip_on_cran_windows()
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
                 root = tempdir())
  expect_error(orderly_db_args(config$destination, config = config),
               "Cannot use a transient SQLite database with orderly")
})


test_that("db includes parameters", {
  skip_on_cran_windows()
  path <- prepare_orderly_example("demo")
  id <- orderly_run("other", parameters = list(nmin = 0.1), root = path,
                    echo = FALSE)
  orderly_commit(id, root = path)
  con <- orderly_db("destination", root = path)
  d <- DBI::dbReadTable(con, "parameters")
  DBI::dbDisconnect(con)
  expect_equal(d, data_frame(id = 1,
                             report_version = id,
                             name = "nmin",
                             type = "number",
                             value = "0.1"))
})


test_that("different parameter types are stored correctly", {
  skip_on_cran_windows()
  path <- prepare_orderly_example("parameters", testing = TRUE)
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
  t <- Sys.Date()
  expect_error(report_db_parameter_type(t), "Unsupported parameter type")
  expect_error(report_db_parameter_serialise(t), "Unsupported parameter type")
})


test_that("dialects", {
  skip_on_cran() # likely platform dependent
  s <- report_db_schema_read(NULL, "sqlite")
  p <- report_db_schema_read(NULL, "postgres")
  expect_false(isTRUE(all.equal(s, p)))

  path <- prepare_orderly_example("minimal")
  config <- orderly_config_$new(path)
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
  skip_on_cran_windows()
  path <- prepare_orderly_example("demo")
  id <- orderly_run("other", root = path, parameters = list(nmin = 0),
                    echo = FALSE)
  orderly_commit(id, root = path)

  con <- orderly_db("destination", root = path)
  on.exit(DBI::dbDisconnect(con))

  p <- path_orderly_run_rds(file.path(path, "archive", "other", id))
  info <- readRDS(p)$meta$file_info_inputs

  h <- hash_files(file.path(path, "archive", "other", id, "functions.R"), FALSE)

  expect_equal(info$filename[info$file_purpose == "source"], "functions.R")
  expect_equal(info$file_hash[info$file_purpose == "source"], h)

  d <- DBI::dbGetQuery(
    con, "SELECT * from file_input WHERE report_version = $1", id)
  expect_false("resource" %in% d$file_purpose)
  expect_true("source" %in% d$file_purpose)
})


test_that("backup", {
  skip_on_cran_windows()
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
  skip_on_cran_windows()
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
  skip_on_cran_windows()
  path <- prepare_orderly_example("demo")
  id <- orderly_run("multifile-artefact", root = path, echo = FALSE)
  p <- orderly_commit(id, root = path)
  h1 <- hash_files(
    file.path(path, "src", "multifile-artefact", "orderly.yml"), FALSE)
  h2 <- hash_files(
    file.path(path, "src", "multifile-artefact", "script.R"), FALSE)
  con <- orderly_db("destination", root = path)
  on.exit(DBI::dbDisconnect(con))

  file_input <- DBI::dbReadTable(con, "file_input")

  expect_equal(
    file_input,
    data_frame(id = 1:2,
               report_version = id,
               file_hash = c(h1, h2),
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

  filenames <- c("orderly.yml", "script.R", "mygraph.png", "mygraph.pdf")
  file <- DBI::dbReadTable(con, "file")

  expect_equal(file,
               data_frame(hash = c(h1, h2,
                                   artefact_hash),
                          size = file_size(file.path(p, filenames))))
})


test_that("connect to database instances", {
  path <- prepare_orderly_example("minimal")
  p <- file.path(path, "orderly_config.yml")
  writeLines(c(
    "database:",
    "  source:",
    "    driver: RSQLite::SQLite",
    "    args:",
    "      dbname: source.sqlite",
    "    instances:",
    "      staging:",
    "        dbname: staging.sqlite",
    "      production:",
    "        dbname: production.sqlite"),
    p)

  f <- function(x) {
    basename(x$source@dbname)
  }
  expect_equal(
    f(orderly_db("source", root = path)),
    "staging.sqlite")
  expect_equal(
    f(orderly_db("source", root = path, instance = "staging")),
    "staging.sqlite")
  expect_equal(
    f(orderly_db("source", root = path, instance = "production")),
    "production.sqlite")
})


test_that("db instance select", {
  config_db <- list(
    x = list(
      driver = c("RSQLite", "SQLite"),
      args = list(name = "a"),
      instances = list(
        a = list(name = "a"),
        b = list(name = "b"))),
    y = list(
      driver = c("RSQLite", "SQLite"),
      args = list(name = "y")))

  config_db_a <- modifyList(config_db, list(x = list(instance = "a")))
  config_db_b <- modifyList(config_db, list(x = list(args = list(name = "b"),
                                                     instance = "b")))

  ## The happy paths:
  expect_identical(db_instance_select(NULL, config_db), config_db_a)

  expect_equal(db_instance_select("a", config_db), config_db_a)
  expect_equal(db_instance_select("b", config_db), config_db_b)

  expect_equal(db_instance_select(c(x = "a"), config_db), config_db_a)
  expect_equal(db_instance_select(c(x = "b"), config_db), config_db_b)

  expect_error(db_instance_select("c", config_db),
               "Invalid instance 'c' for database 'x'")
  expect_error(db_instance_select(c(x = "c"), config_db),
               "Invalid instance: 'c' for 'x'")
  expect_error(db_instance_select(c(z = "a"), config_db),
               "Invalid database name 'z' in provided instance")
})


test_that("db instance select with two instanced databases", {
  config_db <- list(
    x = list(
      driver = c("RSQLite", "SQLite"),
      args = list(name = "b"),
      instances = list(
        b = list(name = "b"),
        a = list(name = "a"))),
    y = list(
      driver = c("RSQLite", "SQLite"),
      args = list(name = "c"),
      instances = list(
        c = list(name = "c"),
        a = list(name = "a"))))

  config_db_aa <- modifyList(config_db,
                             list(x = list(args = list(name = "a"),
                                           instance = "a"),
                                  y = list(args = list(name = "a"),
                                           instance = "a")))
  config_db_bc <- modifyList(config_db, list(x = list(instance = "b"),
                                             y = list(instance = "c")))
  config_db_ac <- modifyList(config_db,
                             list(x = list(args = list(name = "a"),
                                           instance = "a"),
                                  y = list(args = list(name = "c"),
                                           instance = "c")))

  ## The happy paths:
  expect_identical(db_instance_select(NULL, config_db), config_db_bc)

  expect_equal(db_instance_select("a", config_db), config_db_aa)
  expect_equal(db_instance_select(c(x = "a", y = "a"), config_db),
               config_db_aa)
  expect_equal(db_instance_select(c(x = "b", y = "c"), config_db),
               config_db_bc)
  expect_equal(db_instance_select(c(x = "a"), config_db), config_db_ac)

  ## Some error paths:
  expect_error(db_instance_select("f", config_db),
               "Invalid instance 'f' for databases 'x', 'y'")
  expect_error(db_instance_select(c(x = "f", y = "g"), config_db),
               "Invalid instances: 'f' for 'x', 'g' for 'y'")
  expect_error(db_instance_select(c(z = "a"), config_db),
               "Invalid database name 'z' in provided instance")
})


test_that("db instance select rejects instance when no dbs support it", {
  config_db <- list(
    x = list(
      driver = c("RSQLite", "SQLite"),
      args = list(name = "a")),
    y = list(
      driver = c("RSQLite", "SQLite"),
      args = list(name = "b")))

  expect_identical(db_instance_select(NULL, config_db), config_db)
  expect_error(db_instance_select("a", config_db),
               "Can't specify 'instance' with no databases supporting it")
})


test_that("Create and verify tags on startup", {
  root <- prepare_orderly_example("minimal")
  append_lines(c("tags:", "  - tag1", "  - tag2"),
               file.path(root, "orderly_config.yml"))
  con <- orderly_db("destination", root = root)
  expect_equal(DBI::dbReadTable(con, "tag"),
               data_frame(id = c("tag1", "tag2")))
  DBI::dbDisconnect(con)
  append_lines("  - tag3", file.path(root, "orderly_config.yml"))
  expect_error(
    orderly_db("destination", root = root),
    "tags have changed: rebuild with orderly::orderly_rebuild()",
    fixed = TRUE)
  orderly_rebuild(root)

  con <- orderly_db("destination", root = root)
  expect_equal(DBI::dbReadTable(con, "tag"),
               data_frame(id = c("tag1", "tag2", "tag3")))
  DBI::dbDisconnect(con)
})


test_that("Add tags to db", {
  root <- prepare_orderly_example("minimal")
  append_lines(c("tags:", "  - tag1", "  - tag2"),
               file.path(root, "orderly_config.yml"))
  append_lines(c("tags:", "  - tag1"),
               file.path(root, "src", "example", "orderly.yml"))
  id <- orderly_run("example", root = root, echo = FALSE)
  p <- orderly_commit(id, root = root)

  con <- orderly_db("destination", root)
  on.exit(DBI::dbDisconnect(con))
  expect_equal(
    DBI::dbReadTable(con, "report_version_tag"),
    data_frame(id = 1, report_version = id, tag = "tag1"))
})

test_that("add batch info to db", {
  path <- prepare_orderly_example("parameters", testing = TRUE)

  params <- data_frame(
    a = c("one", "two", "three"),
    b = c(1, 2, 3)
  )
  batch_id <- ids::random_id()
  mockery::stub(orderly_batch, "ids::random_id", batch_id)
  ids <- orderly_batch("example", parameters = params,
                       root = path, echo = FALSE)
  p <- lapply(ids, function(id) {
    orderly_commit(id, root = path)
  })

  con <- orderly_db("destination", path)
  on.exit(DBI::dbDisconnect(con))
  expect_equal(
    DBI::dbReadTable(con, "report_batch"),
    data_frame(id = batch_id))
  expect_equal(
    DBI::dbReadTable(con, "report_version_batch"),
    data_frame(report_version = ids, report_batch = rep(batch_id, 3)))
})

test_that("add workflow info to db", {
  path <- prepare_orderly_example("demo")

  mock_random_id <- mockery::mock("workflow_id", "report_id1", "report_id2")
  with_mock("ids::random_id" = mock_random_id, {
    ids <- orderly_workflow("my_workflow", root = path)
  })

  con <- orderly_db("destination", path)
  on.exit(DBI::dbDisconnect(con))
  expect_equal(
    DBI::dbReadTable(con, "workflow"),
    data_frame(id = "workflow_id",
               name = "my_workflow"))
  expect_equal(
    DBI::dbReadTable(con, "report_version_workflow"),
    data_frame(report_version = ids, workflow_id = rep("workflow_id", 2)))
})


## Regression test for vimc-3652
test_that("trailing slash in report name is tolerated", {
  path <- prepare_orderly_example("minimal")
  id <- orderly_run("src/example/", root = path, echo = FALSE)
  expect_error(orderly_commit(id, root = path), NA)
})


test_that("db includes elapsed time", {
  skip_on_cran_windows()
  path <- prepare_orderly_example("minimal")
  id <- orderly_run("example", root = path, echo = FALSE)
  p <- orderly_commit(id, root = path)
  con <- orderly_db("destination", root = path)
  on.exit(DBI::dbDisconnect(con))
  d <- DBI::dbReadTable(con, "report_version")
  expect_true(d$elapsed > 0)
  expect_equal(d$elapsed,
               readRDS(path_orderly_run_rds(p))$meta$elapsed)
})


test_that("rebuild nonempty database with backup", {
  skip_on_cran_windows()
  path <- prepare_orderly_example("minimal")
  id <- orderly_run("example", root = path, echo = FALSE)
  orderly_commit(id, root = path)

  con <- orderly_db("destination", path)
  DBI::dbExecute(con, "UPDATE report_version SET published = 1")
  DBI::dbDisconnect(con)

  orderly_rebuild(path)

  files <- dir(file.path(path, "backup/db"))
  expect_equal(length(files), 1)
  expect_match(files, "^orderly\\.sqlite\\.[0-9]{8}-[0-9]{6}$")

  con1 <- orderly_db("destination", path)
  con2 <- DBI::dbConnect(RSQLite::SQLite(),
                         dbname = file.path(path, "backup/db", files))
  expect_equal(
    DBI::dbReadTable(con1, "report_version")$published, 0)
  expect_equal(
    DBI::dbReadTable(con2, "report_version")$published, 1)

  DBI::dbDisconnect(con1)
  DBI::dbDisconnect(con2)
})

test_that("db write collision", {
  skip_on_cran()

  unlink("tmp", recursive = TRUE)
  path <- prepare_orderly_example("minimal", "tmp")
  id1 <- orderly_run("example", root = path, echo = FALSE)
  id2 <- orderly_run("example", root = path, echo = FALSE)

  orderly_commit(id1, root = path)
  con <- orderly_db("destination", root = path)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "BEGIN IMMEDIATE")
  DBI::dbExecute(con, "DELETE FROM file_artefact")

  elapsed <- system.time(
    testthat::expect_error(
      orderly_commit(id2, root = path, timeout = 5),
      "database is locked"))
  expect_true(elapsed["elapsed"] > 5)

  DBI::dbRollback(con)
  p <- orderly_commit(id2, root = path)
  ids <- DBI::dbGetQuery(con, "SELECT id from report_version")$id
  expect_equal(length(ids), 2)
  expect_setequal(ids, c(id1, id2))
})
