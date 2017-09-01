context("config")

test_that("read", {
  cfg <- orderly_config("example")
  expect_is(cfg, "orderly_config")

  ## default destination database:
  expect_equal(cfg$destination$driver, c("RSQLite", "SQLite"))
  expect_equal(cfg$destination$args, list(dbname = "orderly.sqlite"))

  dat <- orderly_db_args("destination", cfg)
  expect_identical(dat$driver, RSQLite::SQLite)
  expect_identical(dat$args$dbname, file.path(cfg$path, "orderly.sqlite"))
})

test_that("environment variables", {
  path <- tempfile()
  dir.create(path)

  dat <- list(source = list(driver = "RPostgres::Postgres",
                            host = "OURHOST",
                            port = "OURPORT",
                            user = "OURUSER",
                            dbname = "OURDBNAME",
                            password = "$OURPASSWORD"))
  writeLines(yaml::as.yaml(dat), path_orderly_config_yml(path))

  cfg <- orderly_config(path)

  expect_error(orderly_db_args("source", cfg),
               "Environment variable 'OURPASSWORD' is not set")

  dat <- withr::with_envvar(
    c(OURPASSWORD = "foo"),
    orderly_db_args("source", cfg))
  expect_equal(dat$args$password, "foo")
  expect_equal(dat$args$host, "OURHOST")
})

test_that("not found", {
  expect_error(orderly_config(tempfile()),
               "Did not find file 'orderly_config.yml' at path")
})

test_that("get: invalid config", {
  expect_error(orderly_config_get(1), "Invalid input")
})

test_that("get: default", {
  cfg <- orderly_config("example")
  oo <- orderly_default_config_set(cfg)
  on.exit(options(oo))

  expect_identical(orderly_default_config(), cfg)
  expect_identical(orderly_config_get(NULL), cfg)

  orderly_default_config_set(NULL)
  expect_error(orderly_default_config(),
               "orderly configuration not found")

  path <- tempfile()
  dir_create(path)
  expect_error(with_wd(path, orderly_default_config(TRUE)),
               "Reached root")
  file.copy(file.path("example/orderly_config.yml"), path)
  cfg2 <- with_wd(path, orderly_default_config(TRUE))
  expect_identical(cfg2$path, normalizePath(path))
})
