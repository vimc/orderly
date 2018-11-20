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

  dat <- list(source = list(driver = "RSQLite::SQLite",
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


test_that("minimum orderly version is enforced", {
  path <- tempfile()
  dir.create(path)

  dat <- list(source = list(driver = "RSQLite::SQLite"),
              minimum_orderly_version = "9.9.9")
  writeLines(yaml::as.yaml(dat), path_orderly_config_yml(path))

  expect_error(orderly_config(path),
               "Orderly version '9.9.9' is required, but only",
               fixed = TRUE)
})


test_that("minimum version is a less than relationship", {
  path <- tempfile()
  dir.create(path)

  dat <- list(source = list(driver = "RSQLite::SQLite"),
              minimum_orderly_version = as.character(packageVersion("orderly")))
  writeLines(yaml::as.yaml(dat), path_orderly_config_yml(path))
  cfg <- orderly_config(path)
  expect_is(cfg, "orderly_config")
  expect_equal(cfg$minimum_orderly_version, dat$minimum_orderly_version)
})


test_that("support declaring api server", {
  path <- tempfile()
  dir.create(path)
  dat <- list(source = list(driver = "RSQLite::SQLite"),
              api_server = list(
                myhost = list(
                  host = "myhost.com",
                  port = 443,
                  basic = TRUE,
                  username = "orderly",
                  password = "secert")))
  writeLines(yaml::as.yaml(dat), path_orderly_config_yml(path))
  cfg <- orderly_config(path)

  expect_is(cfg$api_server, "list")
  expect_is(cfg$api_server$myhost$server, "montagu_server")

  withr::with_envvar(
    c("ORDERLY_API_SERVER_IDENTITY" = NA),
    expect_null(orderly_config(path)$api_server_identity))
  withr::with_envvar(
    c("ORDERLY_API_SERVER_IDENTITY" = "myhost"),
    expect_equal(orderly_config(path)$api_server_identity, "myhost"))
  withr::with_envvar(
    c("ORDERLY_API_SERVER_IDENTITY" = "other"),
    expect_error(
      orderly_config(path)$api_server_identity,
      "api_server_identity must be one of 'myhost'"))
})

test_that("no global folder", {
  path <- prepare_orderly_example("global")
  # now we break the orderly_config.yml
  path_config <- file.path(path, "orderly_config.yml")
  config_lines <- readLines(path_config)
  # we know the final line (line six) is the global resource folder
  # so set it to something invalid
  config_lines[6] <- "  invalid_directory"
  writeLines(config_lines, path_config)

  expect_error( 
    orderly_config(path = path),
    "global resource does not exist: 'invalid_directory'",
    fixed = TRUE
  )
})
