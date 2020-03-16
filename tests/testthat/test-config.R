context("config")

test_that("read", {
  cfg <- orderly_config("example")
  expect_is(cfg, "orderly_config")

  ## default destination database:
  expect_equal(cfg$destination$driver, c("RSQLite", "SQLite"))
  expect_equal(cfg$destination$args, list(dbname = "orderly.sqlite"))

  dat <- orderly_db_args(cfg$destination, cfg, "loc")
  expect_identical(dat$driver, RSQLite::SQLite)
  expect_identical(dat$args$dbname, file.path(cfg$root, "orderly.sqlite"))
})

test_that("environment variables", {
  path <- tempfile()
  dir.create(path)

  dat <- list(database =
                list(source =
                       list(driver = "RSQLite::SQLite",
                            args = list(
                              host = "OURHOST",
                              port = "OURPORT",
                              user = "OURUSER",
                              dbname = "OURDBNAME",
                              password = "$OURPASSWORD"))))
  writeLines(yaml::as.yaml(dat), path_orderly_config_yml(path))

  cfg <- orderly_config(path)

  expect_error(
    orderly_db_args(cfg$database$source, cfg, "loc"),
    "Environment variable 'OURPASSWORD' is not set.*used in loc:password")

  dat <- withr::with_envvar(
    c(OURPASSWORD = "foo"),
    orderly_db_args(cfg$database$source, cfg, "loc"))
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


test_that("get: fail descend", {
  expect_error(withr::with_dir(tempdir(), orderly_config_get(NULL, TRUE)),
               "Reached root")
})


test_that("minimum orderly version is enforced", {
  path <- tempfile()
  dir.create(path)

  dat <- list(minimum_orderly_version = "9.9.9")
  writeLines(yaml::as.yaml(dat), path_orderly_config_yml(path))

  expect_error(orderly_config(path),
               "Orderly version '9.9.9' is required, but only",
               fixed = TRUE)
})


test_that("minimum version is a less than relationship", {
  path <- tempfile()
  dir.create(path)

  dat <- list(minimum_orderly_version = as.character(packageVersion("orderly")))
  writeLines(yaml::as.yaml(dat), path_orderly_config_yml(path))
  cfg <- orderly_config(path)
  expect_is(cfg, "orderly_config")
  expect_equal(cfg$minimum_orderly_version, dat$minimum_orderly_version)
})


test_that("support declaring api server", {
  path <- tempfile()
  dir.create(path)
  dat <- list(remote = list(
                main = list(
                  driver = "orderly::orderly_remote_path",
                  primary = TRUE,
                  master_only = TRUE,
                  args = list(root = path)),
                other = list(
                  driver = "orderly::orderly_remote_path",
                  args = list(root = path))))
  writeLines(yaml::as.yaml(dat), path_orderly_config_yml(path))
  cfg <- orderly_config(path)

  expect_is(cfg$remote, "list")
  expect_equal(cfg$remote$main$args,
               list(root = path, name = "main"))

  cfg <- withr::with_envvar(
    c("ORDERLY_API_SERVER_IDENTITY" = "main"),
    orderly_config(path))
  expect_equal(cfg$remote_identity, "main")
  expect_true(cfg$server_options$primary)
  expect_true(cfg$server_options$master_only)

  cfg <- withr::with_envvar(
    c("ORDERLY_API_SERVER_IDENTITY" = "other"),
    orderly_config(path))
  expect_equal(cfg$remote_identity, "other")
  expect_false(cfg$server_options$primary)
  expect_false(cfg$server_options$master_only)

  cfg <- withr::with_envvar(
    c("ORDERLY_API_SERVER_IDENTITY" = NA),
    orderly_config(path))
  expect_null(cfg$remote_identity)
  expect_null(cfg$server_options)

  withr::with_envvar(
    c("ORDERLY_API_SERVER_IDENTITY" = "something-else"),
    expect_error(
      orderly_config(path)$remote_identity,
      "remote_identity must be one of 'main', 'other'"))
})


test_that("api server has only one primary", {
  path <- tempfile()
  dir.create(path)
  dat <- list(remote = list(
                main = list(
                  driver = "orderly::orderly_remote_path",
                  primary = TRUE,
                  args = list(root = path)),
                other = list(
                  driver = "orderly::orderly_remote_path",
                  primary = TRUE,
                  args = list(root = path))))

  writeLines(yaml::as.yaml(dat), path_orderly_config_yml(path))
  expect_error(
    orderly_config(path),
    "At most one remote can be listed as primary but here 2 are")
  dat$remote$other$primary <- FALSE
  writeLines(yaml::as.yaml(dat), path_orderly_config_yml(path))
  cfg <- orderly_config(path)
  expect_true(cfg$remote$main$primary)
  expect_false(cfg$remote$other$primary)
})

test_that("remote parse check", {
  path <- tempfile()
  dir.create(path)
  dat <- list(remote = list(
                myhost = list(
                  driver = "orderly::orderly_remote_path",
                  primary = TRUE,
                  args = list(root = path),
                  master_only = "yeah")))
  writeLines(yaml::as.yaml(dat), path_orderly_config_yml(path))
  expect_error(
    orderly_config(path),
    "'.+/orderly_config.yml:remote:myhost:master_only' must be logical")
})

test_that("no global folder", {
  path <- prepare_orderly_example("global", testing = TRUE)
  # now we break the orderly_config.yml
  path_config <- file.path(path, "orderly_config.yml")
  dat <- yaml_read(path_config)
  dat$global_resources <- "invalid_directory"
  writeLines(yaml::as.yaml(dat), path_config)

  expect_error(
    orderly_config(root = path),
    "global resource does not exist: 'invalid_directory'",
    fixed = TRUE
  )
})


test_that("vault configuration validation when absent", {
  expect_null(config_check_vault(NULL, NULL, "orderly.yml"))
})


test_that("vault configuration validation for typical use", {
  vault <- list(addr = "https://vault.example.com")
  expect_identical(config_check_vault(vault, NULL, "orderly.yml"), vault)

  vault <- list(addr = "https://vault.example.com",
                auth = list(method = "github"))
  expect_identical(config_check_vault(vault, NULL, "orderly.yml"), vault)
})


test_that("vault configuration requires string for url", {
  oo <- options(orderly.nowarnings = TRUE)
  on.exit(options(oo))
  expect_error(config_check_vault(NULL, TRUE, "orderly.yml"),
               "'orderly.yml:vault_server' must be character")
  expect_error(
    config_check_vault(NULL, c("a", "b"), "orderly.yml"),
    "'orderly.yml:vault_server' must be a scalar")
  expect_null(config_check_vault(NULL, NULL, "orderly.yml"))
})


test_that("previous configuration is transformed with warning", {
  addr <- "https://vault.example.com"
  expect_warning(
    res <- config_check_vault(NULL, addr, "orderly.yml"),
    "Use of 'vault_server' is deprecated")
  expect_equal(res, list(addr = addr))
})

test_that("vault_server (not vault) in configuration yaml", {

  # 1.0.10 - this fails if config.R:55 uses $vault instead of [['vault']]

  path <- prepare_orderly_example("minimal")
  path_config <- file.path(path, "orderly_config.yml")
  text <- readLines(path_config)

  expect_null(orderly_config(root = path)$vault)

  url <- "https://vault.example.com"
  writeLines(c(text, sprintf("vault_server: %s", url)), path_config)
  expect_warning(res <- orderly_config(root = path)$vault,
                 "Use of 'vault_server' is deprecated")
  expect_equal(res, list(addr = url))
})

test_that("Can't use both new and old vault configurations", {
  expect_error(config_check_vault(list(login = "token"),
                                  "https://vault.example.com",
                                  "orderly.yml"),
               "Can't specify both 'vault' and 'vault_server' in orderly.yml")
})


test_that("vault configuration", {
  path <- prepare_orderly_example("minimal")
  path_config <- file.path(path, "orderly_config.yml")
  text <- readLines(path_config)

  expect_null(orderly_config(root = path)$vault)

  url <- "https://vault.example.com"
  writeLines(c(text, sprintf("vault:\n  addr: %s", url)), path_config)
  expect_equal(orderly_config(root = path)$vault, list(addr = url))
})


test_that("can't use both database and source sections", {
  path <- prepare_orderly_example("minimal")
  path_config <- file.path(path, "orderly_config.yml")
  txt <- readLines(path_config)
  dat <- list(source = list(driver = "RSQLite::SQLite",
                            dbname = "source.sqlite"))
  writeLines(c(txt, yaml::as.yaml(dat)), path_config)
  expect_error(orderly_config(path),
               "Both 'database' and 'source' fields may not be used",
               fixed = TRUE)
})


test_that("can read a configuration with no database", {
  path <- prepare_orderly_example("db0", testing = TRUE)
  config <- orderly_config(path)
  expect_false("database" %in% names(config))
})


test_that("can read a configuration with two databases", {
  path <- prepare_orderly_example("db2", testing = TRUE)
  config <- orderly_config(path)
  expect_setequal(names(config$database), c("source1", "source2"))
  expect_equal(config$database$source1$args, list(dbname = "source1.sqlite"))
  expect_equal(config$database$source2$args, list(dbname = "source2.sqlite"))

  con <- orderly_db("source", root = path)
  DBI::dbListTables(con$source1)
  DBI::dbListTables(con$source2)
})


test_that("warn when reading old-style configuration", {
  path <- withr::with_options(
    list(orderly.nowarnings = TRUE),
    prepare_orderly_example("olddb", testing = TRUE))

  expect_warning(orderly_config(path),
                 "Use of 'source' is deprecated and will be removed")
})


test_that("warn when reading old-style db config", {
  path <- prepare_orderly_example("minimal")
  content <- c(
    "database:",
    "  source:",
    "    driver: RSQLite::SQLite",
    "    dbname: source.sqlite")
  writeLines(content, file.path(path, "orderly_config.yml"))
  expect_warning(
    orderly_config(path),
    "Please move your database arguments")
  withr::with_options(
    list(orderly.nowarnings = TRUE),
    expect_warning(orderly_config(path), NA))
})


test_that("warn when using url in remote definition", {
  path <- prepare_orderly_example("minimal")
  append_lines(c(
    "remote:",
    "  testing:",
    "    driver: orderly::orderly_remote_path",
    "    url: https://example.com",
    "    args:",
    sprintf("      path: %s", path),
    "    slack_url: https://httpbin.org/post"),
    file.path(path, "orderly_config.yml"))
  expect_warning(
    orderly_config(path),
    "deprecated and will be dropped")
  withr::with_options(
    list(orderly.nowarnings = TRUE),
    expect_warning(orderly_config(path), NA))
})


test_that("multiple database configurations", {
  path <- prepare_orderly_example("minimal")
  p <- file.path(path, "orderly_config.yml")
  writeLines(c(
    "database:",
    "  source:",
    "    driver: RSQLite::SQLite",
    "    instances:",
    "      staging:",
    "        dbname: staging.sqlite",
    "      production:",
    "        dbname: production.sqlite",
    "    default_instance: production"),
    p)
  cfg <- orderly_config(path)
  expect_equal(cfg$database$source$args, list(dbname = "production.sqlite"))
  expect_equal(cfg$database$source$instances,
               list(staging = list(dbname = "staging.sqlite"),
                    production = list(dbname = "production.sqlite")))
})


test_that("instances not supported for destination db", {
  path <- prepare_orderly_example("minimal")
  p <- file.path(path, "orderly_config.yml")
  writeLines(c(
    "database:",
    "  source:",
    "    driver: RSQLite::SQLite",
    "    args:",
    "        dbname: staging.sqlite",
    "destination:",
    "    driver: RSQLite::SQLite",
    "    instances:",
    "      staging:",
    "        dbname: dest-staging.sqlite",
    "      production:",
    "        dbname: dest-production.sqlite"),
    p)
  expect_error(
    orderly_config(path),
    "Unknown fields in .*orderly_config.yml:destination: instances")
})


test_that("default_instance not allowed without instances", {
  path <- prepare_orderly_example("minimal")
  p <- file.path(path, "orderly_config.yml")
  writeLines(c(
    "database:",
    "  source:",
    "    driver: RSQLite::SQLite",
    "    args:",
    "      dbname: destination.sqlite",
    "    default_instance: production"),
    p)
  expect_error(
    orderly_config(path),
    "Can't specify 'default_instance' with no defined instances")
})


test_that("default instance from an environmental variable", {
  path <- prepare_orderly_example("minimal")
  p <- file.path(path, "orderly_config.yml")
  writeLines(c(
    "database:",
    "  source:",
    "    driver: RSQLite::SQLite",
    "    instances:",
    "      staging:",
    "        dbname: staging.sqlite",
    "      production:",
    "        dbname: production.sqlite",
    "    default_instance: $ORDERLY_TEST_DEFAULT_INSTANCE"),
    p)
  cfg <- orderly_config(path)
  expect_equal(cfg$database$source$args, list(dbname = "staging.sqlite"))

  cfg <- withr::with_envvar(
    c("ORDERLY_TEST_DEFAULT_INSTANCE" = "production"),
    orderly_config(path))
  expect_equal(cfg$database$source$args, list(dbname = "production.sqlite"))

  writeLines("ORDERLY_TEST_DEFAULT_INSTANCE: production",
             file.path(path, "orderly_envir.yml"))
  cfg <- orderly_config(path)
  expect_equal(cfg$database$source$args, list(dbname = "production.sqlite"))
})


test_that("tags can be included in the configuration", {
  path <- prepare_orderly_example("minimal")
  expect_null(orderly_config(path)$tags)
  p <- file.path(path, "orderly_config.yml")
  append_lines(c("tags:", "  - tag1", "  - tag2"), p)
  expect_equal(orderly_config(path)$tags, c("tag1", "tag2"))
})


test_that("tags are validated", {
  path <- prepare_orderly_example("minimal")
  p <- file.path(path, "orderly_config.yml")
  append_lines(c("tags:", "  - 1", "  - 2"), p)
  expect_error(
    orderly_config(path),
    "orderly_config.yml:tags' must be character")
})


## VIMC-3442
test_that("adding new fields in new versions gives good errors", {
  path <- prepare_orderly_example("minimal")
  append_lines(
    c("new_toplevel_field: value",
      "minimum_orderly_version: 9.9.9"),
    file.path(path, "orderly_config.yml"))
  expect_error(
    orderly_config(path),
    "Orderly version '9.9.9' is required, but only '.+' installed")
})
