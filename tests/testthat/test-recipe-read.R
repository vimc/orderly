context("recipe_read")

test_that("nonexistant file", {
  ## TODO: I think that we should be normalising the path here?
  config <- orderly_config_read_yaml("example_config.yml", ".")
  expect_error(recipe_read(tempfile(), config),
               "Report working directory does not exist")
  expect_error(recipe_read(tempdir(), config),
               "Orderly configuration does not exist")
})

test_that("minimal", {
  path <- prepare_orderly_example("minimal")
  on.exit(unlink(path))

  config <- orderly_config(path)
  path_example <- file.path(path, "src", "example")
  info <- recipe_read(path_example, config)

  expect_is(info$data$dat$query, "character")
  expect_equal(info$data$dat$query, "SELECT name, number FROM thing")
  expect_equal(info$data$dat$database, "source")

  expect_equal(info$script, "script.R")
  expect_equal(info$script_hash,
               hash_files(file.path(path_example, "script.R"), FALSE))
  expect_equal(info$path, path_example)
  expect_is(info$hash, "character")

  expect_null(info$displayname)
  expect_null(info$description)

  ## Now, with this in place, check the parse:
  yml <- file.path(path_example, "orderly.yml")
  dat <- yaml_read(yml)
  write <- function(d) {
    writeLines(yaml::as.yaml(d), yml)
  }

  sql <- file.path(path_example, "query.sql")
  writeLines(dat$data$dat$query, sql)
  write(modifyList(dat, list(data = list(dat = list(query = "query.sql")))))

  cmp <- recipe_read(path_example, config)
  expect_equal(cmp$data$dat$query_file, "query.sql")
  cmp$data$dat$query_file <- NULL
  info$data$dat$query_file <- NULL
  expect_equal(cmp$data, info$data)
  expect_equal(cmp$resources, "query.sql")

  write(modifyList(dat, list(data = list(dat = list(query = "foo.sql")))))
  expect_error(recipe_read(path_example, config),
               "SQL file does not exist: 'foo.sql'")

  ## TODO: The formatting of these error messages could be improved
  write(modifyList(dat, list("unknown" = "foo")))
  expect_error(recipe_read(path_example, config),
               "Unknown fields in .*: unknown")

  write(dat[setdiff(names(dat), "script")])
  expect_error(recipe_read(path_example, config),
               "Fields missing from .*: script")

  write(modifyList(dat, list(script = "missing.R")))
  expect_error(recipe_read(path_example, config),
               "Script file does not exist: 'missing.R'")

  write(modifyList(dat, list(resources = "missing-file")))
  expect_error(recipe_read(path_example, config),
               "Resource file does not exist: 'missing-file'")

  write(modifyList(dat, list(resources = "..")))
  expect_error(recipe_read(path_example, config),
               "Declared resources not in right place: \\.\\.")

  write(modifyList(dat, list(resources = "query.sql")))
  cmp <- recipe_read(path_example, config)
  expect_equal(cmp$resources, "query.sql")

  write(modifyList(dat, list(artefacts = character(0))))
  expect_error(recipe_read(path_example, config),
               "At least one artefact required")

  write(modifyList(dat, list(packages = 10)))
  expect_error(recipe_read(path_example, config),
               "orderly.yml:packages' must be character")
})

test_that("other", {
  path <- prepare_orderly_example("other")
  config <- orderly_config(path)
  info <- recipe_read(file.path(path_src(path), "other"), config)
  expect_is(info$displayname, "character")
  expect_is(info$description, "character")
})

test_that("ill formed artefacts", {
  path <- prepare_orderly_example("minimal")
  on.exit(unlink(path))
  config <- orderly_config(path)
  path_example <- file.path(path, "src", "example")
  yml <- file.path(path_example, "orderly.yml")
  dat <- yaml_read(yml)

  expect_silent(recipe_read(path_example, config))

  dat$artefacts <- c(dat$artefacts,
                     list(data = list(filename = "foo", description = "bar")))
  writeLines(yaml::as.yaml(dat), yml)
  expect_error(suppressMessages(recipe_read(path_example, config)),
               "Expected an ordered map")
})

test_that("unknown artefact type", {
  path <- prepare_orderly_example("minimal")
  on.exit(unlink(path))
  config <- orderly_config(path)
  path_example <- file.path(path, "src", "example")
  yml <- file.path(path_example, "orderly.yml")
  dat <- yaml_read(yml)

  expect_silent(recipe_read(path_example, config))

  dat$artefacts <- list(unknown = list(filename = "foo", description = "bar"))
  writeLines(yaml::as.yaml(dat), yml)
  expect_error(suppressMessages(recipe_read(path_example, config)),
               "Unknown artefact type: 'unknown'")
})

test_that("duplicate artefact filenames; within artefact", {
  path <- prepare_orderly_example("minimal")
  on.exit(unlink(path))
  config <- orderly_config(path)
  path_example <- file.path(path, "src", "example")
  yml <- file.path(path_example, "orderly.yml")
  dat <- yaml_read(yml)
  dat$artefacts[[1]]$filenames <- c("mygraph.png", "mygraph.png")
  writeLines(yaml::as.yaml(dat), yml)
  expect_error(recipe_read(path_example, config),
               "Duplicate artefact filenames are not allowed: 'mygraph.png'")
})


test_that("duplicate artefact filenames; between artefacts", {
  path <- prepare_orderly_example("minimal")
  on.exit(unlink(path))
  config <- orderly_config(path)
  path_example <- file.path(path, "src", "example")
  yml <- file.path(path_example, "orderly.yml")
  dat <- yaml_read(yml)
  dat$artefacts <- list(dat$artefacts, dat$artefacts)
  writeLines(yaml::as.yaml(dat), yml)
  expect_error(recipe_read(path_example, config),
               "Duplicate artefact filenames are not allowed: 'mygraph.png'")
})


test_that("resource case matters", {
  path <- prepare_orderly_example("minimal")
  file.rename(file.path(path, "src", "example", "script.R"),
              file.path(path, "src", "example", "script.r"))

  config <- orderly_config_get(path, FALSE)
  expect_error(recipe_read(file.path(path, "src", "example"), config),
               "Script file does not exist: 'script.R'")
})

test_that("dependencies must be scalar", {
  path <- prepare_orderly_example("depends")
  id <- orderly_run("example", root = path, echo = FALSE)

  filename <- file.path(path, "src", "depend", "orderly.yml")
  dat <- yaml_read(filename)
  dat$depends$example$use$previous.rds <- character(0)
  yaml_write(dat, filename)

  expect_error(orderly_run("depend", root = path, echo = FALSE),
               "depends:example:use must all be scalar character")
})


test_that("dependencies must exist", {
  path <- prepare_orderly_example("depends")
  id <- orderly_run("example", root = path, echo = FALSE)

  filename <- file.path(path, "src", "depend", "orderly.yml")
  dat <- yaml_read(filename)
  dat$depends$example$use$previous.rds <- "unknown.file"
  yaml_write(dat, filename)

  expect_error(orderly_run("depend", root = path, echo = FALSE),
               "Did not find file unknown.file at")
})


test_that("data field is optional", {
  path <- prepare_orderly_example("nodata")
  report_path <- file.path(path, "src", "example")

  ## expect no error
  expect_error(orderly_run("example", root = path, echo = FALSE), NA)
})


test_that("can't use database in configurations that lack them", {
  path <- prepare_orderly_example("db0")
  p <- file.path(path, "src", "example", "orderly.yml")
  txt <- readLines(p)
  dat <- list(data = list(dat = list(query = "SELECT name, number FROM thing")))
  writeLines(c(txt, yaml::as.yaml(dat)), p)
  expect_error(
    recipe_read(dirname(p), orderly_config(path)),
    "No databases are configured - can't use a 'data' section")
})


test_that("can't use connection in configurations that lack databases", {
  path <- prepare_orderly_example("db0")
  p <- file.path(path, "src", "example", "orderly.yml")
  txt <- readLines(p)
  dat <- list(connection = "con")
  writeLines(c(txt, yaml::as.yaml(dat)), p)
  expect_error(
    recipe_read(dirname(p), orderly_config(path)),
    "No databases are configured - can't use a 'connection' section")
})


test_that("database names are required with more than one db", {
  path <- prepare_orderly_example("db2")
  p <- file.path(path, "src", "example", "orderly.yml")
  dat <- yaml_read(p)
  dat$data$dat1$database <- NULL
  writeLines(yaml::as.yaml(dat), p)
  expect_error(
    recipe_read(dirname(p), orderly_config(path)),
    "More than one database configured; a 'database' field is required for")
})


test_that("connection names are required with more than one db", {
  path <- prepare_orderly_example("db2")
  p <- file.path(path, "src", "connection", "orderly.yml")
  dat <- yaml_read(p)
  dat$connection <- "con"
  writeLines(yaml::as.yaml(dat), p)
  expect_error(
    suppressWarnings(recipe_read(dirname(p), orderly_config(path))),
    "More than one database configured; update 'connection' from 'con'")
})


## This is not *strictly* necessary, but let's roll with it for now
test_that("Can't use database name on old style configuration", {
  path <- prepare_orderly_example("db1")
  p <- file.path(path, "orderly_config.yml")
  dat <- yaml_read(p)
  writeLines(yaml::as.yaml(list(source = dat$database$source1)), p)

  p <- file.path(path, "src", "example", "orderly.yml")
  txt <- readLines(p)
  writeLines(sub("source1", "source", txt), p)

  ## If database present, the new style is validated correctly:
  expect_warning(
    config <- orderly_config(path),
    "Use of 'source' is deprecated")

  res <- recipe_read(dirname(p), config)
  expect_equal(res$data$dat1$database, "source")

  ## If database absent, the new style is imputed correctly
  writeLines(txt[!grepl("^ +database:", txt)], p)
  res <- recipe_read(dirname(p), config)
  expect_equal(res$data$dat1$database, "source")
})


test_that("validate database names", {
  path <- prepare_orderly_example("db2")
  p <- file.path(path, "orderly_config.yml")
  dat <- yaml_read(p)
  names(dat$database) <- c("db1", "db2")
  writeLines(yaml::as.yaml(dat), p)

  cfg <- orderly_config(path)

  expect_error(recipe_read(file.path(path, "src", "example"), cfg),
               "orderly.yml:data:dat1:database must be one of 'db1', 'db2'",
               fixed = TRUE)
  expect_error(recipe_read(file.path(path, "src", "connection"), cfg),
               "orderly.yml:connection:con1 must be one of 'db1', 'db2'",
               fixed = TRUE)
})


test_that("warn old style db", {
  path <- withr::with_options(
    list(orderly.nowarnings = TRUE),
    prepare_orderly_example("olddb"))
  cfg <- withr::with_options(
    list(orderly.nowarnings = TRUE),
    orderly_config(path))

  expect_silent(
    recipe_read(file.path(path, "src", "example"), cfg))
  expect_silent(
    recipe_read(file.path(path, "src", "connection"), cfg))

  file.rename(file.path(path, "orderly_config.yml.new"),
              file.path(path, "orderly_config.yml"))
  cfg <- orderly_config(path)

  expect_warning(
    recipe_read(file.path(path, "src", "example"), cfg),
    "Use of strings for queries is deprecated")
  expect_warning(
    recipe_read(file.path(path, "src", "connection"), cfg),
    "Use of strings for connection: is deprecated")
})


test_that("detect modified artefacts", {
  path <- prepare_orderly_example("demo")
  id <- orderly_run("other", parameters = list(nmin = 0),
                    echo = FALSE, root = path)
  p <- orderly_commit(id, root = path)
  writeLines(character(0), file.path(p, "summary.csv"))

  cfg <- orderly_config(path)
  expect_error(
    recipe_read(file.path(path, "src", "use_dependency"), config = cfg),
    "Validation of dependency 'summary.csv' failed: artefact has been modified")
})
