context("orderly_recipe")

test_that("nonexistant file", {
  path <- prepare_orderly_example("minimal")
  config <- orderly_config$new(path)
  expect_error(orderly_recipe$new("missing", config),
               "Report working directory does not exist")
  dir.create(file.path(path, "src", "missing"), FALSE, TRUE)
  expect_error(orderly_recipe$new("missing", config),
               "Orderly configuration does not exist")
})

test_that("minimal", {
  skip_on_cran_windows()
  path <- prepare_orderly_example("minimal")
  path_example <- file.path(path, "src", "example")
  on.exit(unlink(path))

  config <- orderly_config$new(path)
  info <- orderly_recipe$new("example", config)

  expect_is(info$data$dat$query, "character")
  expect_equal(info$data$dat$query, "SELECT name, number FROM thing")
  expect_equal(info$data$dat$database, "source")

  expect_equal(info$script, "script.R")
  expect_equal(info$path, normalizePath(file.path(path, "src", "example")))

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

  cmp <- orderly_recipe$new("example", config)
  expect_equal(cmp$data$dat$query_file, "query.sql")
  expect_equal(cmp$resources, "query.sql")
  expect_equal(cmp$data$dat[c("query", "database")],
               info$data$dat[c("query", "database")])

  write(modifyList(dat, list(data = list(dat = list(query = "foo.sql")))))
  expect_error(orderly_recipe$new("example", config),
               "SQL file does not exist: 'foo.sql'")

  ## TODO: The formatting of these error messages could be improved
  write(modifyList(dat, list("unknown" = "foo")))
  expect_error(orderly_recipe$new("example", config),
               "Unknown fields in .*: unknown")

  write(dat[setdiff(names(dat), "script")])
  expect_error(orderly_recipe$new("example", config),
               "Fields missing from .*: script")

  write(modifyList(dat, list(script = "missing.R")))
  expect_error(orderly_recipe$new("example", config),
               "Script file does not exist: 'missing.R'")

  write(modifyList(dat, list(resources = "missing-file")))
  expect_error(orderly_recipe$new("example", config),
               "Resource file does not exist: 'missing-file'")

  write(modifyList(dat, list(resources = "..")))
  expect_error(orderly_recipe$new("example", config),
               "Declared resources not in right place: \\.\\.")

  write(modifyList(dat, list(resources = "query.sql")))
  cmp <- orderly_recipe$new("example", config)
  expect_equal(cmp$resources, "query.sql")

  write(modifyList(dat, list(artefacts = character(0))))
  expect_error(orderly_recipe$new("example", config),
               "At least one artefact required")

  write(modifyList(dat, list(packages = 10)))
  expect_error(orderly_recipe$new("example", config),
               "orderly.yml:packages' must be character")
})


test_that("ill formed artefacts", {
  path <- prepare_orderly_example("minimal")
  on.exit(unlink(path))
  config <- orderly_config$new(path)
  path_example <- file.path(path, "src", "example")
  yml <- file.path(path_example, "orderly.yml")
  dat <- yaml_read(yml)

  expect_silent(orderly_recipe$new("example", config))

  dat$artefacts <- c(dat$artefacts,
                     list(data = list(filename = "foo", description = "bar")))
  writeLines(yaml::as.yaml(dat), yml)
  expect_error(suppressMessages(orderly_recipe$new("example", config)),
               "Expected an ordered map")
})

test_that("unknown artefact type", {
  path <- prepare_orderly_example("minimal")
  on.exit(unlink(path))
  config <- orderly_config$new(path)
  path_example <- file.path(path, "src", "example")
  yml <- file.path(path_example, "orderly.yml")
  dat <- yaml_read(yml)

  expect_silent(orderly_recipe$new("example", config))

  dat$artefacts <- list(unknown = list(filenames = "foo", description = "bar"))
  writeLines(yaml::as.yaml(dat), yml)
  expect_error(suppressMessages(orderly_recipe$new("example", config)),
               "Unknown artefact type: 'unknown'")
})

test_that("duplicate artefact filenames; within artefact", {
  path <- prepare_orderly_example("minimal")
  on.exit(unlink(path))
  config <- orderly_config$new(path)
  path_example <- file.path(path, "src", "example")
  yml <- file.path(path_example, "orderly.yml")
  dat <- yaml_read(yml)
  dat$artefacts[[1]]$filenames <- c("mygraph.png", "mygraph.png")
  writeLines(yaml::as.yaml(dat), yml)
  expect_error(orderly_recipe$new("example", config),
               "Duplicate artefact filenames are not allowed: 'mygraph.png'")
})


test_that("duplicate artefact filenames; between artefacts", {
  path <- prepare_orderly_example("minimal")
  on.exit(unlink(path))
  config <- orderly_config$new(path)
  path_example <- file.path(path, "src", "example")
  yml <- file.path(path_example, "orderly.yml")
  dat <- yaml_read(yml)
  dat$artefacts <- list(dat$artefacts, dat$artefacts)
  writeLines(yaml::as.yaml(dat), yml)
  expect_error(orderly_recipe$new("example", config),
               "Duplicate artefact filenames are not allowed: 'mygraph.png'")
})


test_that("resource case matters", {
  path <- prepare_orderly_example("minimal")
  file.rename(file.path(path, "src", "example", "script.R"),
              file.path(path, "src", "example", "script.r"))

  config <- orderly_config_get(path, FALSE)
  expect_error(orderly_recipe$new("example", config),
               "Script file does not exist: 'script.R'")
})

test_that("dependencies must be scalar", {
  path <- prepare_orderly_example("depends", testing = TRUE)
  id <- orderly_run("example", root = path, echo = FALSE)

  filename <- file.path(path, "src", "depend", "orderly.yml")
  dat <- yaml_read(filename)
  dat$depends$example$use[["previous.rds"]] <- character(0)
  yaml_write(dat, filename)

  expect_error(orderly_recipe$new("depend", orderly_config$new(path)),
               "depends:example:use must all be scalar character")
})


test_that("dependencies must exist", {
  path <- prepare_orderly_example("depends", testing = TRUE)
  id <- orderly_run("example", root = path, echo = FALSE)

  filename <- file.path(path, "src", "depend", "orderly.yml")
  dat <- yaml_read(filename)
  dat$depends$example$use[["previous.rds"]] <- "unknown.file"
  yaml_write(dat, filename)

  expect_error(orderly_run("depend", root = path, echo = FALSE,
                           use_draft = TRUE),
               "Did not find file 'unknown.file' at")
})


test_that("dependencies draft, new interface", {
  path <- prepare_orderly_example("depends", testing = TRUE)
  id1 <- orderly_run("example", root = path, echo = FALSE)
  id2 <- orderly_run("example", root = path, echo = FALSE)
  orderly_commit(id1, root = path)

  filename <- file.path(path, "src", "depend", "orderly.yml")
  dat <- yaml_read(filename)
  dat$depends$example$draft <- NULL
  yaml_write(dat, filename)

  f <- function(id) {
    readRDS(path_orderly_run_rds(file.path(path, "draft", "depend", id)))
  }

  id3 <- orderly_run("depend", root = path, use_draft = TRUE, echo = FALSE)
  id4 <- orderly_run("depend", root = path, use_draft = "always", echo = FALSE)
  id5 <- orderly_run("depend", root = path, use_draft = "newer", echo = FALSE)
  expect_equal(f(id3)$meta$depends$id, id2)
  expect_equal(f(id4)$meta$depends$id, id2)
  expect_equal(f(id5)$meta$depends$id, id2)

  id6 <- orderly_run("depend", root = path, use_draft = FALSE, echo = FALSE)
  id7 <- orderly_run("depend", root = path, use_draft = "never", echo = FALSE)
  expect_equal(f(id6)$meta$depends$id, id1)
  expect_equal(f(id7)$meta$depends$id, id1)
})


test_that("use_draft = newer ignores fails drafts", {
  path <- prepare_orderly_example("depends", testing = TRUE)
  id1 <- orderly_run("example", root = path, echo = FALSE)
  id2 <- orderly_run("example", root = path, echo = FALSE)
  id3 <- orderly_run("example", root = path, echo = FALSE)
  orderly_commit(id1, root = path)
  unlink(file.path(path, "draft", "example", id3, "orderly_run.rds"))

  filename <- file.path(path, "src", "depend", "orderly.yml")
  dat <- yaml_read(filename)
  dat$depends$example$draft <- NULL
  yaml_write(dat, filename)

  config <- orderly_config$new(path)

  info <- orderly_recipe$new("depend", config)
  info$resolve_dependencies(use_draft = "newer")

  expect_equal(basename(info$depends$path), id2)
  unlink(file.path(path, "draft", "example", id2, "orderly_run.rds"))

  info <- orderly_recipe$new("depend", config)
  info$resolve_dependencies(use_draft = "newer")
  expect_equal(basename(info$depends$path), id1)
})


test_that("dependencies draft, new interface, throws sensible errors", {
  path <- prepare_orderly_example("depends", testing = TRUE)

  filename <- file.path(path, "src", "depend", "orderly.yml")
  dat <- yaml_read(filename)
  dat$depends$example$draft <- NULL
  yaml_write(dat, filename)

  expect_error(
    orderly_run("depend", root = path, use_draft = TRUE, echo = FALSE),
    "Did not find draft report example:latest")
  expect_error(
    orderly_run("depend", root = path, use_draft = "always", echo = FALSE),
    "Did not find draft report example:latest")
  expect_error(
    orderly_run("depend", root = path, use_draft = "newer", echo = FALSE),
    "Did not find draft or archive report example:latest")
  expect_error(
    orderly_run("depend", root = path, use_draft = FALSE, echo = FALSE),
    "Did not find archive report example:latest")
  expect_error(
    orderly_run("depend", root = path, use_draft = "never", echo = FALSE),
    "Did not find archive report example:latest")
})


test_that("Using draft within a dependency is now a warning", {
  path <- prepare_orderly_example("depends", testing = TRUE)
  id1 <- orderly_run("example", root = path, echo = FALSE)

  filename <- file.path(path, "src", "depend", "orderly.yml")
  dat <- yaml_read(filename)
  dat$depends$example$draft <- TRUE
  yaml_write(dat, filename)

  expect_warning(
    id2 <- orderly_run("depend", root = path, echo = FALSE),
    "Using 'draft:' within an ordery.yml is deprecated")
})


test_that("data field is optional", {
  path <- prepare_orderly_example("nodata")
  report_path <- file.path(path, "src", "example")

  ## expect no error
  expect_error(orderly_run("example", root = path, echo = FALSE), NA)
})


test_that("can't use database in configurations that lack them", {
  path <- prepare_orderly_example("db0", testing = TRUE)
  p <- file.path(path, "src", "example", "orderly.yml")
  txt <- readLines(p)
  dat <- list(data = list(dat = list(query = "SELECT name, number FROM thing")))
  writeLines(c(txt, yaml::as.yaml(dat)), p)
  expect_error(
    orderly_recipe$new("example", orderly_config$new(path)),
    "No databases are configured - can't use a 'data' section")
})


test_that("can't use connection in configurations that lack databases", {
  path <- prepare_orderly_example("db0", testing = TRUE)
  p <- file.path(path, "src", "example", "orderly.yml")
  txt <- readLines(p)
  dat <- list(connection = "con")
  writeLines(c(txt, yaml::as.yaml(dat)), p)
  expect_error(
    suppressWarnings(orderly_recipe$new("example", orderly_config$new(path))),
    "No databases are configured - can't use a 'connection' section")
})


test_that("database names are required with more than one db", {
  path <- prepare_orderly_example("db2", testing = TRUE)
  p <- file.path(path, "src", "example", "orderly.yml")
  dat <- yaml_read(p)
  dat$data$dat1$database <- NULL
  writeLines(yaml::as.yaml(dat), p)
  expect_error(
    orderly_recipe$new("example", orderly_config$new(path)),
    "More than one database configured; a 'database' field is required for")
})


test_that("connection names are required with more than one db", {
  path <- prepare_orderly_example("db2", testing = TRUE)
  p <- file.path(path, "src", "connection", "orderly.yml")
  dat <- yaml_read(p)
  dat$connection <- "con"
  writeLines(yaml::as.yaml(dat), p)
  config <- orderly_config$new(path)
  expect_error(
    suppressWarnings(orderly_recipe$new("connection", config)),
    "More than one database configured; update 'connection' from 'con'")
})


## This is not *strictly* necessary, but let's roll with it for now
test_that("Can't use database name on old style configuration", {
  path <- prepare_orderly_example("db1", testing = TRUE)
  p <- file.path(path, "orderly_config.yml")
  dat <- yaml_read(p)
  writeLines(yaml::as.yaml(list(source = dat$database$source1)), p)

  p <- file.path(path, "src", "example", "orderly.yml")
  txt <- readLines(p)
  writeLines(sub("source1", "source", txt), p)

  ## If database present, the new style is validated correctly:
  expect_warning(
    config <- orderly_config$new(path),
    "Use of 'source' is deprecated")

  res <- orderly_recipe$new("example", config)
  expect_equal(res$data$dat1$database, "source")

  ## If database absent, the new style is imputed correctly
  writeLines(txt[!grepl("^ +database:", txt)], p)
  res <- orderly_recipe$new("example", config)
  expect_equal(res$data$dat1$database, "source")
})


test_that("validate database names", {
  path <- prepare_orderly_example("db2", testing = TRUE)
  p <- file.path(path, "orderly_config.yml")
  dat <- yaml_read(p)
  names(dat$database) <- c("db1", "db2")
  writeLines(yaml::as.yaml(dat), p)

  cfg <- orderly_config$new(path)

  expect_error(orderly_recipe$new("example", cfg),
               "orderly.yml:data:dat1:database must be one of 'db1', 'db2'",
               fixed = TRUE)
  expect_error(orderly_recipe$new("connection", cfg),
               "orderly.yml:connection:con1 must be one of 'db1', 'db2'",
               fixed = TRUE)
})


test_that("warn old style db", {
  path <- withr::with_options(
    list(orderly.nowarnings = TRUE),
    prepare_orderly_example("olddb", testing = TRUE))
  cfg <- withr::with_options(
    list(orderly.nowarnings = TRUE),
    orderly_config$new(path))

  expect_warning(
    orderly_recipe$new("example", cfg),
    "Use of strings for queries is deprecated")
  expect_warning(
    orderly_recipe$new("connection", cfg),
    "Use of strings for connection: is deprecated")

  file.rename(file.path(path, "orderly_config.yml.new"),
              file.path(path, "orderly_config.yml"))
  expect_warning(
    cfg <- orderly_config$new(path),
    "Please move your database arguments")

  expect_warning(
    orderly_recipe$new("example", cfg),
    "Use of strings for queries is deprecated")
  expect_warning(
    orderly_recipe$new("connection", cfg),
    "Use of strings for connection: is deprecated")
})


test_that("detect modified artefacts", {
  skip_on_cran_windows()
  path <- prepare_orderly_example("demo")
  id <- orderly_run("other", parameters = list(nmin = 0),
                    echo = FALSE, root = path)
  p <- orderly_commit(id, root = path)
  writeLines(character(0), file.path(p, "summary.csv"))

  cfg <- orderly_config$new(path)
  expect_error(
    orderly_recipe$new("use_dependency", cfg)$resolve_dependencies(),
    paste("Validation of dependency 'summary.csv' (other/latest) failed:",
          "artefact has been modified"), fixed = TRUE)
})


test_that("modified artefacts when more than one used", {
  skip_on_cran_windows()
  path <- prepare_orderly_example("demo")

  id <- orderly_run("multifile-artefact", echo = FALSE, root = path)
  p <- orderly_commit(id, root = path)

  ## Modify artefact
  writeLines(character(0), file.path(p, "mygraph.pdf"))

  path_yml <- file.path(path, "src", "use_dependency", "orderly.yml")
  yml <- yaml_read(path_yml)
  yml$depends <- list(
    "multifile-artefact" = list(id = "latest",
                                use = list(mygraph.png = "mygraph.png",
                                           mygraph.pdf = "mygraph.pdf")))
  yaml_write(yml, path_yml)

  cfg <- orderly_config$new(path)
  expect_error(
    orderly_recipe$new("use_dependency", cfg)$resolve_dependencies(),
    paste("Validation of dependency 'mygraph.pdf' (multifile-artefact/latest)",
          "failed: artefact has been modified"), fixed = TRUE)
})


test_that("sources and resources are exclusive", {
  path <- orderly_example("demo")

  p <- file.path(path, "src", "other", "orderly.yml")
  d <- yaml_read(p)
  d$resources <- d$sources
  yaml_write(d, p)

  config <- orderly_config$new(path)
  expect_error(
    orderly_recipe$new("other", config),
    "Do not list source files \\(sources\\) as resources:\\s+- functions\\.R")
})


test_that("trailing slash on resource directory", {
  skip_on_cran_windows()
  path <- prepare_orderly_example("demo")
  ## in report directory create a file called README.md
  report_path <- file.path(path, "src", "use_resource")
  ## rewrite yml to include extra readme file
  yml_path <- file.path(report_path, "orderly.yml")
  yml <- c("data:",
           "  dat:",
           "    query: SELECT name, number FROM thing",
           "script: script.R",
           "resources:",
           "  - meta/",
           "artefacts:",
           "  staticgraph:",
           "    description: A graph of things",
           "    filenames: mygraph.png",
           "author: Dr Serious",
           "requester: ACME"
           )
  writeLines(yml, file.path(yml_path))
  id <- orderly_run("use_resource", root = path, echo = FALSE)
  p <- file.path(path, "draft", "use_resource", id)

  # make sure the directory has been copied across
  expect_true(file.exists(file.path(p, "meta")))
  orderly_commit(id, root = path)
  con <- orderly_db("destination", root = path)
  on.exit(DBI::dbDisconnect(con))
  dat <- DBI::dbReadTable(con, "file_input")
  # make sure the resource filename does not contain a double slash //
  expect_true("meta/data.csv" %in% dat$filename)
})


test_that("old style global resources deprecated", {
  path <- prepare_orderly_example("global", testing = TRUE)
  path_example <- file.path(path, "src", "example")
  path_yaml <- file.path(path_example, "orderly.yml")
  config_lines <- readLines(path_yaml)
  config_lines[[11]] <- "  data.csv"
  writeLines(config_lines, path_yaml)

  expect_warning(
    res <- orderly_recipe$new("example", orderly_config$new(path)),
    "Use of strings for global_resources: is deprecated")
  expect_equal(
    res$global_resources,
    c(data.csv = "data.csv"))
})


test_that("read parameters", {
  path <- prepare_orderly_example("parameters", testing = TRUE)
  path_example <- file.path(path, "src", "example")
  info <- orderly_recipe$new("example", orderly_config$new(path))
  expect_equal(info$parameters,
               list(a = NULL, b = NULL, c = list(default = 1)))
})


test_that("read old-style parameters", {
  path <- prepare_orderly_example("parameters", testing = TRUE)
  path_example <- file.path(path, "src", "example")
  path_orderly <- file.path(path_example, "orderly.yml")
  dat <- yaml_read(path_orderly)
  dat$parameters <- list("a", "b", "c")
  yaml_write(dat, path_orderly)
  expect_warning(
    info <- orderly_recipe$new("example", orderly_config$new(path)),
    "Use of strings for parameters: is deprecated")
  expect_equal(info$parameters,
               list(a = NULL, b = NULL, c = NULL))
})


test_that("validate parameters", {
  path <- prepare_orderly_example("parameters", testing = TRUE)
  path_example <- file.path(path, "src", "example")
  path_orderly <- file.path(path_example, "orderly.yml")
  config <- orderly_config$new(path)

  dat <- yaml_read(path_orderly)
  dat$parameters <- list(a = list(something = 1))
  yaml_write(dat, path_orderly)

  expect_error(
    orderly_recipe$new("example", config),
    "Unknown fields in .*orderly.yml:parameters:a: something")
})


test_that("Can resolve dependencies remotely", {
  dat <- prepare_orderly_remote_example()
  config <- orderly_config$new(dat$path_local)

  info <- orderly_recipe$new("depend", config)

  expect_equal(nrow(orderly_list_archive(dat$path_local)), 0)
  expect_error(
    info$resolve_dependencies(FALSE, NULL, NULL),
    "Did not find archive report example:latest")

  expect_error(
    info$resolve_dependencies(TRUE, NULL, "default"),
    "Can't use 'use_draft' with remote")

  info$resolve_dependencies(FALSE, NULL, "default")
  expect_equal(nrow(orderly_list_archive(dat$path_local)), 1)

  cmp <- orderly_recipe$new("depend", config)
  expect_equal(
    cmp$resolve_dependencies(FALSE, NULL, NULL)$depends,
    info$depends)
})


test_that("resolve_dependencies_remote", {
  dat <- prepare_orderly_remote_example()
  config <- orderly_config$new(dat$path_local)
  remote <- get_remote("default", config)

  p <- file.path(normalizePath(dat$path_local), "archive", "example")

  expect_equal(
    resolve_dependencies_remote("latest", "example", config, remote),
    list(path = file.path(p, dat$id2), is_latest = TRUE))
  expect_true(file.exists(file.path(p, dat$id2)))

  ## With explicit id:
  expect_equal(
    resolve_dependencies_remote(dat$id2, "example", config, remote),
    list(path = file.path(p, dat$id2), is_latest = TRUE))
  expect_equal(
    resolve_dependencies_remote(dat$id1, "example", config, remote),
    list(path = file.path(p, dat$id1), is_latest = FALSE))

  ## And an error
  expect_error(
    resolve_dependencies_remote(new_report_id(), "example", config, remote),
    "Did not find report 'example:.+' on remote 'default'")
})


test_that("Can't use queries when resolving dependencies remotely", {
  dat <- prepare_orderly_remote_example()
  config <- orderly_config$new(dat$path_local)
  remote <- get_remote("default", config)
  expect_error(
    resolve_dependencies_remote("latest()", "example", config, remote),
    "Can't (yet) use query dependencies with remotes",
    fixed = TRUE)
})


test_that("friendly error message if artefacts are incorrectly given", {
  path <- prepare_orderly_example("minimal")
  p <- file.path(path, "src", "example", "orderly.yml")
  dat <- yaml_read(p)
  dat$artefacts <- "mygraph.png"
  yaml_write(dat, p)

  config <- orderly_config$new(path)
  err <- expect_error(
    orderly_recipe$new("example", config),
    "Your artefacts are misformatted.  You must provide")

  ## check that the suggested fix is good:
  fix <- strsplit(err$message, "\n")[[1]][4:9]
  txt <- c(readLines(p)[1:4], fix)
  writeLines(txt, p)
  res <- orderly_recipe$new("example", config)$artefacts
  expect_equal(res[, "filenames"],
               list(filenames = "mygraph.png"))
  expect_equal(res[, "description"],
               list(description = "These are data for x, y, z"))
  expect_equal(res[, "format"],
               list(format = "data"))
})


test_that("Read partial orderly.yml", {
  path <- prepare_orderly_example("minimal")
  p <- orderly_new("partial", root = path, quiet = TRUE)
  config <- orderly_config$new(path)
  expect_message(
    orderly_recipe$new("partial", config, develop = TRUE),
    "At least one artefact required")
  expect_message(
    orderly_recipe$new("partial", config, develop = TRUE),
    "orderly.yml:script' must be a scalar", fixed = TRUE)
})


test_that("Read completely empty orderly.yml", {
  path <- prepare_orderly_example("minimal")
  p <- orderly_new("partial", root = path, quiet = TRUE)
  file.create(file.path(p, "orderly.yml")) # truncates file
  config <- orderly_config$new(path)
  expect_message(
    orderly_recipe$new("partial", config, develop = TRUE),
    "Fields missing from .*: script, artefacts")
})


test_that("Validate report tag", {
  root <- prepare_orderly_example("minimal")
  append_lines(c("tags:", "  - tag1", "  - tag2"),
               file.path(root, "orderly_config.yml"))
  config <- orderly_config$new(root)
  path <- file.path(root, "src", "example")
  path_config <- file.path(path, "orderly.yml")
  txt <- readLines(path_config)

  expect_null(orderly_recipe$new("example", config)$tags)

  writeLines(c(txt, "tags: tag1"), path_config)
  expect_equal(orderly_recipe$new("example", config)$tags, "tag1")

  writeLines(c(txt, "tags:", "- tag1", "- tag2"), path_config)
  expect_equal(orderly_recipe$new("example", config)$tags, c("tag1", "tag2"))

  writeLines(c(txt, "tags:", "- tag1", "- tag2", "- tag3"), path_config)
  expect_error(orderly_recipe$new("example", config),
               "Unknown tag: 'tag3'")

  writeLines(c(txt, "tags:", "- tag1", "- tag2", "- tag1"), path_config)
  expect_error(orderly_recipe$new("example", config),
               "Duplicated tag: 'tag1'")
})


test_that("Better error message where tags not enabled", {
  root <- prepare_orderly_example("minimal")
  config <- orderly_config$new(root)
  path <- file.path(root, "src", "example")
  path_config <- file.path(path, "orderly.yml")
  txt <- readLines(path_config)

  writeLines(c(txt, "tags: tag1"), path_config)
  expect_error(
    orderly_recipe$new("example", config),
    "Tags are not supported; please edit orderly_config.yml to enable")
})

test_that("read secrets", {
  config <- list(root = tempfile(),
                 vault = list(addr = "https://example.com/vault"))
  filename <- "orderly.yml"

  expect_null(recipe_validate_secrets(NULL, NULL, filename))
  expect_error(
    recipe_validate_secrets(list(a = "path"), NULL, filename),
    "Vault not enabled in orderly_config.yml")

  expect_equal(
    recipe_validate_secrets(list(a = "first:one"), config, filename),
    list(a = "VAULT:first:one"))
  expect_equal(
    recipe_validate_secrets(list(a = "first:one", b = "second:other"),
                              config, filename),
    list(a = "VAULT:first:one",
         b = "VAULT:second:other"))

  expect_error(
    recipe_validate_secrets(list("path", "other"), config, filename),
    "'orderly.yml:secrets' must be named")
  expect_error(
    recipe_validate_secrets(list(a = "path", a = "other"), config, filename),
    "'orderly.yml:secrets' must have unique names")
  expect_error(
    recipe_validate_secrets(list(a = "path", b = 2), config, filename),
    "'orderly.yml:secrets:b' must be character")
  expect_error(
    recipe_validate_secrets(list(a = "path"), config, filename),
    "Misformatted secret path: 'path' for 'a'")
  expect_error(
    recipe_validate_secrets(list(a = "path", b = "other:field:what"),
                              config, filename),
    "Misformatted secret path: 'path' for 'a', 'other:field:what' for 'b'")
  expect_error(
    recipe_validate_secrets(list(a = "first:path", b = "other:field:what"),
                              config, filename),
    "Misformatted secret path: 'other:field:what' for 'b'")
})

test_that("can read env vars from orderly yml", {
  filename <- "orderly.yml"
  config <- NULL

  expect_null(recipe_validate_environment(NULL, config, filename))
  expect_error(
    recipe_validate_environment(list("ENV", "VAR"), config, filename),
    "'orderly.yml:environment' must be named")
  expect_error(
    recipe_validate_environment(list(a = "ENV", a = "VAR"), config, filename),
    "'orderly.yml:environment' must have unique names")
  expect_error(
    recipe_validate_environment(list(a = "ENV", b = 2), config, filename),
    "'orderly.yml:environment:b' must be character")
  expect_error(
    recipe_validate_environment(list(a = list("ENV", "VAR")), config, filename),
    "'orderly.yml:environment:a' must be a scalar")

  env_vars <- list(a = "ENV", b = "VAR")
  expect_equal(recipe_validate_environment(env_vars), env_vars)
})


test_that("Query interface", {
  dat <- prepare_orderly_query_example(TRUE)
  root <- dat$root
  ids <- dat$ids

  config <- orderly_config$new(root)

  p <- file.path(root, "src", "use_dependency", "orderly.yml")
  txt <- readLines(p)
  writeLines(
    sub("latest", "latest(parameter:nmin < 0.25)", txt, fixed = TRUE),
    p)

  res <- resolve_dependencies_local("latest(parameter:nmin < 0.25)", "other",
                                    config, NULL, TRUE)
  expect_equal(res$path, file.path(config$root, "draft", "other", ids[[2]]))
  expect_true(res$is_latest)

  orderly_commit(ids[[2]], root = root)

  res <- resolve_dependencies_local("latest(parameter:nmin < 0.25)", "other",
                                    config, NULL, TRUE)
  expect_equal(res$path, file.path(config$root, "draft", "other", ids[[1]]))
  expect_true(res$is_latest)

  res <- resolve_dependencies_local("latest(parameter:nmin < 0.25)", "other",
                                    config, NULL, "newer")
  expect_equal(res$path, file.path(config$root, "archive", "other", ids[[2]]))
  expect_true(res$is_latest)


  res <- resolve_dependencies_local("latest(parameter:nmin > 0.25)", "other",
                                    config, NULL, "newer")
  expect_equal(res$path, file.path(config$root, "draft", "other", ids[[3]]))
  expect_true(res$is_latest)
})


test_that("pass parameters through query interface", {
  dat <- prepare_orderly_query_example()
  root <- dat$root
  ids <- dat$ids

  config <- orderly_config$new(root)

  p <- file.path(root, "src", "use_dependency", "orderly.yml")
  txt <- readLines(p)
  writeLines(
    sub("latest", "latest(parameter:nmin < 0.25)", txt, fixed = TRUE),
    p)

  res <- resolve_dependencies_local("latest(parameter:nmin < p)", "other",
                                    config, list(p = 0.25), FALSE)
  expect_equal(res$path, file.path(config$root, "archive", "other", ids[[2]]))
  expect_true(res$is_latest)

  expect_equal(
    resolve_dependencies_local("latest(parameter:nmin < nmin)", "other",
                               config, list(nmin = 0.25), FALSE),
    res)
})


test_that("Errors are thrown if required missing fields are not present", {
  path <- prepare_orderly_example("demo")

  config <- orderly_config$new(path)

  path_example <- file.path(path, "src", "minimal")
  yml_path <- file.path(path_example, "orderly.yml")
  dat <- yaml_read(yml_path)
  dat$requester <- NULL
  yaml_write(dat, yml_path)

  expect_error(
    orderly_recipe$new("minimal", config),
    "Fields missing from orderly.yml: 'requester'")
})


test_that("Errors are thrown if required missing fields are wrong type", {
  path <- prepare_orderly_example("demo")

  config <- orderly_config$new(path)

  path_example <- file.path(path, "src", "minimal")
  yml_path <- file.path(path_example, "orderly.yml")
  dat <- yaml_read(yml_path)
  dat$requester <- 1
  yaml_write(dat, yml_path)

  expect_error(
    orderly_recipe$new("minimal", config),
    "'orderly.yml:requester' must be character")
})


test_that("Cope with missing optional fields", {
  path <- prepare_orderly_example("demo")

  config <- orderly_config$new(path)

  path_example <- file.path(path, "src", "minimal")
  yml_path <- file.path(path_example, "orderly.yml")
  dat <- yaml_read(yml_path)
  dat$comment <- NULL
  yaml_write(dat, yml_path)

  fields <- orderly_recipe$new("minimal", config)$fields
  expect_mapequal(
    fields,
    list(requester = "Funder McFunderface",
         author = "Researcher McResearcherface",
         comment = NA_character_))
})


test_that("read changelog", {
  skip_on_cran_windows()
  path <- prepare_orderly_example("changelog", testing = TRUE)

  tmp <- tempfile()
  path_example <- file.path(path, "src", "example")
  path_cl <- path_changelog_txt(path_example)

  info <- orderly_recipe$new("example", orderly_config$new(path))
  expect_null(info$changelog)

  writeLines(c("[label1]", "value1"), path_cl)
  info <- orderly_recipe$new("example", orderly_config$new(path))
  expect_equal(
    info$changelog,
    data_frame(label = "label1", value = "value1", from_file = TRUE))
})


test_that("readme detection", {
  path <- prepare_orderly_example("minimal")
  config <- orderly_config$new(path)
  expect_null(orderly_recipe$new("example", config)$readme)

  file.create(file.path(path, "src", "example", "README.md"))
  expect_equal(
    orderly_recipe$new("example", config)$readme,
    c("README.md" = "README.md"))

  dir.create(file.path(path, "src", "example", "subdir"))
  file.create(file.path(path, "src", "example", "subdir", "Readme.MD"))
  expect_mapequal(
    orderly_recipe$new("example", config)$readme,
    c("README.md" = "README.md", "subdir/README.md" = "subdir/Readme.MD"))

  file.create(file.path(path, "src", "example", "subdir", "readme"))
  expect_mapequal(
    orderly_recipe$new("example", config)$readme,
    c("README.md" = "README.md",
      "subdir/README.md" = "subdir/Readme.MD",
      "subdir/README" = "subdir/readme"))

  file.create(file.path(path, "src", "example", "subdir", "please_README.md"))
  expect_mapequal(
    orderly_recipe$new("example", config)$readme,
    c("README.md" = "README.md",
      "subdir/README.md" = "subdir/Readme.MD",
      "subdir/README" = "subdir/readme"))
})


test_that("readme listed as a resource", {
  path <- prepare_orderly_example("minimal")
  report_path <- file.path(path, "src", "example")
  ## in report directory create a file called README.md
  path_example <- file.path(path, "src", "example")
  file.create(file.path(report_path, "README.md"))

  yml_path <- file.path(path_example, "orderly.yml")
  minimal_yml <- readLines(yml_path)
  extended_yml <- c(minimal_yml,
                    "resources:",
                    "  README.md")
  writeLines(extended_yml, yml_path)

  file.create(file.path(path_example, "README.md"))
  expect_message(
    orderly_recipe$new("example", orderly_config$new(path)),
    "README.md should not be listed as a resource")
})


test_that("list README.md as artefact",  {
  path <- prepare_orderly_example("minimal")
  report_path <- file.path(path, "src", "example")
  ## in report directory create a file called README.md
  path_example <- file.path(path, "src", "example")
  file.create(file.path(report_path, "README.md"))

  yml_path <- file.path(path_example, "orderly.yml")
  ## rewrite the yaml to include README.md as a resource
  yml <- c("data:",
           "  dat:",
           "    query: SELECT name, number FROM thing",
           "script: script.R",
           "artefacts:",
           "  - staticgraph:",
           "      description: A graph of things",
           "      filenames: mygraph.png",
           "  - data:",
           "      description: a readme file",
           "      filenames: README.md"
           )
  writeLines(yml, file.path(yml_path))

  expect_error(
    orderly_recipe$new("example", orderly_config$new(path)),
    "README.md should not be listed as an artefact")
})
