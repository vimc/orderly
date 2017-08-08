context("recipe_read")

test_that("nonexistant file", {
  ## TODO: I think that we should be normalising the path here?
  config <- orderly_config_read_yaml("example_config.yml", ".")
  expect_error(recipe_read(tempfile(), config),
               "Did not find file 'orderly.yml' at path")
  expect_error(recipe_read(tempdir(), config),
               "Did not find file 'orderly.yml' at path")
})

test_that("minimal", {
  path <- prepare_orderly_example("minimal")
  on.exit(unlink(path))

  config <- orderly_config(path)
  path_example <- file.path(path, "src", "example")
  info <- recipe_read(path_example, config)

  expect_is(info$data, "character")
  expect_equal(info$data, c(dat = "SELECT name, number FROM thing"))

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
  writeLines(dat$data$dat, sql)
  write(modifyList(dat, list(data = list(dat = "query.sql"))))

  cmp <- recipe_read(path_example, config)
  expect_equal(attr(cmp$data, "files"), "query.sql")
  attr(cmp$data, "files") <- NULL
  expect_equal(cmp$data, info$data)
  expect_equal(cmp$resources, "query.sql")

  write(modifyList(dat, list(data = list(dat = "foo.sql"))))
  expect_error(recipe_read(path_example, config),
               "data does not exist: foo.sql")

  ## TODO: The formatting of these error messages could be improved
  write(modifyList(dat, list("unknown" = "foo")))
  expect_error(recipe_read(path_example, config),
               "Unknown fields in .*: unknown")

  write(dat[setdiff(names(dat), "script")])
  expect_error(recipe_read(path_example, config),
               "Fields missing from .*: script")

  write(modifyList(dat, list(script = "missing.R")))
  expect_error(recipe_read(path_example, config),
               "script file missing.R does not exist")

  write(modifyList(dat, list(resources = "missing-file")))
  expect_error(recipe_read(path_example, config),
               "Declared resources missing: missing-file")

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
