context("run")

## This is an integration test really.  I need to carve more units out
## of this code still but this will let me know if I'm going along the
## right track and if I'm breaking things!
test_that("run", {
  path <- tempfile()
  on.exit({
    if (exists("con")) {
      ## flush any SQLite connections:
      rm(con)
      gc()
    }
    unlink(path, recursive = TRUE)
  })

  orderly_init(path)
  fake_db(file.path(path, "source.sqlite"))
  file.copy("example_config.yml", file.path(path, "orderly_config.yml"),
            overwrite = TRUE)

  path_example <- file.path(path, "src", "example")

  dir.create(path_example)
  file.copy("example_report.yml", file.path(path_example, "orderly.yml"))
  file.copy("example_script.R", file.path(path_example, "script.R"))

  parameters <- list(minvalue = 0.5)

  ## This probably all rolls together I think?  It's not totally clear
  ## what the run/commit workflow should look like (especially when
  ## developing a draft analysis)
  config <- orderly_config(path)
  info <- recipe_read(file.path(path, "src", "example"), config)
  expect_equal(info$name, basename(path_example))

  p <- recipe_run(info, parameters, config = path)
  expect_true(is_directory(p))
  expect_equal(normalizePath(dirname(p)),
               normalizePath(path_draft(path)))

  expect_true(file.exists(file.path(p, "mygraph.png")))
  expect_true(file.exists(file.path(p, "script.R")))
  expect_true(file.exists(file.path(p, "orderly.yml")))
  expect_true(file.exists(file.path(p, "orderly_run.yml")))
  expect_true(file.exists(file.path(p, "orderly_run.rds")))
  expect_equal(length(dir(p)), 5) # the above are _all_ files produced
  files <- dir(p)
  cmp <- set_names(hash_files(dir(p, full.names = TRUE), FALSE), files)

  ## These files are unmodified
  expect_equal(hash_files(file.path(path_example, "orderly.yml"), FALSE),
               hash_files(file.path(p, "orderly.yml"), FALSE))
  expect_equal(hash_files(file.path(path_example, "script.R"), FALSE),
               hash_files(file.path(p, "script.R"), FALSE))

  ## This needs to look reasonable:
  expect_is(readRDS(file.path(p, "orderly_run.rds")), "sessionInfo")

  expect_identical(readBin(file.path(p, "mygraph.png"), raw(), 8),
                   MAGIC_PNG)

  run <- yaml_read(file.path(p, "orderly_run.yml"))
  expect_equal(run$id, basename(p))
  expect_equal(run$name, info$name)
  expect_identical(run$hash_artefacts,
                   hash_files(file.path(p, "mygraph.png"), FALSE))
  expect_is(run$identifier, "character")
  ## TODO: set to NULL instead?
  expect_identical(run$hash_resources, list())
  expect_identical(run$parameters, parameters)
  expect_is(run$date, "character")
  ## I feel hash_orderly and hash_input have the wrong names here
  expect_identical(run$hash_orderly, info$hash)
  expect_identical(run$hash_input,
                   hash_files(file.path(p, "orderly.yml"), FALSE))

  expect_is(run$hash_data, "character")
  expect_equal(length(run$hash_data), 1)

  con <- orderly_connect(config)
  expect_identical(con$rds$list(), run$hash_data)
  expect_identical(con$csv$list(), run$hash_data)

  ## Confirm that things are OK:
  expect_equal(con$rds$get(run$hash_data),
               con$csv$get(run$hash_data))

  ## Confirm that the *format* is OK too by reading the files manually:
  expect_identical(con$rds$get(run$hash_data),
                   readRDS(con$rds$filename(run$hash_data)))
  expect_identical(con$csv$get(run$hash_data),
                   read_csv(con$csv$filename(run$hash_data)))

  ## Then we commit the results:
  q <- recipe_commit(p, path)
  expect_false(file.exists(p))
  expect_true(file.exists(q))

  ## Everything copied over ok:
  expect_equal(set_names(hash_files(dir(q, full.names = TRUE), FALSE), files),
               cmp)

  expect_equal(DBI::dbListTables(con$destination), "orderly")
  d <- DBI::dbReadTable(con$destination, "orderly")
  expect_true(all(vlapply(d, is.character)))

  expect_equal(d$id, basename(q))
})
