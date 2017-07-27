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
  fake_db(DBI::dbConnect(RSQLite::SQLite(), file.path(path, "source.sqlite")))
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

  p <- recipe_run(info, parameters, config = path, echo = FALSE)
  expect_true(is_directory(p))
  expect_equal(normalizePath(dirname(dirname(p))),
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
  expect_identical(unname(unlist(run$hash_artefacts, use.names = FALSE)),
                   hash_files(file.path(p, "mygraph.png"), FALSE))
  expect_null(run$hash_resources)
  expect_identical(run$parameters, parameters)
  expect_is(run$date, "character")
  ## I feel hash_orderly and hash_input have the wrong names here
  expect_identical(run$hash_orderly, info$hash)
  expect_identical(run$hash_input,
                   hash_files(file.path(p, "orderly.yml"), FALSE))

  expect_is(run$hash_data, "list")
  expect_equal(length(run$hash_data), 1)

  con <- orderly_connect(config)
  expect_identical(con$rds$list(), unlist(run$hash_data, use.names = FALSE))
  expect_identical(con$csv$list(), unlist(run$hash_data, use.names = FALSE))

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
  expect_true(all(vlapply(d[names(d) != "published"], is.character)))
  expect_true(is.numeric(d$published))

  expect_equal(d$id, basename(q))
})

## Same as in read; we generate a report and then break it
test_that("minimal", {
  path <- prepare_minimal()
  on.exit(unlink(path, recursive = TRUE))

  config <- orderly_config(path)
  src <- orderly_db("source", config)
  info <- recipe_read(file.path(path, "src/example"), config)
  data <- recipe_data(src, info, NULL, new.env(parent = .GlobalEnv))
  expect_is(data$dat, "data.frame")

  expect_error(
    recipe_data(src, info, list(a = 1), new.env(parent = .GlobalEnv)),
    "Extra parameters: 'a'")
  expect_error(
    recipe_data(src, info, NULL, NULL),
    "Invalid input for 'dest'")

  workdir <- tempfile()
  dir.create(workdir)
  expect_error(recipe_prepare_workdir(info, workdir),
               "'workdir' must not exist")
  unlink(workdir, recursive = TRUE)

  res <- recipe_run(info, NULL, config = config, echo = FALSE)
  files <- dir(res)
  expect_true(file.exists(file.path(res, "orderly.yml")))
  expect_true(file.exists(file.path(res, "orderly_run.yml")))
  expect_true(file.exists(file.path(res, "orderly_run.rds")))
  expect_true(file.exists(file.path(res, "script.R")))
  expect_true(file.exists(file.path(res, "mygraph.png")))

  recipe_commit(res, config)
})

test_that("orderly_data", {
  path <- prepare_minimal()
  on.exit(unlink(path, recursive = TRUE))

  d <- orderly_data("example", config = path)
  expect_is(d, "list")
  expect_is(d$dat, "data.frame")

  e <- orderly_data("example", config = path, envir = baseenv())
  expect_is(e, "environment")
  expect_identical(parent.env(e), baseenv())

  expect_identical(e$dat, d$dat)
})

test_that("fail to create artefact", {
  path <- prepare_minimal()
  on.exit(unlink(path, recursive = TRUE))
  config <- orderly_config(path)
  writeLines("1 + 1", file.path(path, "src/example/script.R"))
  info <- recipe_read(file.path(path, "src/example"), config)
  expect_error(recipe_run(info, NULL, config = config, echo = FALSE),
               "Script did not produce expected artefacts: mygraph.png")
})

test_that("leave device open", {
  path <- prepare_minimal()
  on.exit(unlink(path, recursive = TRUE))
  config <- orderly_config(path)
  txt <- readLines(file.path(path, "src/example/script.R"))
  writeLines(txt[!grepl("dev.off()", txt, fixed = TRUE)],
             file.path(path, "src/example/script.R"))
  info <- recipe_read(file.path(path, "src/example"), config)
  expect_error(recipe_run(info, NULL, config = config, echo = FALSE),
               "Report left 1 device open")
})

test_that("close too many devices", {
  path <- prepare_minimal()
  png(tempfile())
  n <- length(dev.list())
  on.exit({
    unlink(path, recursive = TRUE)
    if (length(dev.list()) == n) {
      dev.off()
    }
  })

  config <- orderly_config(path)
  txt <- readLines(file.path(path, "src/example/script.R"))
  writeLines(c(txt, "dev.off()"), file.path(path, "src/example/script.R"))
  info <- recipe_read(file.path(path, "src/example"), config)
  expect_error(recipe_run(info, NULL, config = config, echo = FALSE),
               "Report closed 1 more devices than it opened")
})

test_that("included example", {
  path <- prepare_example()
  id <- orderly_run("example", list(cyl = 4), config = path, echo = FALSE)
  p <- orderly_commit(id, config = path)
  expect_true(is_directory(p))
  db <- orderly_db("destination", path)
  dat <- DBI::dbReadTable(db, "orderly")
  expect_equal(dat$description, NA_character_)
  expect_equal(dat$displayname, NA_character_)
})

test_that("included other", {
  path <- prepare_other()
  id <- orderly_run("other", list(nmin = 0), config = path, echo = FALSE)
  p <- orderly_commit(id, config = path)
  info <- recipe_read(file.path(path_src(path), "other"),
                      orderly_config(path))
  db <- orderly_db("destination", path)
  dat <- DBI::dbReadTable(db, "orderly")
  expect_equal(dat$description, info$description)
  expect_equal(dat$displayname, info$displayname)
})

test_that("connection", {
  path <- prepare_minimal()
  on.exit(unlink(path, recursive = TRUE))

  path_example <- file.path(path, "src", "example")
  yml <- file.path(path_example, "orderly.yml")
  txt <- readLines(yml)
  writeLines(c(txt, "connection: con"), yml)

  config <- orderly_config(path)
  info <- recipe_read(path_example, config)
  expect_identical(info$connection, "con")

  data <- orderly_data("example",
                       envir = new.env(parent = .GlobalEnv),
                       config = path)
  expect_is(data$con, "SQLiteConnection")
  expect_is(DBI::dbReadTable(data$con, "data"), "data.frame")
})
