context("remote")


test_that("defaults: null", {
  path <- prepare_orderly_example("minimal")
  expect_null(orderly_default_remote_set(NULL, path))
  expect_error(orderly_default_remote_get(path, FALSE),
               "default remote has not been set yet")
  expect_error(get_remote(NULL, path),
               "default remote has not been set yet")
})


test_that("get_remote type failure", {
  dat <- prepare_orderly_remote_example()
  expect_error(get_remote(1, dat$config),
               "Unknown remote type")
  expect_error(get_remote("extra", dat$config),
               "Unknown remote 'extra'")
})


test_that("orderly_pull_archive with wrong version", {
  dat <- prepare_orderly_remote_example()

  expect_error(
    orderly_pull_archive("example", new_report_id(), root = dat$config,
                         remote = dat$remote),
    paste0("Version '.+?' not found at '.+?': valid versions are:.+",
           dat$id1))
})


test_that("pull dependencies", {
  dat <- prepare_orderly_remote_example()

  expect_log_message(
    orderly_pull_dependencies("depend", root = dat$config,
                              remote = dat$remote),
    "\\[ pull\\s+ \\]  example:")
  expect_equal(orderly_list_archive(dat$config),
               data_frame(name = "example", id = dat$id2))

  ## and update
  id3 <- orderly_run("example", root = dat$path_remote, echo = FALSE)
  orderly_commit(id3, root = dat$path_remote)
  expect_log_message(
    orderly_pull_dependencies("depend", root = dat$config,
                              remote = dat$remote),
    "\\[ pull\\s+ \\]  example:")
  expect_equal(orderly_list_archive(dat$config),
               data_frame(name = "example", id = c(dat$id2, id3)))
})


test_that("pull_dependencies counts dependencies", {
  dat <- prepare_orderly_remote_example()

  expect_log_message(
    orderly_pull_dependencies("example", root = dat$config,
                              remote = dat$remote),
    "\\[ depends\\s+ \\]  example has 0 dependencies")

  id <- orderly_run("example", root = dat$path_remote, echo = FALSE)
  orderly_commit(id, root = dat$path_remote)
  expect_log_message(
    orderly_pull_dependencies("depend", root = dat$config,
                              remote = dat$remote),
    "\\[ depends\\s+ \\]  depend has 1 dependency")
})


## These need dealing with properly, but check that they trigger
## correctly here:
test_that("pull from old remote", {
  oo <- options(orderly.nowarnings = TRUE)
  on.exit(options(oo))
  path_local <- prepare_orderly_example("demo")
  path_remote <- unpack_reference("0.6.0")

  ## In order to make this work we do need to update the data table.
  ## This will stop being a problem shortly.
  ##
  ## Once we get a further migration we should disable importing of
  ## all files prior to archive version 0.6.8 because of this problem.
  db_local <- orderly_db("destination", root = path_local)
  db_remote <- orderly_db("destination", root = path_remote, validate = FALSE)
  tbl_data <- DBI::dbReadTable(db_remote, "data")
  DBI::dbWriteTable(db_local, "data", tbl_data, append = TRUE)
  DBI::dbDisconnect(db_local)
  DBI::dbDisconnect(db_remote)

  expect_log_message(
    orderly_pull_archive("minimal", root = path_local, remote = path_remote),
    "^\\[ migrate")

  contents <- orderly_list_archive(path_local)
  expect_equal(nrow(contents), 1)
  path <- file.path(path_local, "archive", "minimal", contents$id)
  expect_equal(
    readRDS(path_orderly_run_rds(path))$archive_version,
    numeric_version(read_orderly_archive_version(path_local)))
})


## These need dealing with properly, but check that they trigger
## correctly here:
test_that("pull from new remote", {
  dat <- prepare_orderly_remote_example()

  p <- path_orderly_run_rds(
    file.path(dat$path_remote, "archive", "example", dat$id2))
  d <- readRDS(p)
  d$archive_version <- numeric_version("100.100.100")
  saveRDS(d, p)

  expect_error(
    orderly_pull_archive("example", dat$id2, root = dat$path_local,
                         remote = dat$remote),
    "Report was created with orderly more recent than this, upgrade!")
})


test_that("pull migrated archive", {
  oo <- options(orderly.nowarnings = TRUE)
  on.exit(options(oo))
  path_local <- prepare_orderly_example("demo")
  unlink(file.path(path_local, "archive"), recursive = TRUE)
  dir.create(file.path(path_local, "archive"))

  path_remote <- unpack_reference("0.5.4")
  withr::with_options(list(orderly.nmowarnings = TRUE),
                      orderly_migrate(path_remote))
  file.copy(file.path(path_local, "orderly_config.yml"),
            file.path(path_remote, "orderly_config.yml"),
            overwrite = TRUE)
  dir.create(file.path(path_remote, "global"))

  ## Empty archives have a null version:
  expect_equal(read_orderly_archive_version(path_local), "0.0.0")

  remote <- orderly_remote_path(path_remote)
  orderly_pull_archive("use_dependency", root = path_local, remote = remote)

  ## The archive version has been upgraded:
  expect_equal(read_orderly_archive_version(path_local),
               as.character(cache$current_archive_version))
  expect_setequal(orderly_list_archive(path_local)$name,
                  c("other", "use_dependency"))

  ## This fails in old versions, but will work here:
  id <- orderly_run("minimal", root = path_local, echo = FALSE)
  orderly_commit(id, root = path_local)
  expect_true(id %in% orderly_list_archive(path_local)$id)

  ## And this is not necessary but also fails on the previous version
  ## because of issues re-running migrations.
  expect_silent(orderly_migrate(root = path_local))
})
