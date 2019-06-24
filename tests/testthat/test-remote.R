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

  expect_message(
    orderly_pull_dependencies("depend", root = dat$config,
                              remote = dat$remote),
    "\\[ pull\\s+ \\]  example:")
  expect_equal(orderly_list_archive(dat$config),
               data_frame(name = "example", id = dat$id2))

  ## and update
  id3 <- orderly_run("example", root = dat$path_remote, echo = FALSE)
  orderly_commit(id3, root = dat$path_remote)
  expect_message(
    orderly_pull_dependencies("depend", root = dat$config,
                              remote = dat$remote),
    "\\[ pull\\s+ \\]  example:")
  expect_equal(orderly_list_archive(dat$config),
               data_frame(name = "example", id = c(dat$id2, id3)))
})


test_that("migrate dependencies on pull", {
  ## create an out of date remote
  ## could be any version, we pick the earliest
  path <- unpack_reference("0.3.2")
  # now create a local repo
  dat <- prepare_orderly_remote_example()

  # delete the local dependency...
  unlink(file.path(dat$path_local, "archive", "example"), recursive = TRUE)
  # ..and pull the out of date version
  w_msg <- c("Use of 'source' is deprecated and will be removed in a",
               "future orderly version - please use 'database' instead.",
               "See the main package vignette for details.")
  expect_warning(orderly_pull_dependencies("depend",
                                           root = dat$path_local,
                                           remote = path),
                 paste(strwrap(paste(w_msg, collapse = " ")), collapse = "\n"))

  # we should get an error here since the report is out of date
  e_msg <- c("Dependency file not an artefact of",
             "example/20170802-182227-5fd9e7ca:\n- 'data.rds'")
  expect_error(orderly_run("depend", root = dat$config, echo = FALSE),
               paste(e_msg, collapse = " "))

  # try to update the local archive; this will do nothing...
  orderly_migrate(root = dat$config)
  # ...so we get the same error message
  expect_error(orderly_run("depend", root = dat$config, echo = FALSE),
               paste(e_msg, collapse = " "))

  # delete the local dependency
  unlink(file.path(dat$path_local, "archive", "example"), recursive = TRUE)
  # update the remote archive...
  orderly_migrate(root = path)
  expect_equal(read_orderly_archive_version(path), "0.6.8")

  # ...and pull the updated version
  expect_warning(orderly_pull_dependencies("depend",
                                           root = dat$path_local,
                                           remote = path),
                 paste(strwrap(paste(w_msg, collapse = " ")), collapse = "\n"))
  # now this should work
  id <- orderly_run("depend", root = dat$config, echo = FALSE)


test_that("pull_dependencies counts dependencies", {
  dat <- prepare_orderly_remote_example()

  expect_message(
    orderly_pull_dependencies("example", root = dat$config,
                              remote = dat$remote),
    "\\[ depends\\s+ \\]  example has 0 dependencies")

  id <- orderly_run("example", root = dat$path_remote, echo = FALSE)
  orderly_commit(id, root = dat$path_remote)
  expect_message(
    orderly_pull_dependencies("depend", root = dat$config,
                              remote = dat$remote),
    "\\[ depends\\s+ \\]  depend has 1 dependency")
})
