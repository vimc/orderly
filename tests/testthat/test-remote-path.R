context("remote: path")

test_that("Reject impossible remotes", {
  remote <- tempfile()
  dir.create(remote)
  expect_error(orderly_remote_path(remote),
               "Does not look like an orderly repository")
})


test_that("remote_name", {
  path <- prepare_orderly_example("minimal")
  expect_identical(normalizePath(orderly_remote_path(path)$name, "/"),
                   normalizePath(path, "/"))
})


test_that("get remote", {
  path1 <- prepare_orderly_example("minimal")
  path2 <- prepare_orderly_example("minimal")

  obj <- get_remote(path1, orderly_config(path2))
  expect_is(obj, "orderly_remote_path")
  expect_equal(normalizePath(obj$name), normalizePath(path1))
})


test_that("pull report", {
  path1 <- prepare_orderly_example("demo")
  id <- orderly_run("multifile-artefact", root = path1, echo = FALSE)
  orderly_commit(id, root = path1)

  path2 <- prepare_orderly_example("demo")
  orderly_pull_archive("multifile-artefact", "latest", path2, remote = path1)

  d <- orderly_list_archive(path2)
  expect_equal(d$name, "multifile-artefact")
  expect_true(d$id %in% orderly_list_archive(path1)$id)

  ## Pulled report is now in the db:
  con <- orderly_db("destination", root = path2)
  on.exit(DBI::dbDisconnect(con))
  expect_equal(DBI::dbReadTable(con, "report_version")$id, id)
})


test_that("pull report: error not found", {
  path1 <- prepare_orderly_example("minimal")
  path2 <- prepare_orderly_example("minimal")

  remote <- orderly_remote_path(path1)

  expect_error(
    orderly_pull_archive("example", new_report_id(), path2, remote = remote),
    "Unknown report")
})


test_that("pull report: already done", {
  path1 <- prepare_orderly_example("minimal")
  path2 <- prepare_orderly_example("minimal")

  id <- orderly_run("example", root = path1, echo = FALSE)
  orderly_commit(id, root = path1)

  remote <- orderly_remote_path(path1)

  expect_message(
    orderly_pull_archive("example", id, path2, remote = remote),
    sprintf("\\[ pull\\s+ \\]  example:%s\\s*$", id))

  expect_message(
    orderly_pull_archive("example", id, path2, remote = remote),
    sprintf("\\[ pull\\s+ \\]  example:%s already exists", id))
})


test_that("remote_report_names", {
  dat <- prepare_orderly_remote_example()
  expect_equal(remote_report_names(dat$config),
               c("depend", "depend2", "depend3", "example"))
})


test_that("remote_report_versions", {
  dat <- prepare_orderly_remote_example()

  expect_equal(
    remote_report_versions("example", dat$config),
    c(dat$id1, dat$id2))
  expect_equal(
    remote_report_versions("unknown", dat$config),
    character(0))
})


test_that("orderly_run", {
  dat <- prepare_orderly_remote_example()
  expect_error(
    orderly_run_remote("example", root = dat$config),
    "'orderly_remote_path' remotes do not run")
})


## TODO: this doesn't make much sense without a second remote too.
test_that("set_default", {
  dat <- prepare_orderly_remote_example()
  v <- orderly_default_remote_set("default", dat$config)
  expect_is(v, "orderly_remote_path")
  expect_equal(v, dat$remote)
  expect_identical(v$name, dat$remote$name)
  expect_identical(orderly_default_remote_get(dat$config), v)
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


test_that("pull report with dependencies", {
  dat <- prepare_orderly_remote_example()

  ## For some reason that I don't understand, we use draft: true for
  ## the depend report here, so fix that:
  p <- file.path(dat$path_remote, "src", "depend", "orderly.yml")
  yml <- grep("^\\s+draft: true", readLines(p), invert = TRUE, value = TRUE)
  writeLines(yml, p)

  id <- orderly_run("depend", root = dat$path_remote, echo = FALSE)
  orderly_commit(id, root = dat$path_remote)

  orderly_pull_archive("depend", id,
                       root = dat$path_local, remote = dat$remote)
  d <- orderly_list_archive(root = dat$path_local)
  expect_setequal(d$id, c(dat$id2, id))
  expect_setequal(d$name, c("example", "depend"))

  con <- orderly_db("destination", root = dat$path_local)
  on.exit(DBI::dbDisconnect(con))
  expect_setequal(d$id, DBI::dbReadTable(con, "report_version")$id)
})


test_that("remote_path implements url_report", {
  path <- prepare_orderly_example("minimal")
  remote <- orderly_remote_path(path)
  expect_equal(remote$url_report("name", "id"),
               file.path(remote$config$root, "name", "id", fsep = "/"))
})
