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

  obj <- get_remote(path1, orderly_config_$new(path2))
  expect_is(obj, "orderly_remote_path")
  expect_equal(normalizePath(obj$name), normalizePath(path1))
})


test_that("pull report", {
  skip_on_cran_windows()
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
    "No versions of 'example' were found at")
})


test_that("pull report: already done", {
  skip_on_cran_windows()
  path1 <- prepare_orderly_example("minimal")
  path2 <- prepare_orderly_example("minimal")

  id <- orderly_run("example", root = path1, echo = FALSE)
  orderly_commit(id, root = path1)

  remote <- orderly_remote_path(path1)

  expect_log_message(
    orderly_pull_archive("example", id, path2, remote = remote),
    sprintf("\\[ pull\\s+ \\]  example:%s\\s*$", id))

  expect_log_message(
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


test_that("pull report with dependencies", {
  dat <- prepare_orderly_remote_example()

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


test_that("push report (path)", {
  skip_on_cran_windows()
  ours <- prepare_orderly_example("demo")
  id <- orderly_run("multifile-artefact", root = ours, echo = FALSE)
  orderly_commit(id, root = ours)

  theirs <- prepare_orderly_example("demo")

  remote <- orderly_remote_path(theirs)
  res <- testthat::evaluate_promise(
    orderly_push_archive("multifile-artefact", "latest", ours, remote = remote))

  re <- "^\\[ push\\s+\\]  multifile-artefact:[0-9]{8}-[0-9]{6}-[[:xdigit:]]{8}"
  expect_match(res$messages, paste0(re, "\\n$"))

  d <- orderly_list_archive(theirs)
  expect_equal(d$name, "multifile-artefact")
  expect_equal(d$id, id)

  res <- testthat::evaluate_promise(
    orderly_push_archive("multifile-artefact", "latest", ours, remote = remote))
  expect_match(res$messages, paste(re, "already exists, skipping\\n$"))
})


test_that("push report & deps", {
  skip_on_cran_windows()
  ours <- prepare_orderly_example("depends", testing = TRUE)
  theirs <- prepare_orderly_example("depends", testing = TRUE)

  id1 <- orderly_run("example", root = ours, echo = FALSE)
  orderly_commit(id1, root = ours)
  id2 <- orderly_run("depend2", root = ours, echo = FALSE)
  orderly_commit(id2, root = ours)
  id3 <- orderly_run("depend3", root = ours, echo = FALSE)
  orderly_commit(id3, root = ours)

  remote <- orderly_remote_path(theirs)
  res <- testthat::evaluate_promise(
    orderly_push_archive("depend3", "latest", ours, remote = remote))

  ## All ended up in the archive:
  d <- orderly_list_archive(theirs)
  expect_setequal(d$name, c("example", "depend2", "depend3"))
  expect_setequal(d$id, c(id1, id2, id3))
  expect_equal(orderly_list_archive(ours), d)

  re <- "\\[ ([a-z]+)\\s*\\]  ([a-z0-9]+)[:/](.*?)\\s*$"
  expect_true(all(grepl(re, res$messages)))
  expect_equal(
    sub(re, "\\1", res$messages),
    c("push", "depends", "push", "depends", "push"))
  expect_equal(
    sub(re, "\\2", res$messages),
    c("depend3", "depend2", "depend2", "example", "example"))
  expect_equal(
    sub(re, "\\3", res$messages),
    c(id3, id2, id2, id1, id1))
})


test_that("Fail to push if not supported", {
  skip_on_cran_windows()
  ours <- prepare_orderly_example("demo")
  theirs <- prepare_orderly_example("demo")

  remote <- as.list(orderly_remote_path(theirs))
  remote$push <- NULL

  expect_error(
    orderly_push_archive("multifile-artefact", "latest", ours, remote = remote),
    "'push' is not supported by this remote")
})


test_that("fetch remote metadata", {
  dat <- prepare_orderly_remote_example()
  base <- normalizePath(file.path(dat$path_remote, "archive"))

  expect_equal(
    normalize_path(dat$remote$metadata("example", dat$id1)),
    normalize_path(file.path(base, "example", dat$id1, "orderly_run.rds")))
})


test_that("pull archive using query", {
  dat <- prepare_orderly_query_example()
  remote <- orderly_remote_path(dat$root)

  root <- prepare_orderly_example("demo")
  orderly_pull_archive("other", "latest(parameter:nmin < 0.25)", root = root,
                       remote = remote)
  expect_equal(
    orderly_list_archive(root),
    data_frame(name = "other", id = dat$ids[[2]]))
})


test_that("pull archive using query and parameters", {
  dat <- prepare_orderly_query_example()
  remote <- orderly_remote_path(dat$root)

  root <- prepare_orderly_example("demo")
  expect_error(
    orderly_pull_archive("other", "latest(parameter:nmin < n)", root = root,
                         remote = remote),
    "Query parameter 'n' not found in supplied parameters")

  orderly_pull_archive("other", "latest(parameter:nmin < n)", root = root,
                       remote = remote, parameters = list(n = 0.25))
  expect_equal(
    orderly_list_archive(root),
    data_frame(name = "other", id = dat$ids[[2]]))
})


test_that("pull dependencies that use a query", {
  dat <- prepare_orderly_query_example()
  remote <- orderly_remote_path(dat$root)

  root <- prepare_orderly_example("demo")

  p <- file.path(root, "src", "use_dependency", "orderly.yml")
  txt <- readLines(p)
  writeLines(sub("latest$", "latest(parameter:nmin < n)", txt), p)

  expect_error(
    orderly_pull_dependencies("use_dependency", root = root, remote = remote),
    "Query parameter 'n' not found in supplied parameters")

  orderly_pull_dependencies("use_dependency", root = root, remote = remote,
                            parameters = list(n = 0.25))

  expect_equal(
    orderly_list_archive(root),
    data_frame(name = "other", id = dat$ids[[2]]))
})


test_that("bundle pack and import", {
  skip_on_cran_windows()
  path1 <- prepare_orderly_example("minimal")
  path2 <- prepare_orderly_example("minimal")

  remote <- orderly_remote_path(path1)
  res <- orderly_bundle_pack_remote("example", remote = remote)
  expect_equal(dirname(res), tempdir())
  expect_match(basename(res), "^[0-9]{8}-[0-9]{6}-[[:xdigit:]]{8}\\.zip$")

  ans <- orderly_bundle_run(res, echo = FALSE)

  orderly_bundle_import_remote(ans$path, remote = remote)

  expect_equal(remote$list_versions("example"), ans$id)
})
