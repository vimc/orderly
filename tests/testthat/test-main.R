context("main")

test_that("run", {
  path <- prepare_orderly_example("minimal")
  args <- c("--root", path, "run", "example")
  res <- main_args(args)
  expect_equal(res$command, "run")
  expect_equal(res$args, "example")
  expect_null(res$options$parameters)
  expect_null(res$options$ref)
  expect_false(res$options$no_commit)
  expect_false(res$options$print_log)
  expect_identical(res$target, main_do_run)

  capture.output(res$target(res))
  expect_equal(orderly_list(path), "example")
  expect_equal(nrow(orderly_list_archive(path)), 1)
})

test_that("run: id-file", {
  path <- prepare_orderly_example("minimal")
  id_file <- tempfile()
  args <- c("--root", path, "run", "--id-file", id_file, "example")
  res <- main_args(args)

  expect_equal(res$command, "run")
  expect_equal(res$args, "example")
  expect_null(res$options$parameters)
  expect_false(res$options$no_commit)
  expect_false(res$options$print_log)
  expect_equal(res$options$id_file, id_file)
  expect_identical(res$target, main_do_run)

  capture.output(res$target(res))
  expect_equal(orderly_list(path), "example")

  expect_true(file.exists(id_file))
  id <- readLines(id_file)
  expect_equal(id, orderly_list_archive(path)$id)
})

test_that("run: ref", {
  path <- unzip_git_demo()

  pars <- as.character(jsonlite::toJSON(list(nmin = 0), auto_unbox = TRUE))
  args <- c("--root", path, "run", "--ref", "other", "--parameters", pars,
            "other")

  res <- main_args(args)
  expect_equal(res$command, "run")
  expect_equal(res$options$ref, "other")

  ans <- capture.output(res$target(res))

  d <- orderly_list_archive(path)
  expect_equal(nrow(d), 1L)
  expect_equal(d$name, "other")
})


test_that("run: fetch", {
  path <- prepare_orderly_git_example()
  path_local <- path[["local"]]
  path_origin <- path[["origin"]]
  sha_local <- git_ref_to_sha("HEAD", path_local)
  sha_origin <- git_ref_to_sha("HEAD", path_origin)

  args <- c("--root", path_local, "run", "--ref", "origin/master", "--fetch",
            "minimal")
  res <- main_args(args)
  expect_true(res$options$fetch)
  expect_false(res$options$pull)

  res$target(res)

  id <- orderly_latest("minimal", config = path_local)
  d <- readRDS(path_orderly_run_rds(
    file.path(path_local, "archive", "minimal", id)))
  expect_equal(d$git$sha, sha_origin)
  expect_equal(git_ref_to_sha("HEAD", path_local), sha_local)
})


test_that("run: pull & ref don't go together", {
  path <- unzip_git_demo()
  args <- c("--root", path, "run", "--ref", "origin/master", "--pull",
            "minimal")
  res <- main_args(args)
  expect_error(res$target(res),
               "Can't use --pull with --ref; perhaps you meant --fetch ?",
               fixed = TRUE)
})


test_that("run: pull before run", {
  path <- prepare_orderly_git_example()
  path_local <- path[["local"]]
  path_origin <- path[["origin"]]
  sha_local <- git_ref_to_sha("HEAD", path_local)
  sha_origin <- git_ref_to_sha("HEAD", path_origin)

  args <- c("--root", path_local, "run", "--pull", "minimal")
  res <- main_args(args)
  expect_true(res$options$pull)
  expect_null(res$options$ref)
  res$target(res)

  id <- orderly_latest("minimal", config = path_local)
  d <- readRDS(path_orderly_run_rds(
    file.path(path_local, "archive", "minimal", id)))
  expect_equal(d$git$sha, sha_origin)
  expect_equal(git_ref_to_sha("HEAD", path_local), sha_origin)
})


test_that("commit", {
  path <- prepare_orderly_example("minimal")
  id <- orderly_run("example", config = path, echo = FALSE)
  args <- c("--root", path, "commit", id)
  res <- main_args(args)
  expect_equal(res$command, "commit")
  expect_equal(res$args, id)
  expect_identical(res$target, main_do_commit)

  res$target(res)
  expect_equal(nrow(orderly_list_archive(path)), 1)
})

test_that("publish", {
  path <- prepare_orderly_example("minimal")
  id <- orderly_run("example", config = path, echo = FALSE)
  p <- orderly_commit(id, config = path)

  args <- c("--root", path, "publish", id)
  res <- main_args(args)
  expect_equal(res$command, "publish")
  expect_equal(res$args, id)
  expect_false(res$options$unpublish)
  expect_identical(res$target, main_do_publish)

  res$target(res)
  file <- path_orderly_published_yml(p)
  expect_true(file.exists(file))
  expect_equal(yaml_read(file), list(published = TRUE))
})

test_that("latest", {
  path <- prepare_orderly_example("minimal")
  id1 <- orderly_run("example", config = path, echo = FALSE)
  Sys.sleep(0.1)
  id2 <- orderly_run("example", config = path, echo = FALSE)

  args <- c("--root", path, "latest", "--draft", "example")
  res <- main_args(args)
  expect_equal(res$command, "latest")
  expect_equal(res$args, "example")
  expect_true(res$options$draft)
  expect_identical(res$target, main_do_latest)

  expect_output(main_do_latest(res), id2)

  args <- c("--root", path, "latest", "--value-if-missing", "NONE", "example")
  res <- main_args(args)
  expect_output(main_do_latest(res), "NONE")
})

test_that("help", {
  expect_error(capture.output(main_args("--help")),
               "Aborting as help requested")
  expect_output(try(main_args("--help"), silent = TRUE),
                "The <command> argument must be one of", fixed = TRUE)

  for (cmd in names(main_args_commands())) {
    expect_output(try(main_args(c(cmd, "--help")), silent = TRUE),
                  sprintf("[--root=ROOT] %s", cmd),
                  fixed = TRUE)
  }
})

test_that("list", {
  path <- prepare_orderly_example("minimal")

  args <- c("--root", path, "list")
  res <- main_args(args)

  expect_output(res$target(res), "^example$")

  expect_equal(main_args(c("--root", path, "list", "names"))$args, "names")
  expect_equal(main_args(c("--root", path, "list", "drafts"))$args, "drafts")
  expect_equal(main_args(c("--root", path, "list", "archive"))$args, "archive")
  expect_error(main_args(c("--root", path, "list", "foo")),
               "argument to list must be one of")
})


test_that("unknown", {
  path <- tempfile()
  args <- c("--root", path, "foo")
  expect_error(capture.output(main_args(args)),
               "unknown command 'foo'")
})

test_that("write_script requires directory", {
  expect_error(write_script(tempfile()),
               "'path' must be a directory")
})


test_that("write script produces sensible script", {
  path <- tempfile()
  dir.create(path, FALSE, TRUE)
  bin <- write_script(path)
  expect_equal(basename(bin), "orderly")
  expect_true(file.exists(bin))
  expect_equal(readLines(bin)[[1]], "#!/usr/bin/env Rscript")
})


test_that("write script can be versioned", {
  path <- tempfile()
  dir.create(path, FALSE, TRUE)
  bin <- write_script(path, TRUE)
  expect_match(readLines(bin)[[1]], R.home(), fixed = TRUE)
})


test_that("migrate", {
  path <- unpack_reference("0.3.2")
  expect_equal(orderly_config(path)$archive_version, "0.0.0")

  args <- c("--root", path, "migrate")
  res <- main_args(args)
  expect_equal(res$command, "migrate")
  expect_false(res$options$dry_run)
  expect_null(res$options$to)
  expect_identical(res$target, main_do_migrate)

  res$target(res)
  expect_equal(orderly_config(path)$archive_version,
               as.character(cache$current_archive_version))
})


test_that("migrate: args", {
  path <- prepare_orderly_example("minimal")
  args <- c("--root", path, "migrate", "--to", "0.3.3", "--dry-run")
  res <- main_args(args)
  expect_equal(res$command, "migrate")
  expect_true(res$options$dry_run)
  expect_equal(res$options$to, "0.3.3")
  expect_identical(res$target, main_do_migrate)
})


test_that("rebuild", {
  path <- unpack_reference("0.5.1")
  args <- c("--root", path, "rebuild", "--if-schema-changed")
  res <- main_args(args)
  expect_equal(res$command, "rebuild")
  expect_true(res$options$if_schema_changed)
  expect_identical(res$target, main_do_rebuild)
  expect_true(res$target(res))
  expect_false(res$target(res))
})


test_that("cleanup", {
  path <- prepare_orderly_example("minimal")
  id <- orderly_run("example", config = path, echo = FALSE)

  res <- main_args(c("--root", path, "cleanup"))
  expect_equal(res$command, "cleanup")
  expect_false(res$options$no_data)
  expect_false(res$options$no_draft)
  expect_false(res$options$failed_only)
  expect_identical(res$target, main_do_cleanup)

  expect_equal(nrow(orderly_list2(config = path, draft = TRUE)), 1)
  expect_true(main_do_cleanup(res))
  expect_equal(nrow(orderly_list2(config = path, draft = TRUE)), 0)
  expect_null(main_do_cleanup(res))
})


test_that("list", {
  path <- prepare_orderly_example("minimal")
  id1 <- orderly_run("example", config = path, echo = FALSE)
  id2 <- orderly_run("example", config = path, echo = FALSE)
  orderly_commit(id2, config = path)

  res <- main_args(c("--root", path, "list", "names"))
  expect_equal(capture.output(res$target(res)), "example")

  res <- main_args(c("--root", path, "list", "drafts"))
  out1 <- capture.output(res$target(res))

  res <- main_args(c("--root", path, "list", "archive"))
  out2 <- capture.output(res$target(res))

  expect_match(out1[[1]], "name\\s+id")
  expect_match(out2[[1]], "name\\s+id")

  expect_match(out1[[2]], sprintf("1\\s+example\\s+%s", id1))
  expect_match(out2[[2]], sprintf("1\\s+example\\s+%s", id2))

  res <- main_args(c("--root", path, "list", "names"))
  res$args <- "other"
  expect_error(res$target(res), "orderly bug")
})

test_that("run: message", {
  ## Should have no errors
  path <- prepare_orderly_example("changelog")
  message <- "[label1] This is a test message."
  args <- c("--root", path, "run", "--message", message, "example")
  res <- main_args(args)
  expect_equal(res$command, "run")
  expect_equal(res$args, "example")
  expect_null(res$options$parameters)
  expect_null(res$options$ref)
  expect_false(res$options$no_commit)
  expect_false(res$options$print_log)
  expect_identical(res$target, main_do_run)
  expect_equal(res$options$message, message)

  capture.output(res$target(res))

  ## mal-formatted message
  path <- prepare_orderly_example("changelog")
  message <- "Invalid message"
  args <- c("--root", path, "run", "--message", message, "example")
  res <- main_args(args)

  error <- paste("message must be of the form '[<label>] <message>' failed on:",
                 sprintf("'%s'", message), sep = "\n")
  expect_error(capture.output(res$target(res)), error, fixed = TRUE)

  ## changelog not enabled
  path <- prepare_orderly_example("minimal")
  message <- "[label1] This is a test message."
  args <- c("--root", path, "run", "--message", message, "example")
  res <- main_args(args)

  error <- paste("report 'example' uses changelog,",
                 "but this is not enabled in orderly_config.yml", sep = " ")
  expect_error(capture.output(res$target(res)), error)
})
