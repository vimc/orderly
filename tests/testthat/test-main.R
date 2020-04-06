context("main")

test_that("run", {
  skip_on_cran_windows()
  path <- prepare_orderly_example("minimal")
  args <- c("--root", path, "run", "example")
  res <- cli_args_process(args)
  expect_equal(res$command, "run")
  expect_equal(res$options$name, "example")
  expect_null(res$options$instance)
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
  skip_on_cran_windows()
  path <- prepare_orderly_example("minimal")
  id_file <- tempfile()
  args <- c("--root", path, "run", "--id-file", id_file, "example")
  res <- cli_args_process(args)

  expect_equal(res$command, "run")
  expect_equal(res$options$name, "example")
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


test_that("run: use instance", {
  skip_on_cran_windows()
  path <- prepare_orderly_example("depends", testing = TRUE)

  p <- file.path(path, "orderly_config.yml")
  writeLines(c(
    "database:",
    "  source:",
    "    driver: RSQLite::SQLite",
    "    instances:",
    "      default:",
    "        dbname: source.sqlite",
    "      alternative:",
    "        dbname: alternative.sqlite"),
    p)

  file.copy(file.path(path, "source.sqlite"),
            file.path(path, "alternative.sqlite"))

  con <- orderly_db("source", root = path, instance = "alternative")
  DBI::dbExecute(con$source, "DELETE from thing where id > 10")

  args <- c("--root", path, "run", "--instance", "alternative", "example")
  res <- cli_args_process(args)

  expect_equal(res$command, "run")
  expect_equal(res$options$name, "example")
  expect_equal(res$options$instance, "alternative")
  expect_null(res$options$parameters)
  expect_false(res$options$no_commit)
  expect_false(res$options$print_log)
  expect_identical(res$target, main_do_run)

  capture.output(res$target(res))

  id <- orderly_list_archive(path)$id
  expect_equal(length(id), 1)
  expect_equal(
    nrow(readRDS(file.path(path, "archive", "example", id, "data.rds"))),
    10)
})


test_that("pass parameters", {
  skip_on_cran_windows()
  f <- function(...) {
    cli_args_process(c("run", "report", ...))$options$parameters
  }
  expect_equal(f("a=1"), list(a = 1))
  expect_equal(f("a=1", "b=value"), list(a = 1, b = "value"))
  expect_equal(f("a=1", "b=value", "c=TRUE"),
               list(a = 1, b = "value", c = TRUE))
  expect_equal(f("a=1+2"), list(a = "1+2"))
  expect_equal(f("a='quoted string'"), list(a = "quoted string"))
  expect_equal(f("a=1,2"), list(a = "1,2"))
})

test_that("pass batch parameters", {
  skip_on_cran_windows()
  f <- function(...) {
    cli_args_process(c("batch", "report", ...))$options$parameters
  }
  expect_equal(f(""), NULL)
  expect_equal(f("a=1"), data_frame(a = 1))
  expect_equal(f("a=1", "b=value"), data_frame(a = 1, b = "value"))
  expect_equal(f("a=1", "b=value", "c=TRUE"),
               data_frame(a = 1, b = "value", c = TRUE))
  expect_equal(f("a=1+2"), data_frame(a = "1+2"))
  expect_error(
    f("a='quoted string'"),
    paste0("Parameters with whitespace not supported in batch run, ",
           "got param 'quoted string'"))
  expect_equal(f("a=1,2"), data_frame(a = c(1, 2)))
  expect_equal(f("a=1,2", "b=value1,value2"),
               data_frame(a = c(1, 2), b = c("value1", "value2")))
  expect_equal(f("a=mixed_types,TRUE,3"),
               data_frame(a = c("mixed_types", "TRUE", "3")))
  expect_error(f("a=1", "b=2,3"),
               "All params must have the same number of values, got.*")
})

test_that("run: ref", {
  testthat::skip_on_cran()
  path <- unzip_git_demo()

  args <- c("--root", path, "run", "--ref", "other", "other", "nmin=0")

  res <- cli_args_process(args)
  expect_equal(res$command, "run")
  expect_equal(res$options$ref, "other")
  expect_equal(res$options$parameters, list(nmin = 0))

  ans <- capture.output(res$target(res))

  d <- orderly_list_archive(path)
  expect_equal(nrow(d), 1L)
  expect_equal(d$name, "other")
})


test_that("run: fetch", {
  testthat::skip_on_cran()
  path <- prepare_orderly_git_example()
  path_local <- path[["local"]]
  path_origin <- path[["origin"]]
  sha_local <- git_ref_to_sha("HEAD", path_local)
  sha_origin <- git_ref_to_sha("HEAD", path_origin)

  args <- c("--root", path_local, "run", "--ref", "origin/master", "--fetch",
            "minimal")
  res <- cli_args_process(args)
  expect_true(res$options$fetch)
  expect_false(res$options$pull)

  res$target(res)

  id <- orderly_latest("minimal", root = path_local)
  d <- readRDS(path_orderly_run_rds(
    file.path(path_local, "archive", "minimal", id)))
  expect_equal(d$git$sha, sha_origin)
  expect_equal(git_ref_to_sha("HEAD", path_local), sha_local)
})


test_that("run: pull & ref don't go together", {
  testthat::skip_on_cran()
  path <- unzip_git_demo()
  args <- c("--root", path, "run", "--ref", "origin/master", "--pull",
            "minimal")
  res <- cli_args_process(args)
  expect_error(res$target(res),
               "Can't use --pull with --ref; perhaps you meant --fetch ?",
               fixed = TRUE,
               class = "orderly_cli_error")
})


test_that("run: pull before run", {
  testthat::skip_on_cran()
  path <- prepare_orderly_git_example()
  path_local <- path[["local"]]
  path_origin <- path[["origin"]]
  sha_local <- git_ref_to_sha("HEAD", path_local)
  sha_origin <- git_ref_to_sha("HEAD", path_origin)

  args <- c("--root", path_local, "run", "--pull", "minimal")
  res <- cli_args_process(args)
  expect_true(res$options$pull)
  expect_null(res$options$ref)
  res$target(res)

  id <- orderly_latest("minimal", root = path_local)
  d <- readRDS(path_orderly_run_rds(
    file.path(path_local, "archive", "minimal", id)))
  expect_equal(d$git$sha, sha_origin)
  expect_equal(git_ref_to_sha("HEAD", path_local), sha_origin)
})


test_that("commit", {
  skip_on_cran_windows()
  path <- prepare_orderly_example("minimal")
  id <- orderly_run("example", root = path, echo = FALSE)
  args <- c("--root", path, "commit", id)
  res <- cli_args_process(args)
  expect_equal(res$command, "commit")
  expect_equal(res$options$id, id)
  expect_identical(res$target, main_do_commit)

  res$target(res)
  expect_equal(nrow(orderly_list_archive(path)), 1)
})


test_that("latest", {
  skip_on_cran_windows()
  path <- prepare_orderly_example("minimal")
  id1 <- orderly_run("example", root = path, echo = FALSE)
  Sys.sleep(0.1)
  id2 <- orderly_run("example", root = path, echo = FALSE)

  args <- c("--root", path, "latest", "--draft", "example")
  res <- cli_args_process(args)
  expect_equal(res$command, "latest")
  expect_equal(res$options$name, "example")
  expect_true(res$options$draft)
  expect_identical(res$target, main_do_latest)

  expect_output(main_do_latest(res), id2)

  args <- c("--root", path, "latest", "--value-if-missing", "NONE", "example")
  res <- cli_args_process(args)
  expect_output(main_do_latest(res), "NONE")
})


test_that("pull dependencies", {
  skip_on_cran_windows()

  dat <- prepare_orderly_remote_example()

  args <- c("--root", dat$path_local, "pull", "--dependencies", "depend",
            "--remote", "default")
  res <- cli_args_process(args)

  expect_equal(res$command, "pull")
  expect_equal(res$options$name, "depend")
  expect_true(res$options$dependencies)
  expect_equal(res$options$remote, "default")
  expect_null(res$options$id)
  expect_identical(res$target, main_do_pull)

  res$target(res)
  expect_equal(orderly_list_archive(dat$config),
               data_frame(name = "example", id = dat$id2))
})


test_that("pull archive", {
  skip_on_cran_windows()

  dat <- prepare_orderly_remote_example()

  args <- c("--root", dat$path_local, "pull", "example",
            "--remote", "default", "--id", dat$id1)
  res <- cli_args_process(args)

  expect_equal(res$command, "pull")
  expect_equal(res$options$name, "example")
  expect_false(res$options$dependencies)
  expect_equal(res$options$remote, "default")
  expect_equal(res$options$id, dat$id1)
  expect_identical(res$target, main_do_pull)

  res$target(res)
  expect_equal(orderly_list_archive(dat$config),
               data_frame(name = "example", id = dat$id1))
})


test_that("pull --dependencies and --id are incompatible", {
  args <- c("pull", "example", "--id", "myid", "--dependencies")
  res <- cli_args_process(args)
  expect_error(
    res$target(res),
    "Do not provide --id with --dependencies",
    fixed = TRUE)
})


test_that("list", {
  skip_on_cran_windows()
  path <- prepare_orderly_example("minimal")

  args <- c("--root", path, "list")
  res <- cli_args_process(args)

  expect_output(res$target(res), "^example$")

  for (i in c("names", "drafts", "archive")) {
    expect_equal(
      cli_args_process(c("--root", path, "list", i))$options$type,
      i)
  }
})


test_that("unknown", {
  path <- tempfile()
  args <- c("--root", path, "foo")
  expect_error(capture.output(cli_args_process(args)),
               "'foo' is not an orderly command. See orderly --help",
               class = "orderly_cli_error")
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
  oo <- options(orderly.nowarnings = TRUE)
  on.exit(options(oo))

  path <- unpack_reference("0.3.2")
  expect_equal(orderly_config$new(path)$archive_version, "0.0.0")

  args <- c("--root", path, "migrate")
  res <- cli_args_process(args)
  expect_equal(res$command, "migrate")
  expect_false(res$options$dry_run)
  expect_false(res$options$clean)
  expect_null(res$options$to)
  expect_identical(res$target, main_do_migrate)

  res$target(res)
  expect_equal(orderly_config$new(path)$archive_version,
               as.character(cache$current_archive_version))
})


test_that("migrate: args", {
  path <- prepare_orderly_example("minimal")
  args <- c("--root", path, "migrate", "--to", "0.3.3", "--dry-run", "--clean")
  res <- cli_args_process(args)
  expect_equal(res$command, "migrate")
  expect_true(res$options$dry_run)
  expect_true(res$options$clean)
  expect_equal(res$options$to, "0.3.3")
  expect_identical(res$target, main_do_migrate)
})


test_that("rebuild", {
  oo <- options(orderly.nowarnings = TRUE)
  on.exit(options(oo))

  path <- unpack_reference("0.5.1")
  args <- c("--root", path, "rebuild", "--if-schema-changed")
  res <- cli_args_process(args)
  expect_equal(res$command, "rebuild")
  expect_true(res$options$if_schema_changed)
  expect_identical(res$target, main_do_rebuild)
  expect_true(res$target(res))
  expect_false(res$target(res))
})


test_that("cleanup", {
  skip_on_cran_windows()
  path <- prepare_orderly_example("minimal")
  id <- orderly_run("example", root = path, echo = FALSE)

  res <- cli_args_process(c("--root", path, "cleanup"))
  expect_equal(res$command, "cleanup")
  expect_false(res$options$no_data)
  expect_false(res$options$no_draft)
  expect_false(res$options$failed_only)
  expect_identical(res$target, main_do_cleanup)

  expect_equal(nrow(orderly_list2(root = path, draft = TRUE)), 1)
  expect_true(main_do_cleanup(res))
  expect_equal(nrow(orderly_list2(root = path, draft = TRUE)), 0)
  expect_null(main_do_cleanup(res))
})


test_that("list", {
  skip_on_cran_windows()
  path <- prepare_orderly_example("minimal")
  id1 <- orderly_run("example", root = path, echo = FALSE)
  id2 <- orderly_run("example", root = path, echo = FALSE)
  orderly_commit(id2, root = path)

  res <- cli_args_process(c("--root", path, "list", "names"))
  expect_equal(capture.output(res$target(res)), "example")

  res <- cli_args_process(c("--root", path, "list", "drafts"))
  out1 <- capture.output(res$target(res))

  res <- cli_args_process(c("--root", path, "list", "archive"))
  out2 <- capture.output(res$target(res))

  expect_match(out1[[1]], "name\\s+id")
  expect_match(out2[[1]], "name\\s+id")

  expect_match(out1[[2]], sprintf("1\\s+example\\s+%s", id1))
  expect_match(out2[[2]], sprintf("1\\s+example\\s+%s", id2))
})


test_that("run: message", {
  skip_on_cran_windows()
  ## Should have no errors
  path <- prepare_orderly_example("changelog", testing = TRUE)
  message <- "[label1] This is a test message."
  args <- c("--root", path, "run", "--message", squote(message), "example")
  res <- cli_args_process(args)
  expect_equal(res$command, "run")
  expect_equal(res$options$name, "example")
  expect_null(res$options$parameters)
  expect_null(res$options$ref)
  expect_false(res$options$no_commit)
  expect_false(res$options$print_log)
  expect_identical(res$target, main_do_run)
  expect_equal(res$options$message, message)

  capture.output(res$target(res))

  con <- orderly_db("destination", root = path)
  on.exit(DBI::dbDisconnect(con))
  d <- DBI::dbReadTable(con, "changelog")

  table_message <- sprintf("[%s] %s", d$label, d$value)
  expect_equal(message, table_message)
})


test_that("run: bad message", {
  ## mal-formatted message
  path <- prepare_orderly_example("changelog", testing = TRUE)
  message <- "Invalid message"
  args <- c("--root", path, "run", "--message", squote(message), "example")
  res <- cli_args_process(args)

  error <- paste("message must be of the form '[<label>] <message>' failed on:",
                 sprintf("'%s'", message), sep = "\n")
  expect_error(capture.output(res$target(res)), error, fixed = TRUE)
})


test_that("run: message no changelog", {
  ## changelog not enabled
  path <- prepare_orderly_example("minimal")
  message <- "[label1] This is a test message."
  args <- c("--root", path, "run", "--message", squote(message), "example")
  res <- cli_args_process(args)

  error <- paste("report 'example' uses changelog,",
                 "but this is not enabled in orderly_config.yml", sep = " ")
  expect_error(capture.output(res$target(res)), error)
})


test_that("preprocess with no options", {
  expect_equal(
    cli_args_preprocess(c("run")),
    list(root = NULL, list_commands = NULL,
         command = "run", args = character(0)))
  expect_equal(
    cli_args_preprocess(c("run", "--option", "other")),
    list(root = NULL, list_commands = NULL,
         command = "run", args = c("--option", "other")))
})


test_that("preprocess set root", {
  expect_equal(
    cli_args_preprocess(c("--root", "value", "list")),
    list(root = "value", list_commands = NULL,
         command = "list", args = character(0)))
})


test_that("main wrapper", {
  path <- prepare_orderly_example("minimal")
  expect_output(main(c("--root", path, "list")), "example")
})


test_that("docopt failure gives reasonable error", {
  path <- prepare_orderly_example("minimal")
  expect_error(
    main(c("--root", path, "list", "unknown")),
    class = "orderly_cli_error")
})


test_that("invalid parameters", {
  expect_error(
    cli_args_process_run_parameters("a"),
    "Invalid parameters 'a' - all must be in form key=value",
    fixed = TRUE)
  expect_error(
    cli_args_process_run_parameters(c("a", "b=2", "c=3=4")),
    "Invalid parameters 'a', 'c=3=4' - all must be in form key=value",
    fixed = TRUE)
})


test_that("parameter type conversion", {
  expect_equal(parse_parameter("abc"), "abc")
  expect_equal(parse_parameter("123abc"), "123abc")
  expect_equal(parse_parameter("123"), 123)
  expect_equal(parse_parameter("TRUE"), TRUE)

  expect_equal(parse_parameter("'quoted string'"), "quoted string")
  expect_equal(parse_parameter('"quoted string"'), "quoted string")
})


test_that("run captures output", {
  skip_on_cran_windows()
  path <- prepare_orderly_example("minimal")
  args <- c("--root", path, "run", "example")
  res <- cli_args_process(args)
  expect_identical(res$target, main_do_run)

  expect_message(res$target(res))

  archive_path <- file.path(path, "archive", "example")
  id <- list.files(archive_path)
  expect_length(id, 1)
  log_file <- file.path(archive_path, id, "orderly.log")
  expect_true(file.exists(log_file))
  logs <- readLines(log_file)
  expect_true(sprintf("[ id         ]  %s", id) %in% logs)

  args <- c("--root", path, "run",  "--print-log", "example")
  res <- cli_args_process(args)
  expect_identical(res$target, main_do_run)

  out <- evaluate_promise(capture.output(res$target(res), type = "message"))

  ids <- list.files(archive_path)
  expect_length(ids, 2)
  id <- ids[ids != id]
  expect_true(sprintf("[ id         ]  %s\n", id) %in% out$messages)
  log_file <- file.path(archive_path, id, "orderly.log")
  ## Logs have not been written to file
  expect_false(file.exists(log_file))
})


test_that("batch", {
  testthat::skip_on_cran()
  path <- unzip_git_demo()

  args <- c("--root", path, "batch", "--ref", "other", "other", "nmin=0,0.2")

  res <- cli_args_process(args)
  expect_equal(res$command, "batch")
  expect_equal(res$options$ref, "other")
  expect_equal(res$options$parameters, data_frame(nmin = c(0, 0.2)))
  expect_equal(res$target, main_do_batch)

  expect_message(res$target(res), "ids:[\\w\\d-]*")

  d <- orderly_list_archive(path)
  expect_equal(nrow(d), 2L)
  expect_equal(d$name, c("other", "other"))

  ids <- d$id
  invisible(lapply(ids, function(id) {
    log_file <- file.path(path, "archive", "other", id, "orderly.log")
    expect_true(file.exists(log_file))
    logs <- readLines(log_file)
    ## Logs are only for one report
    expect_true(sprintf("[ id         ]  %s", id) %in% logs)
    expect_equal(sum(grepl("\\[ id         \\].*", logs)), 1)
  }))

  ## Batch run without saving logs
  args <- c("--root", path, "batch", "--ref", "other", "--print-log",
            "other", "nmin=0,0.2")
  res <- cli_args_process(args)
  out <- evaluate_promise(capture.output(res$target(res), type = "message"))

  d <- orderly_list_archive(path)
  expect_equal(nrow(d), 4L)
  expect_equal(d$name, rep("other", times = 4))

  ids <- d$id[d$id != ids]
  lapply(ids, function(id) {
    expect_true(sprintf("[ id         ]  %s\n", id) %in% out$messages)
    log_file <- file.path(archive_path, id, "orderly.log")
    ## Logs have not been written to file
    expect_false(file.exists(log_file))
  })
})


test_that("batch: pull & ref don't go together", {
  testthat::skip_on_cran()
  path <- unzip_git_demo()
  args <- c("--root", path, "batch", "--ref", "other", "--pull", "other")
  res <- cli_args_process(args)
  expect_error(res$target(res),
               "Can't use --pull with --ref; perhaps you meant --fetch ?",
               fixed = TRUE,
               class = "orderly_cli_error")
})

test_that("batch: pull before run", {
  testthat::skip_on_cran()
  path <- prepare_orderly_git_example(branch = "other")
  path_local <- path[["local"]]
  path_origin <- path[["origin"]]
  sha_local <- git_ref_to_sha("HEAD", path_local)
  sha_origin <- git_ref_to_sha("HEAD", path_origin)

  args <- c("--root", path_local, "batch", "--pull", "other", "nmin=0,0.2")
  res <- cli_args_process(args)
  expect_true(res$options$pull)
  expect_null(res$options$ref)
  res$target(res)

  id <- orderly_latest("other", root = path_local)
  d <- readRDS(path_orderly_run_rds(
    file.path(path_local, "archive", "other", id)))
  expect_equal(d$git$sha, sha_origin)
  expect_equal(git_ref_to_sha("HEAD", path_local), sha_origin)
})