context("remote")


test_that("defaults: null", {
  path <- test_prepare_orderly_example("minimal")
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


test_that("pull dependencies with implied name", {
  dat <- prepare_orderly_remote_example()
    expect_equal(nrow(orderly_list_archive(dat$config)), 0)
  withr::with_dir(
    file.path(dat$config$root, "src", "depend"),
    orderly_pull_dependencies(remote = dat$remote))
  expect_equal(nrow(orderly_list_archive(dat$config)), 1)
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
  path_local <- test_prepare_orderly_example("demo")
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
  path_local <- test_prepare_orderly_example("demo")
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


test_that("silently ignore missing slack url, but resolve args", {
  path <- test_prepare_orderly_example("minimal")

  append_lines(
    c("remote:",
      "  default:",
      "    driver: orderly1::orderly_remote_path",
      "    args:",
      "      path: $ORDERLY_UNSET_REMOTE_PATH",
      "    slack_url: $ORDERLY_UNSET_SLACK_URL"),
    file.path(path, "orderly_config.yml"))

  config <- orderly_config_$new(path)

  clear_remote_cache()
  remote <- withr::with_envvar(
    c(ORDERLY_UNSET_REMOTE_PATH = path),
    get_remote("default", config))
  expect_equal(length(orderly1:::cache$remotes), 1L)
  expect_null(attr(remote, "slack_url"))
  expect_false(attr(remote, "primary"))

  clear_remote_cache()
  remote <- withr::with_envvar(
    c(ORDERLY_UNSET_REMOTE_PATH = path,
      ORDERLY_UNSET_SLACK_URL = "http://example.com/slack"),
    get_remote("default", config))
  expect_equal(length(orderly1:::cache$remotes), 1L)
  expect_equal(attr(remote, "slack_url"), "http://example.com/slack")
  expect_false(attr(remote, "primary"))
})


test_that("get remote", {
  path_remote <- test_prepare_orderly_example("minimal")
  path_local <- test_prepare_orderly_example("minimal")

  ## Configure our remote:
  path_config <- file.path(path_local, "orderly_config.yml")
  txt <- readLines(path_config)
  writeLines(c(
    txt,
    "remote:",
    "  default:",
    "    driver: orderly1::orderly_remote_path",
    "    args:",
    paste("      path:", path_remote)),
    path_config)

  ## Get our remote:
  remote <- orderly_remote(root = path_local)

  expect_is(remote, "orderly_remote_path")
  expect_equal(remote$name, "default")
  expect_true(same_path(remote$config$root, path_remote))
})

test_that("teams url can be configured and silently ignored if missing", {
  path <- test_prepare_orderly_example("minimal")

  append_lines(
    c("remote:",
      "  default:",
      "    driver: orderly1::orderly_remote_path",
      "    args:",
      "      path: $ORDERLY_PATH",
      "    teams_url: $ORDERLY_TEAMS_URL"),
    file.path(path, "orderly_config.yml"))

  config <- orderly_config_$new(path)

  clear_remote_cache()
  remote <- withr::with_envvar(
    c(ORDERLY_PATH = path),
    get_remote("default", config))
  expect_equal(length(orderly1:::cache$remotes), 1L)
  expect_null(attr(remote, "teams_url"))
  expect_false(attr(remote, "primary"))

  clear_remote_cache()
  remote <- withr::with_envvar(
    c(ORDERLY_PATH = path,
      ORDERLY_TEAMS_URL = "http://example.com/slack"),
    get_remote("default", config))
  expect_equal(length(orderly1:::cache$remotes), 1L)
  expect_equal(attr(remote, "teams_url"), "http://example.com/slack")
  expect_false(attr(remote, "primary"))
})

test_that("orderly run remote passes instance to run", {
  path_local <- test_prepare_orderly_example("demo")

  ## Create a minimal remote class which will satisfy implements_remote
  mock_remote <- R6::R6Class(
    "orderly_mock_remote",
    lock_objects = FALSE,
    public = list(
      list_reports = function() TRUE,
      list_versions = function() TRUE,
      pull = function() TRUE,
      url_report = function() TRUE,
      kill = function() TRUE
    )
  )

  ## Bit of awkwardness with adding run function here. We want to mock out new
  ## function but can't do that inside the class.
  remote <- mock_remote$new()
  remote$run <- mockery::mock(TRUE, cycle = TRUE)
  orderly_run_remote("minimal", remote = remote, root = path_local)

  mockery::expect_called(remote$run, 1)
  args <- mockery::mock_args(remote$run)[[1]]
  expect_null(args$instance)

  orderly_run_remote("minimal", remote = remote, root = path_local,
                     instance = "test")

  mockery::expect_called(remote$run, 2)
  args <- mockery::mock_args(remote$run)[[2]]
  expect_equal(args$instance, "test")
})


test_that("orderly_bundle_(pack|import)_remote do not use root/locate", {
  skip_on_cran_windows()
  path <- test_prepare_orderly_example("minimal")
  remote <- orderly_remote_path(path)

  temp <- tempfile()
  on.exit(unlink(temp, recursive = TRUE))
  dir_create(temp)

  res <- withr::with_dir(
    temp,
    orderly_bundle_pack_remote("example", remote = remote,
                               root = stop("don't force me"),
                               locate = stop("don't force me"),
                               dest = "."))

  expect_true(file.exists(file.path(temp, basename(res))))
  expect_equal(dirname(res), ".")

  ans <- orderly_bundle_run(file.path(temp, basename(res)), echo = FALSE)

  withr::with_dir(
    temp,
    orderly_bundle_import_remote(ans$path, remote = remote,
                                 root = stop("don't force me"),
                                 locate = stop("don't force me")))

  expect_equal(remote$list_versions("example"), ans$id)
})


test_that("orderly run remote passes ref to run", {
  path <- test_prepare_orderly_git_example()

  ## Create a minimal remote class which will satisfy implements_remote
  mock_remote <- R6::R6Class(
    "orderly_mock_remote",
    lock_objects = FALSE,
    public = list(
      list_reports = function() TRUE,
      list_versions = function() TRUE,
      pull = function() TRUE,
      url_report = function() TRUE,
      kill = function() TRUE
    )
  )

  ## Bit of awkwardness with adding run function here. We want to mock out new
  ## function but can't do that inside the class.
  remote <- mock_remote$new()
  remote$run <- mockery::mock(TRUE, cycle = TRUE)
  orderly_run_remote("minimal", remote = remote, root = path[["local"]])

  mockery::expect_called(remote$run, 1)
  args <- mockery::mock_args(remote$run)[[1]]
  expect_null(args$ref)

  orderly_run_remote("minimal", remote = remote, root = path[["local"]],
                     ref = "master")

  mockery::expect_called(remote$run, 2)
  args <- mockery::mock_args(remote$run)[[2]]
  expect_match(args$ref, "[0-9a-f]{40}")
})


test_that("can get status of remote queue", {
  path_local <- test_prepare_orderly_example("demo")

  status <- list(
    tasks = list(
      name = "slow3",
      version = "20210423-143954-e634ca18",
      key = "fungiform_kiwi",
      status = "running"
    )
  )
  mock_status <- mockery::mock(status)
  ## Create a minimal remote class which will satisfy implements_remote
  mock_remote <- R6::R6Class(
    "orderly_mock_remote",
    lock_objects = FALSE,
    public = list(
      list_reports = function() TRUE,
      list_versions = function() TRUE,
      pull = function() TRUE,
      run = function() TRUE,
      url_report = function() TRUE,
      kill = function() TRUE,
      queue_status = function() mock_status()
    )
  )

  remote <- mock_remote$new()
  res <- orderly_remote_status(remote = remote)
  mockery::expect_called(mock_status, 1)
  expect_equal(res, status)
})


test_that("pull leaf only", {
  dat <- prepare_orderly_remote_example()
  id3 <- orderly_run("depend", root = dat$path_remote, echo = FALSE)
  orderly_commit(id3, root = dat$path_remote)

  orderly_pull_archive("depend", root = dat$config, remote = dat$remote,
                       recursive = FALSE)

  ## We only have the one archive report now
  expect_equal(
    orderly_list_archive(dat$config),
    data_frame(name = "depend", id = id3))

  ## And one with metadata only
  expect_equal(
    orderly_list_metadata(dat$config),
    data_frame(name = "example", id = dat$id2))
  expect_equal(
    orderly_list_metadata(dat$config, include_archive = TRUE),
    data_frame(name = "example", id = dat$id2))

  ## But we have two in the database
  con <- orderly_db("destination", dat$config)
  d <- DBI::dbReadTable(con, "report_version")
  DBI::dbDisconnect(con)
  expect_equal(nrow(d), 2)
  expect_equal(d$id, c(dat$id2, id3))
})


test_that("Can rebuild when there is a metadata store", {
  dat <- prepare_orderly_remote_example()
  id3 <- orderly_run("depend", root = dat$path_remote, echo = FALSE)
  orderly_commit(id3, root = dat$path_remote)
  orderly_pull_archive("depend", root = dat$config, remote = dat$remote,
                       recursive = FALSE)

  orderly_rebuild(dat$config)

  con <- orderly_db("destination", dat$config)
  d <- DBI::dbReadTable(con, "report_version")
  DBI::dbDisconnect(con)
  expect_equal(nrow(d), 2)
  expect_equal(d$id, c(dat$id2, id3))
})


test_that("Can cope will pulling complete tree after metadata pulled", {
  dat <- prepare_orderly_remote_example()
  id3 <- orderly_run("depend", root = dat$path_remote, echo = FALSE)
  orderly_commit(id3, root = dat$path_remote)
  orderly_pull_archive("depend", root = dat$config, remote = dat$remote,
                       recursive = FALSE)
  orderly_pull_archive("example", root = dat$config, remote = dat$remote)

  expect_equal(
    orderly_list_metadata(dat$config),
    data_frame(name = character(), id = character()))
  expect_equal(
    orderly_list_metadata(dat$config, include_archive = TRUE),
    data_frame(name = "example", id = dat$id2))

  orderly_rebuild(dat$config)
})


test_that("re-pulling metadata prints informative message", {
  dat <- prepare_orderly_remote_example()
  expect_message(
    orderly_pull_metadata("example", dat$id2, root = dat$config,
                          remote = dat$remote),
    "metadata.+fetching example:[0-9]{8}-[0-9]{6}-[[:xdigit:]]{8}")
  expect_message(
    orderly_pull_metadata("example", dat$id2, root = dat$config,
                          remote = dat$remote),
    "metadata.+example:[0-9]{8}-[0-9]{6}-[[:xdigit:]]{8} already exists, skip")
})


test_that("Can't pull incompatible metadata", {
  dat <- prepare_orderly_remote_example()
  p <- file.path(dat$path_remote, "archive", "example", dat$id2,
                 "orderly_run.rds")
  d <- readRDS(p)
  d$archive_version <- numeric_version("1.0.0")
  saveRDS(d, p)

  expect_error(
    orderly_pull_metadata("example", dat$id2, root = dat$config,
                          remote = dat$remote),
    "Can't migrate metadata for 'example:.+', migrate remote or pull archive")

  d$archive_version <- numeric_version("9.0.0")
  saveRDS(d, p)

  expect_error(
    orderly_pull_metadata("example", dat$id2, root = dat$config,
                          remote = dat$remote),
    "Report was created with orderly more recent than this, upgrade!")
})


test_that("reports can be cancelled", {
  path_local <- test_prepare_orderly_example("demo")

  res_1 <- list(
    killed = TRUE,
    message = NULL
  )
  res_2 <- list(
    killed = FALSE,
    message = "Could not kill task, already complete"
  )
  mock_kill <- mockery::mock(res_1, res_2)
  ## Create a minimal remote class which will satisfy implements_remote
  mock_remote <- R6::R6Class(
    "orderly_mock_remote",
    lock_objects = FALSE,
    public = list(
      list_reports = function() TRUE,
      list_versions = function() TRUE,
      pull = function() TRUE,
      run = function() TRUE,
      url_report = function() TRUE,
      queue_status = function() TRUE,
      kill = function(key) mock_kill()
    )
  )

  remote <- mock_remote$new()
  res <- orderly_cancel_remote(
    c("fungiform_kiwi", "lithe_iaerismetalmark"), remote = remote)
  mockery::expect_called(mock_kill, 2)
  expect_equal(names(res), c("fungiform_kiwi", "lithe_iaerismetalmark"))
  expect_equal(res$fungiform_kiwi, res_1)
  expect_equal(res$lithe_iaerismetalmark, res_2)
})
