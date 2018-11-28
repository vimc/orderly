context("remote")


test_that("defaults: null", {
  path <- prepare_orderly_example("minimal")
  expect_null(set_default_remote(NULL, path))
  expect_error(get_default_remote(path, FALSE),
               "default remote has not been set yet")
  expect_error(get_remote(NULL, path),
               "default remote has not been set yet")
})


test_that("get_remote", {
  fake_server <- function(name) {
    list(name = name,
         server = structure(list(is_authorised = function() FALSE,
                                 name = name),
                            class = "montagu_server"))
  }

  config <- list(api_server = list(foo = fake_server("foo"),
                                   bar = fake_server("bar")),
                 path = normalizePath("."))
  class(config) <- "orderly_config"
  expect_equal(get_remote(NULL, config), config$api_server$foo$server)
  expect_equal(get_remote("foo", config), config$api_server$foo$server)
  expect_equal(get_remote("bar", config), config$api_server$bar$server)
  expect_error(get_remote("other", config),
               "Unknown remote 'other'",
               fixed = TRUE)

  p <- prepare_orderly_example("minimal")
  expect_equal(get_remote(p, config), orderly_remote_path(p))

  expect_error(get_remote(TRUE, config),
               "Unknown remote type 'logical'",
               fixed = TRUE)

  expect_equal(remote_name(get_remote("foo", config)), "foo")

  config$api_server$foo$server <- NULL
  ## This will come out once VIMC-2416 is done
  expect_error(get_remote("foo", config),
               "The 'montagu' package is required to use an api server")
})


test_that("defaults: envvar", {
  path <- prepare_orderly_example("minimal")
  tmp <- tempfile()
  dir.create(tmp)
  writeLines("", path_orderly_config_yml(tmp))
  withr::with_envvar(
    c("ORDERLY_DEFAULT_REMOTE_PATH" = tmp),
    expect_equal(get_default_remote(path, FALSE),
                 orderly_remote_path(tmp)))
})


test_that("unpack archive", {
  path <- prepare_orderly_example("minimal")
  id <- orderly_run("example", config = path, echo = FALSE)
  p <- orderly_commit(id, config = path)

  zip <- zip_dir(p)

  root <- tempfile()
  unzip_archive(zip, root, "example", id)
  res <- file.path(root, "archive", "example")
  expect_equal(dir(res), id)
  expect_equal(sort(dir(file.path(res, id), recursive = TRUE)),
               sort(dir(p, recursive = TRUE)))
})


test_that("unpack report failure: corrupt download", {
  skip_on_cran()
  bytes <- as.raw(c(0x50, 0x4b, 0x05, 0x06, rep(0x00, 18L)))
  zip <- tempfile()
  writeBin(bytes, zip)
  ## This test might be platform dependent as a sane unzip function
  ## would have caught this.
  expect_error(suppressWarnings(unzip_archive(zip, tempfile(), NULL, NULL)),
               "Corrupt zip file? No files extracted",
               fixed = TRUE)
})


test_that("unpack failure: not an orderly archive", {
  tmp <- file.path(tempfile(), "parent")
  dir.create(tmp, FALSE, TRUE)
  file.create(file.path(tmp, c("a", "b")))
  zip <- tempfile(fileext = ".zip")
  withr::with_dir(tmp, zip(zip, dir(), extras = "-q"))
  expect_error(unzip_archive(zip, tempfile(), NULL, NULL),
               "Invalid orderly archive")
})


test_that("unpack failure: not expected id", {
  id <- new_report_id()
  tmp <- file.path(tempfile(), id)
  dir.create(tmp, FALSE, TRUE)
  dir.create(file.path(tmp, "orderly.yml"))
  zip <- zip_dir(tmp)
  expect_error(unzip_archive(zip, tempfile(), NULL, "other"),
               sprintf("This is archive '%s' but expected 'other'", id),
               fixed = TRUE)
})


test_that("unpack failure: missing files", {
  id <- new_report_id()
  tmp <- file.path(tempfile(), id)
  dir.create(tmp, FALSE, TRUE)
  dir.create(file.path(tmp, "orderly.yml"))
  zip <- zip_dir(tmp)
  expect_error(unzip_archive(zip, tempfile(), NULL, id),
               "Invalid orderly archive: missing files orderly_run.yml",
               fixed = TRUE)
})


test_that("pull_archive with wrong version", {
  dat <- prepare_orderly_remote_example(FALSE)

  expect_error(
    pull_archive("example", new_report_id(), config = dat$config,
                 remote = dat$remote),
    paste0("Version '.+?' not found at '.+?': valid versions are:.+",
           dat$id1))
})


test_that("pull dependencies", {
  dat <- prepare_orderly_remote_example(FALSE)

  expect_message(
    pull_dependencies("depend", config = dat$config, remote = dat$remote),
    "\\[ pull\\s+ \\]  example:")
  expect_equal(orderly_list_archive(dat$config),
               data_frame(name = "example", id = dat$id2))

  ## and update
  id3 <- orderly_run("example", config = dat$path_remote, echo = FALSE)
  orderly_commit(id3, config = dat$path_remote)
  expect_message(
    pull_dependencies("depend", config = dat$config, remote = dat$remote),
    "\\[ pull\\s+ \\]  example:")
  expect_equal(orderly_list_archive(dat$config),
               data_frame(name = "example", id = c(dat$id2, id3)))
})


test_that("check_remote_type prevents unknown remotes", {
  expect_error(check_remote_type(NULL), "Unknown remote type")
})
