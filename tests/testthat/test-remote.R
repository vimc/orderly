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
