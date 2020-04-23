context("slack")

test_that("slack payload is correct", {
  skip_if_not_installed("httr")
  skip_if_not_installed("jsonlite")
  server_url <- "https://example.com"
  server_is_primary <- FALSE
  server_name <- "myserver"
  dat <- list(meta = list(elapsed = 10,
                          id = "20181213-123456-fedcba98",
                          name = "example"),
              git = NULL)
  report_url <- sprintf("%s/reports/%s/%s/", server_url,
                        dat$meta$name, dat$meta$id)
  d <- slack_data(dat, server_name, report_url, server_is_primary)

  expect_equal(
    d,
    list(
      username = "orderly",
      icon_emoji = ":ambulance:",
      attachments = list(list(
        title = "Ran report 'example'",
        text = "on server *myserver* in 10 secs",
        color = "warning",
        fallback = sprintf("Ran '%s' as '%s'; view at %s",
                           dat$meta$name, dat$meta$id, report_url),
        fields = list(list(
          title = "id", value = sprintf("`%s`", dat$meta$id), short = TRUE)),
        actions = list(list(
          name = "link", type = "button", text = ":clipboard: View report",
          style = "primary", url = report_url))))))

  skip_if_no_internet()
  slack_url <- "https://httpbin.org/post"
  r <- do_post_success(slack_url, d, "slack")

  str <- httr::content(r, "text", encoding = "UTF-8")
  cmp <- jsonlite::fromJSON(str, FALSE)$json

  expect_setequal(names(d), names(cmp))
  expect_equal(d$username, cmp$username)
  expect_equal(d$icon_emoji, cmp$icon_emoji)
  expect_setequal(names(d$attachments[[1]]), names(cmp$attachments[[1]]))
})


test_that("git information is collected correctly", {
  skip_if_not_installed("httr")
  skip_if_not_installed("jsonlite")
  server_url <- "https://example.com"
  server_is_primary <- FALSE
  server_name <- "myserver"
  dat <- list(meta = list(elapsed = 10,
                          id = "20181213-123456-fedcba98",
                          name = "example"),
              git = NULL)
  report_url <- sprintf("%s/reports/%s/%s/", server_url,
                        dat$meta$name, dat$meta$id)
  d <- slack_data(dat, server_name, report_url, server_is_primary)

  expect_equal(length(d$attachments[[1]]$fields), 1L)

  dat <- list(meta = list(elapsed = 10,
                          id = "20181213-123456-fedcba98",
                          name = "example"),
              git = list(branch = "master", sha_short = "abcdefg"))
  d <- slack_data(dat, server_name, report_url, server_is_primary)
  expect_equal(length(d$attachments[[1]]$fields), 2L)
  expect_equal(d$attachments[[1]]$fields[[2L]],
               list(title = "git", value = "master@abcdefg", short = TRUE))

  dat <- list(meta = list(elapsed = 10,
                          id = "20181213-123456-fedcba98",
                          name = "example"),
              git = list(branch = "master", sha_short = "abcdefg",
                         github_url = "https://github.com/vimc/repo"))
  d <- slack_data(dat, server_name, report_url, server_is_primary)
  expect_equal(
    d$attachments[[1]]$fields[[2]]$value,
    paste0("<https://github.com/vimc/repo/tree/master|master>@",
           "<https://github.com/vimc/repo/tree/abcdefg|abcdefg>"))
})


test_that("git information works in detached head mode", {
  server_url <- "https://example.com"
  server_is_primary <- FALSE
  server_name <- "myserver"

  dat <- list(meta = list(elapsed = 10,
                          id = "20181213-123456-fedcba98",
                          name = "example"),
              git = list(branch = NULL, sha_short = "abcdefg",
                         github_url = "https://github.com/vimc/repo"))
  report_url <- sprintf("%s/reports/%s/%s/", server_url,
                        dat$meta$name, dat$meta$id)
  d <- slack_data(dat, server_name, report_url, server_is_primary)
  expect_equal(
    d$attachments[[1]]$fields[[2]]$value,
    "(detached)@<https://github.com/vimc/repo/tree/abcdefg|abcdefg>")
})


test_that("primary server changes colour", {
  server_url <- "https://example.com"
  server_is_primary <- FALSE
  server_name <- "myserver"
  dat <- list(meta = list(elapsed = 10,
                          id = "20181213-123456-fedcba98",
                          name = "example"),
              git = NULL)
  report_url <- sprintf("%s/reports/%s/%s/", server_url,
                        dat$meta$name, dat$meta$id)
  d1 <- slack_data(dat, server_name, report_url, TRUE)
  d2 <- slack_data(dat, server_name, report_url, FALSE)
  expect_equal(d1$attachments[[1]]$color, "good")
  expect_equal(d2$attachments[[1]]$color, "warning")
  d2$attachments[[1]]$color <- d1$attachments[[1]]$color
  expect_identical(d1, d2)
})


test_that("sending messages is not a failure", {
  skip_if_no_internet()

  server_url <- "https://example.com"
  server_is_primary <- FALSE
  server_name <- "myserver"
  dat <- list(meta = list(elapsed = 10,
                          id = "20181213-123456-fedcba98",
                          name = "example"),
              git = NULL)
  report_url <- sprintf("%s/reports/%s/%s/", server_url,
                        dat$meta$name, dat$meta$id)
  d <- slack_data(dat, server_name, report_url, server_is_primary)

  slack_url <- "https://httpbin.org/status/403"
  expect_message(do_post_success(slack_url, d, "slack"),
                 "NOTE: running slack hook failed")
})


test_that("main interface", {
  skip_if_no_internet()
  skip_if_not_installed("jsonlite")

  path <- prepare_orderly_example("minimal")
  id <- "20181213-123456-fedcba98"
  name <- "example"
  dat <- list(meta = list(elapsed = 10, id = id, name = name),
              git = NULL)

  path <- tempfile()
  dir.create(path)
  writeLines(
    c("remote:",
      "  myserver:",
      "    driver: orderly::orderly_remote_path",
      "    args:",
      paste("      path:", path),
      "    slack_url: https://httpbin.org/post",
      "    primary: true"),
    file.path(path, "orderly_config.yml"))

  config <- withr::with_envvar(
    c(ORDERLY_API_SERVER_IDENTITY = "myserver"),
    orderly_config$new(path))

  r <- post_success(dat, config)

  expect_equal(r$slack$status_code, 200L)
  res <- jsonlite::fromJSON(httr::content(r$slack)$data, FALSE)
  expect_equal(res$attachments[[1]]$actions[[1]]$url,
               orderly_remote_path(path)$url_report(name, id))

  expect_null(r$teams)
})


test_that("main interface", {
  skip_if_no_internet()
  skip_if_not_installed("httr")

  path <- prepare_orderly_example("minimal")
  append_lines(c(
    "remote:",
    "  testing:",
    "    driver: orderly::orderly_remote_path",
    "    args:",
    sprintf("      path: %s", path),
    "    slack_url: https://httpbin.org/post",
    "  production:",
    "    driver: orderly::orderly_remote_path",
    "    args:",
    sprintf("      path: %s", path),
    "    slack_url: https://httpbin.org/post",
    "    primary: true"),
    file.path(path, "orderly_config.yml"))

  dat <- list(meta = list(elapsed = 10,
                          id = "20181213-123456-fedcba98",
                          name = "example"),
              git = NULL)

  config <- withr::with_envvar(
    c("ORDERLY_API_SERVER_IDENTITY" = "production"),
    orderly_config$new(path))

  r <- post_success(dat, config)
  expect_equal(r$slack$status_code, 200L)
  d <- httr::content(r$slack)
  expect_equal(d$json$attachments[[1]]$color, "good") # primary
  expect_true(endsWith(d$json$attachments[[1]]$actions[[1]]$url,
                       file.path("example", dat$meta$id, fsep = "/")))

  config <- withr::with_envvar(
    c("ORDERLY_API_SERVER_IDENTITY" = "testing"),
    orderly_config$new(path))

  r <- post_success(dat, config)
  expect_equal(r$slack$status_code, 200L)
  d <- httr::content(r$slack)
  expect_equal(d$json$attachments[[1]]$color, "warning") # secondary
})


test_that("exit on no httr", {
  skip_if_not_installed("mockery")
  dat <- list(meta = list(elapsed = 10,
                          id = "20181213-123456-fedcba98",
                          name = "example"))
  url <- "https://httpbin.org/post"
  mockery::stub(do_post_success, "requireNamespace", FALSE)
  expect_null(do_post_success(url, dat, "slack"))
})


## This changed to trigger VIMC-2992; this test will fail if the
## underlying data changes again, while the above tests with mock data
## will continue to work.
test_that("slack payload is correct given actual run data", {
  skip_on_cran()
  skip_on_appveyor()
  path <- prepare_orderly_git_example()
  path1 <- path[["origin"]]
  id <- orderly_run("minimal", root = path1, echo = FALSE)
  p <- file.path(path1, "draft", "minimal", id)
  dat <- readRDS(path_orderly_run_rds(p))

  server_url <- "https://example.com"
  server_is_primary <- FALSE
  server_name <- "myserver"

  report_url <- sprintf("%s/reports/%s/%s/", server_url,
                        dat$meta$name, dat$meta$id)
  d <- slack_data(dat, server_name, report_url, server_is_primary)
  git_info <- sprintf("%s@%s", dat$git$branch, dat$git$sha_short)

  expect_equal(d$username, "orderly")
  expect_equal(d$icon_emoji, ":ambulance:")
  expect_equal(length(d$attachments), 1L)

  a <- d$attachments[[1]]
  expect_setequal(names(a),
                  c("title", "text", "color", "fallback", "fields", "actions"))

  expect_equal(a$title, "Ran report 'minimal'")
  expect_match(a$text, "on server \\*myserver\\* in [0-9]+(.[0-9]+)? secs")
  expect_equal(a$color, "warning")
  expect_equal(a$fallback,
               sprintf("Ran 'minimal' as '%s'; view at %s", id, report_url))
  expect_equal(
    a$fields,
    list(
      list(title = "id", value = sprintf("`%s`", id), short = TRUE),
      list(title = "git", value = git_info, short = TRUE)))
  expect_equal(
    a$actions,
    list(list(
      name = "link", type = "button", text = ":clipboard: View report",
      style = "primary", url = report_url)))

  skip_if_no_internet()
  slack_url <- "https://httpbin.org/post"
  r <- do_post_success(slack_url, d, "slack")

  str <- httr::content(r, "text", encoding = "UTF-8")
  cmp <- jsonlite::fromJSON(str, FALSE)$json

  expect_setequal(names(d), names(cmp))
  expect_equal(d$username, cmp$username)
  expect_equal(d$icon_emoji, cmp$icon_emoji)
  expect_setequal(names(d$attachments[[1]]), names(cmp$attachments[[1]]))
})

test_that("teams payload is correct", {
  skip_if_not_installed("httr")
  skip_if_not_installed("jsonlite")
  server_url <- "https://example.com"
  server_is_primary <- FALSE
  server_name <- "myserver"
  dat <- list(meta = list(elapsed = 10,
                          id = "20181213-123456-fedcba98",
                          name = "example"),
              git = NULL)
  report_url <- sprintf("%s/reports/%s/%s/", server_url,
                        dat$meta$name, dat$meta$id)
  d <- teams_data(dat, server_name, report_url, server_is_primary)

  expect_equal(
    d,
    list(
      "@type" = "MessageCard",
      "@context" = "http://schema.org/extensions",
      summary = sprintf("Ran '%s' as '%s'; view at %s",
                        dat$meta$name, dat$meta$id, report_url),
      themeColor = "DAA038",
      sections = list(
        list(
          activityTitle = "Ran report 'example'",
          activityText = "on server *myserver* in 10 secs",
          activityImage = paste0("https://cdn.pixabay.com/photo/2017/",
                                 "06/10/07/18/list-2389219_960_720.png")
        ),
        list(
          facts = list(list(
            name = "id:",
            value = sprintf("`%s`", dat$meta$id)
          ))
        )
      ),
      potentialAction = list(list(
        "@type" = "OpenUri",
        name = "View report",
        targets = list(
          list(os = "default", uri = report_url)
        )
      ))
    ))

  skip_if_no_internet()
  steamsurl <- "https://httpbin.org/post"
  r <- do_post_success(steamsurl, d, "teams")

  str <- httr::content(r, "text", encoding = "UTF-8")
  cmp <- jsonlite::fromJSON(str, FALSE)$json

  expect_setequal(names(d), names(cmp))
  expect_equal(d$`@type`, cmp$`@type`)
  expect_equal(d$`@context`, cmp$`@context`)
})

test_that("teams payload is correct with git information", {
  skip_if_not_installed("httr")
  skip_if_not_installed("jsonlite")
  server_url <- "https://example.com"
  server_is_primary <- TRUE
  server_name <- "myserver"
  dat <- list(meta = list(elapsed = 10,
                          id = "20181213-123456-fedcba98",
                          name = "example"),
              git = list(branch = "master", sha_short = "abcdefg",
                         github_url = "https://github.com/vimc/repo"))
  report_url <- sprintf("%s/reports/%s/%s/", server_url,
                        dat$meta$name, dat$meta$id)
  d <- teams_data(dat, server_name, report_url, server_is_primary)

  expected_git_value <- paste0(
    "[master](https://github.com/vimc/repo/tree/master)@",
    "[abcdefg](https://github.com/vimc/repo/tree/abcdefg)")
  expect_equal(
    d,
    list(
      "@type" = "MessageCard",
      "@context" = "http://schema.org/extensions",
      summary = sprintf("Ran '%s' as '%s'; view at %s",
                        dat$meta$name, dat$meta$id, report_url),
      themeColor = "2EB886",
      sections = list(
        list(
          activityTitle = "Ran report 'example'",
          activityText = "on server *myserver* in 10 secs",
          activityImage = paste0("https://cdn.pixabay.com/photo/2017/",
                                 "06/10/07/18/list-2389219_960_720.png")
        ),
        list(
          facts = list(list(
            name = "id:",
            value = sprintf("`%s`", dat$meta$id)
          ),
          list(
            name = "git:",
            value = expected_git_value
          ))
        )
      ),
      potentialAction = list(list(
        "@type" = "OpenUri",
        name = "View report",
        targets = list(
          list(os = "default", uri = report_url)
        )
      ))
    ))

  skip_if_no_internet()
  teamsurl <- "https://httpbin.org/post"
  r <- do_post_success(teamsurl, d, "teams")

  str <- httr::content(r, "text", encoding = "UTF-8")
  cmp <- jsonlite::fromJSON(str, FALSE)$json

  expect_setequal(names(d), names(cmp))
  expect_equal(d$`@type`, cmp$`@type`)
  expect_equal(d$`@context`, cmp$`@context`)
})

test_that("main teams interface", {
  skip_if_no_internet()
  skip_if_not_installed("jsonlite")

  path <- prepare_orderly_example("minimal")
  id <- "20181213-123456-fedcba98"
  name <- "example"
  dat <- list(meta = list(elapsed = 10, id = id, name = name),
              git = NULL)

  path <- tempfile()
  dir.create(path)
  writeLines(
    c("remote:",
      "  myserver:",
      "    driver: orderly::orderly_remote_path",
      "    args:",
      paste("      path:", path),
      "    teams_url: https://httpbin.org/post",
      "    primary: true"),
    file.path(path, "orderly_config.yml"))

  config <- withr::with_envvar(
    c(ORDERLY_API_SERVER_IDENTITY = "myserver"),
    orderly_config$new(path))

  r <- post_success(dat, config)

  expect_equal(r$teams$status_code, 200L)
  res <- jsonlite::fromJSON(httr::content(r$teams)$data, FALSE)
  expect_equal(res$potentialAction[[1]]$targets[[1]]$uri,
               orderly_remote_path(path)$url_report(name, id))

  expect_null(r$slack)
})


test_that("look up environment variables before hook send", {
  skip_if_no_internet()
  skip_if_not_installed("jsonlite")

  path <- prepare_orderly_example("minimal")
  id <- "20181213-123456-fedcba98"
  name <- "example"
  dat <- list(meta = list(elapsed = 10, id = id, name = name),
              git = NULL)

  path <- tempfile()
  dir.create(path)
  writeLines(
    c("remote:",
      "  myserver:",
      "    driver: orderly::orderly_remote_path",
      "    args:",
      paste("      path:", path),
      "    slack_url: $ORDERLY_SLACK_URL",
      "    primary: true"),
    file.path(path, "orderly_config.yml"))

  env <- c(ORDERLY_API_SERVER_IDENTITY = "myserver",
           ORDERLY_SLACK_URL = "https://httpbin.org/post")

  config <- withr::with_envvar(env, orderly_config$new(path))

  r <- post_success(dat, config)
  expect_equal(r, list())

  r <- withr::with_envvar(env, post_success(dat, config))

  expect_equal(r$slack$status_code, 200L)
  res <- jsonlite::fromJSON(httr::content(r$slack)$data, FALSE)
  expect_equal(res$attachments[[1]]$actions[[1]]$url,
               orderly_remote_path(path)$url_report(name, id))

  expect_null(r$teams)
})
