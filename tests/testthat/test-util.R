context("utils")

test_that("yaml_read throws nicely", {
  expect_error(suppressWarnings(yaml_read("foo")), "while reading 'foo'")
})

test_that("string symbol parse", {
  expect_equal(check_symbol_from_str("a::b"), c("a", "b"))
  expect_error(check_symbol_from_str("a", "name"),
               "Expected fully qualified name for name")
  expect_error(check_symbol_from_str("a::b::c", "name"),
               "Expected fully qualified name for name")
})

test_that("Descend failure", {
  path <- tempfile()
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE))
  expect_null(find_file_descend(".orderly_foobar", tempdir(), path))
  expect_null(find_file_descend(".orderly_foobar", "/", path))
  expect_null(find_file_descend(".orderly_foobar", "/", "/"))
})

test_that("copy failure", {
  path1 <- tempfile()
  path2 <- tempfile()
  writeLines("a", path1)
  writeLines("b", path2)
  on.exit(file.remove(path1, path2))
  expect_error(file_copy(path1, path2), "Error copying files")
  expect_equal(readLines(path2), "b")
})

test_that("resolve_env", {
  set.seed(1)
  v <- paste(sample(c(LETTERS, 0:9, "_"), 20, replace = TRUE), collapse = "")
  vv <- paste0("$", v)
  expect_identical(resolve_env(v), list(v))
  expect_error(resolve_env(vv),
               sprintf("Environment variable '%s' is not set", v))
  expect_identical(withr::with_envvar(setNames("value", v), resolve_env(vv)),
                   list("value"))
})

test_that("val_to_bytes", {
  x <- sort(runif(10, 0.1, 0.15)) + 100
  y <- vcapply(x, val_to_bytes, 2)
  expect_equal(sort_c(y), y)
  expect_true(all(nchar(y)), 4)
})

test_that("new_report_id", {
  t <- Sys.time()
  i1 <- new_report_id(t)
  i2 <- new_report_id(t)
  i3 <- t + (1 / 256^2)
  expect_identical(substr(i1, 1, 20), substr(i2, 1, 20))
  expect_false(substr(i1, 1, 20) == substr(i3, 1, 20))
})

test_that("secrets", {
  skip_if_no_vault_server()
  x <- list(name = "alice",
            password = "VAULT:/secret/users/alice:password")
  vaultr::vault_clear_token_cache()
  withr::with_envvar(c(VAULTR_AUTH_METHOD = NA_character_), {
    expect_error(resolve_secrets(x, NULL),
                 "Have not authenticated against vault")
  })
  withr::with_envvar(c(VAULTR_AUTH_METHOD = "token", VAULT_TOKEN = NA), {
    expect_error(resolve_secrets(x, NULL), "token not found")
  })
  withr::with_envvar(c(VAULTR_AUTH_METHOD = "token", VAULT_TOKEN = "fake"), {
    expect_error(resolve_secrets(x, NULL), "Token verification failed with code")
  })
  expect_equal(resolve_secrets(x, NULL),
               list(name = "alice", password = "ALICE"))
  expect_equal(resolve_secrets(unlist(x), NULL),
               list(name = "alice", password = "ALICE"))
})

test_that("resolve secret env", {
  ## This the pattern that we have during startup
  skip_if_no_vault_server()
  vaultr::vault_clear_token_cache()

  x <- list(user = "$ORDERLY_USER",
            password = "$ORDERLY_PASSWORD",
            other = "string")

  vars <- c("ORDERLY_PASSWORD"="VAULT:/secret/users/alice:password",
            "ORDERLY_USER"="alice")

  res <- withr::with_envvar(vars, resolve_driver_config(x, NULL))
  expect_equal(res,
               list(user = "alice", password = "ALICE", other = "string"))
})

test_that("which_max_time", {
  times <- as.list(Sys.time() + sort(rnorm(3, 0, 20)))
  expect_equal(which_max_time(times), 3)
  expect_equal(which_max_time(times[c(1, 3, 2)]), 2)
})

test_that("git", {
  skip_if_no_git()
  skip_on_appveyor() # needs some git work

  path <- tempfile()
  code <- system2("git", c("init", path), stdout = FALSE, stderr = FALSE)
  if (code != 0) {
    skip("git not working nicely")
  }

  txt <- file.path(path, "txt")
  writeLines("orderly", txt)
  git_info_call(path, c("add", txt))
  git_info_call(path, c("commit", "-m", "initial"))

  info <- git_info(path)
  expect_true(setequal(names(info),
                       c("sha_short", "sha", "branch", "status")))

  expect_equal(info$branch, "master")
  expect_null(info$status)
  expect_match(info$sha_short, "^[[:xdigit:]]{7}$")
  expect_match(info$sha, "^[[:xdigit:]]{40}$")

  writeLines("orderly2", txt)
  info2 <- git_info(path)
  expect_equal(info2$status, " M txt")
  expect_equal(info2[c("sha_short", "sha", "branch")],
               info[c("sha_short", "sha", "branch")])
})

test_that("canonical case - redundant paths", {
  skip("flakey")
  expect_true(file_has_canonical_case("../../README.md"))
  expect_true(file_has_canonical_case("../..//README.md"))
  expect_true(file_has_canonical_case("../..///README.md"))
})
