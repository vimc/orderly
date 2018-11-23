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
  expect_error(file_copy(path1, path2, overwrite = FALSE),
               "Error copying files")
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
  config <- list(path = tempfile(),
                 vault_server = vaultr::vault_test_server()$url)

  x <- list(name = "alice",
            password = "VAULT:/secret/users/alice:password")
  vaultr::vault_clear_token_cache()
  withr::with_envvar(c(VAULTR_AUTH_METHOD = NA_character_), {
    expect_error(resolve_secrets(x, config),
                 "Have not authenticated against vault")
  })
  withr::with_envvar(c(VAULTR_AUTH_METHOD = "token", VAULT_TOKEN = NA), {
    expect_error(resolve_secrets(x, config), "token not found")
  })
  withr::with_envvar(c(VAULTR_AUTH_METHOD = "token", VAULT_TOKEN = "fake"), {
    expect_error(resolve_secrets(x, config),
                 "Token verification failed with code")
  })
  expect_equal(resolve_secrets(x, config),
               list(name = "alice", password = "ALICE"))
  expect_equal(resolve_secrets(unlist(x), config),
               list(name = "alice", password = "ALICE"))
})

test_that("resolve secret env", {
  ## This the pattern that we have during startup
  skip_if_no_vault_server()
  vaultr::vault_clear_token_cache()
  config <- list(path = tempfile(),
                 vault_server = vaultr::vault_test_server()$url)

  x <- list(user = "$ORDERLY_USER",
            password = "$ORDERLY_PASSWORD",
            other = "string")

  vars <- c("ORDERLY_PASSWORD"="VAULT:/secret/users/alice:password",
            "ORDERLY_USER"="alice")

  res <- withr::with_envvar(vars, resolve_driver_config(x, config))
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
  expect_true(all(c("sha_short", "sha", "branch", "status") %in% names(info)))

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


test_that("git_clean_url", {
  expect_null(git_clean_url(NULL))
  expect_equal(git_clean_url("git@github.com:foo/bar.git"),
               "https://github.com/foo/bar")
})


test_that("canonical case - redundant paths", {
  expect_true(file_has_canonical_case("../../README.md"))
  expect_true(file_has_canonical_case("../..//README.md"))
  expect_true(file_has_canonical_case("../..///README.md"))
})


test_that("platform detection", {
  expect_equal(is_windows(), Sys.info()[["sysname"]] == "Windows")
  expect_equal(is_linux(), Sys.info()[["sysname"]] == "Linux")
})

test_that("canonical case: single file", {
  root <- tempfile()
  dir.create(root)
  path <- "a"
  PATH <- toupper(path)
  full <- file.path(root, path)

  dir.create(dirname(full), FALSE, TRUE)
  file.create(full)

  withr::with_dir(root, {
    expect_true(file_has_canonical_case(path))
    expect_equal(file_canonical_case(path), path)
    expect_true(file_exists(path))
    expect_true(file_exists(path, check_case = TRUE))

    expect_false(file_has_canonical_case(PATH))
    expect_equal(file_canonical_case(PATH), path)
  })

  expect_true(file_exists(path, check_case = FALSE, workdir = root))
  expect_true(file_exists(path, check_case = TRUE, workdir = root))

  expect_true(file_exists(PATH, check_case = FALSE, workdir = root))
  expect_false(file_exists(PATH, check_case = TRUE, workdir = root))

  v <- file_exists(PATH, check_case = TRUE, workdir = root)
  expect_identical(attr(v, "incorrect_case"), TRUE)
  expect_equal(attr(v, "correct_case"), set_names(path, PATH))
})


test_that("canonical case: relative path", {
  root <- tempfile()
  dir.create(root)
  path <- file.path("a", "b", "c")
  PATH <- toupper(path)
  full <- file.path(root, path)

  dir.create(dirname(full), FALSE, TRUE)
  file.create(full)

  withr::with_dir(root, {
    expect_true(file_has_canonical_case(path))
    expect_equal(file_canonical_case(path), path)
    expect_true(file_exists(path))
    expect_true(file_exists(path, check_case = TRUE))

    expect_false(file_has_canonical_case(PATH))
    expect_equal(file_canonical_case(PATH), path)
  })

  expect_true(file_exists(path, check_case = FALSE, workdir = root))
  expect_true(file_exists(path, check_case = TRUE, workdir = root))

  expect_true(file_exists(PATH, check_case = FALSE, workdir = root))
  expect_false(file_exists(PATH, check_case = TRUE, workdir = root))

  v <- file_exists(PATH, check_case = TRUE, workdir = root)
  expect_identical(attr(v, "incorrect_case"), TRUE)
  expect_equal(attr(v, "correct_case"), set_names(path, PATH))
})


test_that("canonical case: absolute path", {
  path <- file.path(tempfile(), "a", "b", "c")
  dir.create(dirname(path), FALSE, TRUE)
  file.create(path)
  path <- normalizePath(path)
  PATH <- toupper(path)

  expect_true(file_has_canonical_case(path))
  expect_equal(file_canonical_case(path), path)
  expect_true(file_exists(path))
  expect_true(file_exists(path, check_case = TRUE))

  expect_false(file_has_canonical_case(PATH))
  expect_equal(file_canonical_case(PATH), path)

  expect_true(file_exists(path, check_case = FALSE))
  expect_true(file_exists(path, check_case = TRUE))

  expect_true(file_exists(PATH, check_case = FALSE))
  expect_false(file_exists(PATH, check_case = TRUE))

  v <- file_exists(PATH, check_case = TRUE)
  expect_identical(attr(v, "incorrect_case"), TRUE)
  expect_equal(attr(v, "correct_case"), set_names(path, PATH))
})


test_that("abbreviate", {
  expect_equal(abbreviate("12345678", 5), "12...")
  expect_equal(abbreviate("12345678", 7), "1234...")
  expect_equal(abbreviate("12345678", 8), "12345678")
  expect_equal(abbreviate("12345678", 10), "12345678")

  expect_equal(abbreviate("12345\n678", 10), "12345")
  expect_equal(abbreviate("12345\n678", 4), "1...")
})


test_that("Sys_getenv", {
  withr::with_envvar(
    c("SOME_VAR" = NA_character_), {
      expect_error(Sys_getenv("SOME_VAR"),
                   "Environment variable 'SOME_VAR' is not set")
      expect_null(Sys_getenv("SOME_VAR", FALSE))
      expect_identical(Sys_getenv("SOME_VAR", FALSE, NA_character_),
                       NA_character_)
    })
})


## I'm not sure there's anything super safe to run this with in
## general...
test_that("sys_which", {
  prog <- "a-path-that-does-not-exist"
  expect_error(sys_which(prog),
               "Did not find 'a-path-that-does-not-exist'")
})


test_that("zip_dir", {
  path <- tempfile()
  expect_error(zip_dir(path), "error running zip")
})


test_that("open_directory: windows", {
  mockery::stub(open_directory, "system2", list)
  mockery::stub(open_directory, "is_windows", TRUE)

  expect_equal(open_directory("."),
               list("cmd", c("/c", "start", "explorer", ".")))
  p <- normalizePath(".")
  expect_equal(open_directory(normalizePath(p)),
               list("cmd", c("/c", "start", "explorer", p)))
})


test_that("open_directory: linux", {
  mockery::stub(open_directory, "system2", list)
  mockery::stub(open_directory, "is_windows", FALSE)
  mockery::stub(open_directory, "is_linux", TRUE)

  expect_equal(open_directory("."), list("xdg-open", "."))
  p <- normalizePath(".")
  expect_equal(open_directory(normalizePath(p)), list("xdg-open", p))
})


test_that("open_directory: mac", {
  mockery::stub(open_directory, "system2", list)
  mockery::stub(open_directory, "is_windows", FALSE)
  mockery::stub(open_directory, "is_linux", FALSE)

  expect_equal(open_directory("."), list("open", "."))
  p <- normalizePath(".")
  expect_equal(open_directory(normalizePath(p)), list("open", p))
})


test_that("open_directory: error", {
  expect_error(open_directory(tempfile()), "Expected a directory")
  expect_error(open_directory("test-util.R"), "Expected a directory")
})


test_that("copy_directory failure", {
  a <- tempfile()
  b <- tempfile()
  dir.create(a, FALSE, TRUE)
  file.create(file.path(a, "file1"))
  file.create(file.path(a, "file2"))

  mockery::stub(copy_directory, "file.copy", c(TRUE, FALSE))
  expect_error(copy_directory(a, b), "Error copying files")
})


test_that("copy_directory rollback", {
  a <- tempfile()
  b <- tempfile()
  dir.create(a, FALSE, TRUE)
  file.create(file.path(a, "file1"))
  file.create(file.path(a, "file2"))

  mockery::stub(copy_directory, "file.copy", c(TRUE, FALSE))
  expect_error(copy_directory(a, b, TRUE), "Error copying files")
  expect_false(file.exists(b))
})


test_that("copy_directory rollback needs missing destination", {
  a <- tempfile()
  b <- tempfile()
  dir.create(a, FALSE, TRUE)
  file.create(file.path(a, "file1"))
  file.create(file.path(a, "file2"))
  dir.create(b, FALSE, TRUE)
  expect_error(copy_directory(a, b, TRUE),
               "Destination cannot already exist")
})


test_that("ordered_map_to_list", {
  expect_equal(ordered_map_to_list(yaml_load("- a: 1\n- b: 2")),
               list(a = 1, b = 2))

  ## The yaml parser will catch this sort of thing
  expect_error(yaml_load("- a: 1\n- b: 2\n c: 3"))

  ## but if it came through it would be as
  d <- list(list(a = 1), list(b = 2, c = 3))
  expect_error(ordered_map_to_list(d),
               "Corrupt ordered map (this should never happen)",
               fixed = TRUE)
})
