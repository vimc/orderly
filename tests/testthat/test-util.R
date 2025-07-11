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


test_that("move failure", {
  path1 <- tempfile()
  path2 <- file.path(tempfile(), "dest")
  writeLines("a", path1)
  expect_error(suppressWarnings(file_move(path1, path2)),
               "Error moving files")
})


test_that("resolve_env", {
  set.seed(1)
  v <- paste(sample(c(LETTERS, 0:9, "_"), 20, replace = TRUE), collapse = "")
  vv <- paste0("$", v)
  expect_identical(resolve_env(c(x = v), "loc"), list(x = v))
  expect_error(
    resolve_env(c(x = vv), "loc"),
    sprintf("Environment variable '%s' is not set.*used in loc:x", v))
  expect_identical(
    withr::with_envvar(setNames("value", v), resolve_env(c(x = vv), "loc")),
    list(x = "value"))
})


test_that("resolve_env skips non-scalars", {
  set.seed(1)
  v <- paste(sample(c(LETTERS, 0:9, "_"), 20, replace = TRUE), collapse = "")
  vv <- paste0("$", v)
  expect_identical(resolve_env(c(x = v), "loc"), list(x = v))

  env <- setNames("value", v)
  expect_identical(
    withr::with_envvar(
      env,
      resolve_env(list(a = vv, b = list(a = 1, b = 2)), "loc")),
    list(a = "value", b = list(a = 1, b = 2)))
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
  srv <- vaultr::vault_test_server()
  cl <- srv$client()
  cl$write("/secret/users/alice", list(password = "ALICE"))
  cl$write("/secret/users/bob", list(password = "BOB"))

  config <- list(root = tempfile(),
                 vault = list(addr = srv$addr))

  x <- list(name = "alice",
            password = "VAULT:/secret/users/alice:password")
  withr::with_envvar(c(VAULTR_AUTH_METHOD = NA_character_), {
    expect_error(resolve_secrets(x, config),
                 "Default login method not set in 'VAULTR_AUTH_METHOD'")
  })
  withr::with_envvar(c(VAULTR_AUTH_METHOD = "token", VAULT_TOKEN = NA), {
    expect_error(resolve_secrets(x, config), "Vault token was not found")
  })
  withr::with_envvar(c(VAULTR_AUTH_METHOD = "token", VAULT_TOKEN = "fake"), {
    expect_error(resolve_secrets(x, config),
                 "Token login failed with error")
  })

  withr::with_envvar(c(VAULTR_AUTH_METHOD = "token", VAULT_TOKEN = srv$token), {
    expect_equal(resolve_secrets(x, config),
                 list(name = "alice", password = "ALICE"))
    expect_equal(resolve_secrets(unlist(x), config),
                 list(name = "alice", password = "ALICE"))
  })
})

test_that("secets and expanded vault definition", {
  srv <- vaultr::vault_test_server()
  cl <- srv$client()
  cl$write("/secret/users/alice", list(password = "ALICE"))
  cl$write("/secret/users/bob", list(password = "BOB"))

  config <- list(root = tempfile(),
                 vault = list(
                   addr = srv$addr,
                   login = "token",
                   token = srv$token))
  x <- list(name = "alice",
            password = "VAULT:/secret/users/alice:password")
  expect_equal(resolve_secrets(x, config),
               list(name = "alice", password = "ALICE"))
})

test_that("resolve secret env", {
  srv <- vaultr::vault_test_server()
  cl <- srv$client()
  cl$write("/secret/users/alice", list(password = "ALICE"))
  cl$write("/secret/users/bob", list(password = "BOB"))

  config <- list(root = tempfile(),
                 vault = list(addr = srv$addr,
                              login = "token",
                              token = srv$token))

  x <- list(user = "$ORDERLY_USER",
            password = "$ORDERLY_PASSWORD",
            other = "string")

  vars <- c("ORDERLY_PASSWORD" = "VAULT:/secret/users/alice:password",
            "ORDERLY_USER" = "alice")

  res <- withr::with_envvar(vars, resolve_driver_config(x, config))
  expect_equal(res,
               list(user = "alice", password = "ALICE", other = "string"))
})


test_that("vault configuration honours environment variables", {
  srv <- vaultr::vault_test_server()
  cl <- srv$client()
  cl$write("/secret/users/alice", list(password = "ALICE"))
  cl$write("/secret/users/bob", list(password = "BOB"))

  path <- test_prepare_orderly_example("minimal")
  path_config <- file.path(path, "orderly_config.yml")
  text <- readLines(path_config)

  text <- c(text, c("vault:",
                    "  addr: $ORDERLY_VAULT_ADDR",
                    "  login: token",
                    "  token: $ORDERLY_VAULT_TOKEN"))
  writeLines(text, path_config)

  x <- list(name = "alice",
            password = "VAULT:/secret/users/alice:password")
  config <- orderly_config_$new(path)
  ## Environment variable not resolved yet:
  expect_equal(config$vault$addr, "$ORDERLY_VAULT_ADDR")
  ## Sensible error if not set:
  expect_error(
    resolve_secrets(x, config),
    paste0("Environment variable 'ORDERLY_VAULT_ADDR' is not set.*",
           "used in orderly_config.yml:vault:addr"))
  ## Resolve if set
  env <- list(ORDERLY_VAULT_ADDR = srv$addr, ORDERLY_VAULT_TOKEN = srv$token)
  yaml_write(env, file.path(path, "orderly_envir.yml"))
  expect_equal(resolve_secrets(x, config),
               list(name = "alice", password = "ALICE"))
})


test_that("which_max_time", {
  times <- as.list(Sys.time() + sort(rnorm(3, 0, 20)))
  expect_equal(which_max_time(times), 3)
  expect_equal(which_max_time(times[c(1, 3, 2)]), 2)
})

test_that("git", {
  skip_on_ci() # Failing on CI, not fixing because package is being retired
  skip_if_no_git()
  skip_on_windows_ci()

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

  # expect_equal(info$branch, "master")
  expect_null(info$status)
  expect_match(info$sha_short, "^[[:xdigit:]]{7}$")
  expect_match(info$sha, "^[[:xdigit:]]{40}$")

  writeLines("orderly2", txt)
  info2 <- git_info(path)
  expect_equal(info2$status, " M txt")
  expect_equal(info2[c("sha_short", "sha", "branch")],
               info[c("sha_short", "sha", "branch")])
})

test_that("git_info non-cran", {
  skip_if_no_git()

  path <- unzip_git_demo()

  withr::with_options(
    list(orderly.nogit = TRUE),
    expect_null(git_info(path)))

  res <- withr::with_options(
    list(orderly.nogit = FALSE),
    git_info(path))
  expect_is(res, "list")
  expect_is(res$branch, "character")
})

test_that("git_clean_url", {
  expect_null(git_clean_url(NULL))
  expect_equal(git_clean_url("git@github.com:foo/bar.git"),
               "https://github.com/foo/bar")
})

test_that("platform detection", {
  expect_equal(is_windows(), Sys.info()[["sysname"]] == "Windows")
  expect_equal(is_linux(), Sys.info()[["sysname"]] == "Linux")
})

test_that("canonical case: single file", {
  ## There are issues with either mocking or system calls for
  ## canonical case checking on solaris, but as it is case-sensitive
  ## the tests are not important.
  skip_on_solaris()
  root <- tempfile()
  dir.create(root)
  path <- "a"
  PATH <- toupper(path) # nolint
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

  expect_false(file_exists(PATH, check_case = TRUE, workdir = root))

  if (is_linux()) {
    mockery::stub(file_exists, "file.exists", TRUE)
  }
  expect_true(file_exists(PATH, check_case = FALSE, workdir = root))
  v <- file_exists(PATH, check_case = TRUE, workdir = root,
                   force_case_check = TRUE)
  expect_identical(attr(v, "incorrect_case"), TRUE)
  expect_equal(attr(v, "correct_case"), set_names(path, PATH))
})


test_that("canonical case: relative path", {
  skip_on_solaris() # See above
  root <- tempfile()
  dir.create(root)
  path <- file.path("a", "b", "c")
  PATH <- toupper(path) # nolint
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

  expect_false(file_exists(PATH, check_case = TRUE, workdir = root))

  if (is_linux()) {
    mockery::stub(file_exists, "file.exists", TRUE)
  }

  expect_true(file_exists(PATH, check_case = FALSE, workdir = root))
  v <- file_exists(PATH, check_case = TRUE, workdir = root,
                   force_case_check = TRUE)
  expect_identical(attr(v, "incorrect_case"), TRUE)
  expect_equal(attr(v, "correct_case"), set_names(path, PATH))
})


test_that("canonical case: absolute path", {
  skip_on_solaris() # See above
  path <- file.path(tempfile(), "a", "b", "c")
  dir.create(dirname(path), FALSE, TRUE)
  file.create(path)
  path <- normalizePath(path, "/")
  PATH <- toupper(path) # nolint
  if (is_windows()) {
    ## On windows, use upper case drive letters here:
    path <- paste0(toupper(substr(path, 1, 1)),
                   substr(path, 2, nchar(path)))
  }

  expect_true(file_has_canonical_case(path))
  expect_equal(file_canonical_case(path), path)
  expect_true(file_exists(path))
  expect_true(file_exists(path, check_case = TRUE))

  expect_false(file_has_canonical_case(PATH))
  expect_equal(file_canonical_case(PATH), path)

  expect_true(file_exists(path, check_case = FALSE))
  expect_true(file_exists(path, check_case = TRUE))

  expect_false(file_exists(PATH, check_case = TRUE))

  if (is_linux()) {
    mockery::stub(file_exists, "file.exists", TRUE)
  }
  expect_true(file_exists(PATH, check_case = FALSE))

  v <- file_exists(PATH, check_case = TRUE, force_case_check = TRUE)
  expect_identical(attr(v, "incorrect_case"), TRUE)
  expect_equal(attr(v, "correct_case"), set_names(path, PATH))
})


test_that("canonical case: path splitting", {
  skip_on_solaris() # See above
  expect_equal(file_split_base("a/b/c"),
               list(path = c("a", "b", "c"), base = ".", absolute = FALSE))
  expect_equal(file_split_base("/a/b/c"),
               list(path = c("a", "b", "c"), base = "/", absolute = TRUE))
  expect_equal(file_split_base("c:/a/b/c"),
               list(path = c("a", "b", "c"), base = "c:/", absolute = TRUE))
  expect_equal(file_split_base("C:/a/b/c"),
               list(path = c("a", "b", "c"), base = "C:/", absolute = TRUE))
})


test_that("canonical case: on missing file", {
  skip_on_solaris() # See above
  expect_equal(file_canonical_case("test-util.R"), "test-util.R")
  expect_identical(file_canonical_case("another file"), NA_character_)
})


test_that("abbreviate", {
  expect_equal(abbreviate("12345678", 5), "12...")
  expect_equal(abbreviate("12345678", 7), "1234...")
  expect_equal(abbreviate("12345678", 8), "12345678")
  expect_equal(abbreviate("12345678", 10), "12345678")

  expect_equal(abbreviate("12345\n678", 10), "12345")
  expect_equal(abbreviate("12345\n678", 4), "1...")
})


test_that("sys_getenv", {
  withr::with_envvar(
    c("SOME_VAR" = NA_character_), {
      expect_error(
        sys_getenv("SOME_VAR", "loc"),
        "Environment variable 'SOME_VAR' is not set.*used in loc")
      expect_null(sys_getenv("SOME_VAR", "loc", FALSE))
      expect_identical(sys_getenv("SOME_VAR", "loc", FALSE, NA_character_),
                       NA_character_)
    })

  ## On windows if env variable is empty then windows will return NA from call
  ## to Sys.getenv
  if (is_windows()) {
    expected_err <- "Environment variable 'SOME_VAR' is not set.*used in loc"
  } else {
    expected_err <- "Environment variable 'SOME_VAR' is empty.*used in loc"
  }

  withr::with_envvar(
    c("SOME_VAR" = ""),
    expect_error(
      sys_getenv("SOME_VAR", "loc"),
      expected_err))
  withr::with_envvar(
    c("SOME_VAR" = "x"),
    expect_identical(sys_getenv("SOME_VAR", "loc"), "x"))
})


## I'm not sure there's anything super safe to run this with in
## general...
test_that("sys_which", {
  prog <- "a-path-that-does-not-exist"
  expect_error(sys_which(prog),
               "Did not find 'a-path-that-does-not-exist'")
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

test_that("handle_missing_packages", {
  ## These packages don't exist so don't even try to install them to avoid
  ## CI pain
  mockery::stub(handle_missing_packages, "install_missing_packages", TRUE)
  ## we test show_question later
  mockery::stub(handle_missing_packages, "show_question", FALSE)

  expect_error(handle_missing_packages(c("foo", "bar"), TRUE),
               NA)

  expect_error(handle_missing_packages(c("foo", "bar"), FALSE),
               "Missing packages: 'foo', 'bar'")
})

test_that("install_missing_packages", {
  ## These packages don't exist so don't even try to install them to avoid
  ## CI pain
  mockery::stub(install_missing_packages, "install_packages", TRUE)

  ## scenario 1: user asks to install the packages and succeed
  mockery::stub(install_missing_packages, "prompt_ask_yes_no", TRUE)
  expect_error(install_missing_packages(c("foo", "bar")),
               NA)
  ## scenario 2: user asks to not install the packages
  mockery::stub(install_missing_packages, "prompt_ask_yes_no", FALSE)
  expect_error(install_missing_packages(c("foo", "bar")),
               "Missing packages: 'foo', 'bar'")
})

test_that("install_packages", {
  ## this test assumes there will never be packages called foo and bar
  ## installed on the testing machine
  mockery::stub(install_packages, "utils::install.packages", TRUE)
  expect_error(install_packages(c("foo", "bar")),
               "Could not install these packages: 'foo', 'bar'")
})

test_that("show_question", {
  oo <- options(orderly.nolog = NULL) # default setting to start
  on.exit(options(oo))

  orderly_log_off()
  expect_identical(show_question(), FALSE)
})

test_that("show_question interactive", {
  withr::with_options(
    list(orderly.nolog = NULL),
    expect_equal(show_question(), interactive()))

  mockery::stub(show_question, "interactive", TRUE)
  withr::with_options(
    list(orderly.nolog = NULL),
    expect_equal(show_question(), TRUE))
  withr::with_options(
    list(orderly.nolog = TRUE),
    expect_equal(show_question(), FALSE))
})


test_that("backup db", {
  skip_on_cran_windows()
  list_tables <- function(path) {
    with_sqlite(path, function(con) DBI::dbListTables(con))
  }

  path <- create_orderly_demo()
  path_db <- file.path(path, "orderly.sqlite")
  dest <- file.path(path, "backup.sqlite")
  dest_prev <- paste0(dest, ".prev")

  sqlite_backup(path_db, dest)
  expect_true(file.exists(dest))
  expect_setequal(list_tables(path_db), list_tables(dest))

  sqlite_backup(path_db, dest)
  expect_true(file.exists(dest))
  expect_true(file.exists(dest_prev))
  expect_setequal(list_tables(path_db), list_tables(dest))
  expect_setequal(list_tables(path_db), list_tables(dest_prev))
})


test_that("pretty_bytes", {
  skip_on_cran_windows()
  expect_equal(pretty_bytes(0), "0 B")
  expect_equal(pretty_bytes(1), "1 B")
  expect_equal(pretty_bytes(11), "11 B")
  expect_equal(pretty_bytes(111), "111 B")
  expect_equal(pretty_bytes(1111), "1.11 kB")
  expect_equal(pretty_bytes(11111), "11.11 kB")
  expect_equal(pretty_bytes(111111), "111.11 kB")
  expect_equal(pretty_bytes(1111111), "1.11 MB")
  expect_equal(pretty_bytes(11111111), "11.11 MB")
  expect_equal(pretty_bytes(111111111), "111.11 MB")
  expect_equal(pretty_bytes(1111111111), "1.11 GB")
  expect_equal(pretty_bytes(11111111111), "11.11 GB")
  expect_equal(pretty_bytes(111111111111), "111.11 GB")
})


test_that("orderly_env picks up ORDERLY variables", {
  env <- c("ORDERLY_A" = "a", "ORDERLY_B" = "b")
  v <- withr::with_envvar(
    env,
    orderly_env())
  expect_true(all(names(env) %in% names(v)))
  expect_equal(v[names(env)], as.list(env))
})


test_that("orderly_env excludes sensitive data", {
  env1 <- c("ORDERLY_SERVER_PASSWORD" = "passw0rd",
           "ORDERLY_SERVER_TOKEN" = "secr7et",
           "ORDERLY_GITHUB_PAT" = "pat")
  env2 <- c("ORDERLY_A" = "a", "ORDERLY_B" = "b")
  v <- withr::with_envvar(
    c(env1, env2),
    orderly_env())
  expect_false(any(names(env1) %in% names(v)))
  expect_true(all(names(env2) %in% names(v)))
  expect_equal(v[names(env2)], as.list(env2))
})


test_that("clean_path", {
  expect_equal(clean_path("c:/My Documents/Projects/whatever"),
               "c:/My Documents/Projects/whatever")
  expect_equal(clean_path("c:\\My Documents/Projects\\whatever"),
               "c:/My Documents/Projects/whatever")
})


test_that("random_seed", {
  e1 <- new.env(parent = emptyenv())
  e2 <- new.env(parent = e1)
  e1[[".Random.seed"]] <- pi
  expect_equal(random_seed(e1), pi)
  expect_null(random_seed(e2))
})


## TODO: add tests of some of the comment processing
test_that("yaml_block_info - simple test", {
  yml <- c("a:", "  - 1", "  - 2", "b:", "    3", "c: 4")
  expect_equal(yaml_load(yml), list(a = 1:2, b = 3, c = 4))

  expect_equal(yaml_block_info("a", yml),
               list(name = "a", exists = TRUE, block = TRUE,
                    start = 1L, end = 3L, indent = "  "))
  expect_equal(yaml_block_info("b", yml),
               list(name = "b", exists = TRUE, block = TRUE,
                    start = 4L, end = 5L, indent = "    "))
  expect_equal(yaml_block_info("c", yml),
               list(name = "c", exists = TRUE, block = FALSE,
                    start = 6L, end = 6L))
  expect_equal(yaml_block_info("d", yml),
               list(name = "d", exists = FALSE, block = FALSE))
})


test_that("yaml parse failure", {
  text <- c("a: 1", "a: 2")
  expect_error(yaml_load(text))
  expect_error(yaml_block_info("a", text), "Failed to process yaml")
})


test_that("insert into files", {
  text <- c("a", "b", "c", "d", "e")
  value <- c("x", "y")
  where <- 3
  path <- tempfile()
  writeLines(text, path)

  withr::with_envvar(c("NO_COLOR" = "true"), {
    res <- evaluate_promise(
      insert_into_file(text, where, value, path,
                       show = TRUE, edit = FALSE, prompt = FALSE))
  })
  expect_match(res$messages, "Changes to '.+'")
  expect_equal(res$result, filediff(text, where, value))
  expect_equal(res$output,
               "  2 | b\n  3 | c\n+ 4 | x\n+ 5 | y\n  6 | d\n  7 | e")

  expect_equal(readLines(path), res$result$text) # unchanged

  res <- evaluate_promise(
    insert_into_file(text, where, value, path,
                     show = FALSE, edit = TRUE, prompt = FALSE))
  expect_equal(res$output, "")
  expect_equal(res$result, filediff(text, where, value))
  expect_equal(readLines(path), res$result$result)
})


test_that("prompting prevents write", {
  skip_if_not_installed("mockery")
  text <- c("a", "b", "c")
  value <- c("x", "y")
  path <- tempfile()
  writeLines(text, path)
  mockery::stub(insert_into_file, "prompt_ask_yes_no", FALSE)
  expect_message(
    res <- insert_into_file(text, 2, value, path,
                            show = FALSE, edit = TRUE, prompt = TRUE),
    "Not modifying file")
  expect_equal(res, filediff(text, 2, value))
  expect_equal(readLines(path), res$text) # unchanged
})


test_that("format filediff", {
  text <- c("a", "b", "c", "d", "e")
  value <- c("x", "y")
  where <- 3
  fd <- filediff(text, where, value)

  str1 <- format_filediff(fd, colour = FALSE)
  expect_equal(str1, "  2 | b\n  3 | c\n+ 4 | x\n+ 5 | y\n  6 | d\n  7 | e\n")
  expect_false(crayon::has_style(str1))

  str2 <- withr::with_options(
    list(crayon.enabled = TRUE),
    format_filediff(fd, colour = TRUE))
  expect_true(crayon::has_style(str2))

  withr::with_options(
    list(crayon.enabled = FALSE),
    expect_equal(format_filediff(fd), str1))
  withr::with_options(
    list(crayon.enabled = TRUE),
    expect_equal(format_filediff(fd), str2))
})


test_that("don't write with no changes", {
  text <- letters
  where <- 26
  value <- character(0)

  p <- tempfile()
  expect_message(
    res <- insert_into_file(text, where, value, p, FALSE, TRUE, FALSE),
    "No changes to make to")

  expect_equal(res$changed, numeric(0))
  expect_equal(format_filediff(res), character(0))
  expect_false(file.exists(p))
})


test_that("palette", {
  withr::with_options(
    list(crayon.enabled = TRUE), {
      expect_equal(orderly_style("highlight")("x"),
                   crayon::bold(crayon::make_style("steelblue3")("x")))
      expect_equal(orderly_style("alert")("x"),
                   crayon::bold(crayon::make_style("hotpink")("x")))
      expect_equal(orderly_style("fade")("x"),
                   crayon::make_style("grey")("x"))
      expect_equal(orderly_style("other")("x"), "x")
      expect_equal(orderly_style("workflow")("x"),
                   crayon::bold(crayon::make_style("orange2")("x")))
   })
})

test_that("can conditionally capture logs", {
  fun <- function() {
    message("Test")
  }
  t <- tempfile()
  expect_message(conditional_capture_log(FALSE, t, fun()), "Test")
  expect_false(file.exists(t))

  expect_silent(conditional_capture_log(TRUE, t, fun()))
  expect_true(file.exists(t))
  expect_equal(readLines(t), "Test")

  ## Logfile can be appended to
  expect_silent(conditional_capture_log(TRUE, t, fun()))
  expect_equal(readLines(t), c("Test", "Test"))
})

test_that("lock_bindings can lock multiple variables at once", {
  test_class <- R6::R6Class(
    "test_class",

    public = list(
      a = "a",
      b = "b",
      initialize = function() {
        lock_bindings(c("a", "b"), self)
      }
    )
  )

  obj <- test_class$new()
  expect_error(obj$a <- "2", "cannot change value of locked binding for 'a'")
  expect_error(obj$b <- "2", "cannot change value of locked binding for 'b'")
})

test_that("clean_report_name strips slashes and src dir", {
  expect_equal(clean_report_name("test"), "test")
  expect_equal(clean_report_name("test/"), "test")
  expect_equal(clean_report_name("src/test"), "test")
})


test_that("canonical case ignores missing windows truncated elements", {
  ## There are issues with either mocking or system calls for
  ## canonical case checking on solaris, but as it is case-sensitive
  ## the tests are not important.
  skip_on_solaris()
  skip_if_not_installed("mockery")
  mock_dir <- mockery::mock(c("aaa", "aax"),
                            c("bbb", "bbx"),
                            c("ccc", "ccx"),
                            cycle = TRUE)
  mockery::stub(file_canonical_case, "dir", mock_dir)

  expect_equal(file_canonical_case("aaa/bbb~1/ccc"),
               "aaa/bbb~1/ccc")
  expect_equal(file_canonical_case("aaa/BBB~1/ccc"),
               "aaa/BBB~1/ccc")
})


test_that("git_info skips where .git not found", {
  skip_on_cran()
  skip_if_not_installed("mockery")
  mock_git_info_call <- mockery::mock(NULL, cycle = TRUE)
  mockery::stub(git_info, "git_info_call", mock_git_info_call)

  root <- tempfile()
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  withr::with_options(
    list(orderly.nogit = FALSE),
    expect_null(git_info(root)))
  mockery::expect_called(mock_git_info_call, 0)

  dir.create(file.path(root, ".git"))
  withr::with_options(
    list(orderly.nogit = FALSE),
    expect_null(git_info(root)))
  mockery::expect_called(mock_git_info_call, 1)
  expect_equal(mockery::mock_args(mock_git_info_call)[[1]],
               list(root, c("rev-parse", "HEAD")))
})
