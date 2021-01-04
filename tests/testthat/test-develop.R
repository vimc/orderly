context("development")


test_that("basic development workflow", {
  path <- prepare_orderly_example("demo")
  p <- file.path(path, "src", "global")

  s1 <- orderly_status(p)
  expect_false(file.exists(file.path(p, "data.csv")))
  orderly_develop_start("global", root = path)
  expect_true(file.exists(file.path(p, "data.csv")))
  s2 <- orderly_status(p)

  orderly_develop_clean("global", path)

  s3 <- orderly_status(p)
  expect_false(file.exists(file.path(p, "data.csv")))
})


test_that("orderly_develop_location", {
  skip_on_cran_windows()
  path <- prepare_orderly_example("demo")

  cfg <- orderly_config_$new(path)
  name <- "minimal"
  cmp1 <- list(config = cfg,
               name = name,
               path = file.path(path_src(cfg$root), name),
               inplace = FALSE)
  cmp2 <- list(config = cfg,
               name = name,
               path = file.path(path_src(cfg$root), name),
               inplace = TRUE)

  id <- orderly_run(name, root = path, echo = FALSE)
  p <- orderly_commit(id, root = path)

  expect_equal(orderly_develop_location(name, path), cmp1)
  expect_equal(orderly_develop_location("src/minimal", path), cmp1)
  ## Strip trailing slash too:
  expect_equal(orderly_develop_location("minimal/", path), cmp1)
  expect_equal(orderly_develop_location("src/minimal/", path), cmp1)

  expect_equal(
    withr::with_dir(cmp1$path, orderly_develop_location(name, path)),
    cmp2)
  expect_equal(
    withr::with_dir(cmp1$path, orderly_develop_location("src/minimal", path)),
    cmp2)
  expect_equal(
    withr::with_dir(cmp1$path, orderly_develop_location(name, NULL, TRUE)),
    cmp2)

  expect_equal(
    withr::with_dir(cmp1$path, orderly_develop_location(NULL, NULL, TRUE)),
    cmp2)
  expect_equal(
    withr::with_dir(cmp1$path, orderly_develop_location(NULL, path, FALSE)),
    cmp2)

  expect_error(orderly_develop_location(NULL, path, FALSE),
               "Did not find orderly.yml within working directory")

  tmp <- orderly_file("examples/minimal/src/example")
  expect_error(
    withr::with_dir(tmp, orderly_develop_location(NULL, path, FALSE)),
    "Working directory is not within the orderly root")

  expect_error(
    withr::with_dir(p, orderly_develop_location(NULL, path, FALSE)),
    "Unexpected working directory - expected src/<name>",
    fixed = TRUE)
})


test_that("orderly_develop_location strips trailing slashes of all types", {
  path <- prepare_orderly_example("minimal")
  expect_equal(orderly_develop_location("foo/", path, FALSE)$name, "foo")
  expect_equal(orderly_develop_location("foo//", path, FALSE)$name, "foo")
  expect_equal(orderly_develop_location("foo\\", path, FALSE)$name, "foo")
  expect_equal(orderly_develop_location("foo\\\\", path, FALSE)$name, "foo")
})


test_that("orderly_develop_location strips src with any slash", {
  path <- prepare_orderly_example("minimal")
  expect_equal(orderly_develop_location("src/foo", path, FALSE)$name, "foo")
  expect_equal(orderly_develop_location("src//foo", path, FALSE)$name, "foo")
  expect_equal(orderly_develop_location("src\\foo", path, FALSE)$name, "foo")
  expect_equal(orderly_develop_location("src\\\\foo", path, FALSE)$name, "foo")
})


test_that("status can detect dependencies", {
  path <- prepare_orderly_example("demo")
  name <- "use_dependency"
  p <- file.path(path, "src", name)

  cmp <- data_frame(
    filename = c("orderly.yml", "script.R", "incoming.csv",
                 "graph.png", "info.rds"),
    type = c("orderly", "script", "dependency", "artefact", "artefact"),
    present = c(TRUE, TRUE, FALSE, FALSE, FALSE),
    derived = c(FALSE, FALSE, TRUE, TRUE, TRUE))
  class(cmp) <- c("orderly_status", "data.frame")

  expect_equal(orderly_develop_status(name, path), cmp)

  file.create(file.path(p, "incoming.csv"))
  cmp$present[[3]] <- TRUE
  expect_equal(orderly_develop_status(name, path), cmp)

  file.create(file.path(p, "info.rds"))
  cmp$present[[5]] <- TRUE
  expect_equal(orderly_develop_status(name, path), cmp)
})


test_that("status reports resources", {
  path <- prepare_orderly_example("demo")
  name <- "use_resource"
  p <- file.path(path, "src", name)

  info <- orderly_recipe$new(name, orderly_config_$new(path))

  cmp <- data_frame(
    filename = c("orderly.yml", "script.R", "meta/data.csv", "README.md",
                 "mygraph.png"),
    type = c("orderly", "script", "resource", "readme", "artefact"),
    present = c(TRUE, TRUE, TRUE, TRUE, FALSE),
    derived = c(FALSE, FALSE, FALSE, FALSE, TRUE))
  class(cmp) <- c("orderly_status", "data.frame")

  expect_equal(orderly_develop_status(name, path), cmp)
})


test_that("status reports globals", {
  path <- prepare_orderly_example("demo")
  name <- "global"
  p <- file.path(path, "src", name)

  cmp <- data_frame(
    filename = c("orderly.yml", "script.R", "data.csv", "out.rds"),
    type = c("orderly", "script", "global", "artefact"),
    present = c(TRUE, TRUE, FALSE, FALSE),
    derived = c(FALSE, FALSE, TRUE, TRUE))
  class(cmp) <- c("orderly_status", "data.frame")

  expect_equal(orderly_develop_status(name, path), cmp)
})


test_that("orderly_develop_status with extra files", {
  path <- prepare_orderly_example("demo")
  p <- file.path(path, "src", "minimal")
  file.create(file.path(p, "extra"))
  status <- orderly_develop_status("minimal", root = path)
  expect_equal(as.list(status[4, ]),
               list(filename = "extra", type = "unknown", present = TRUE,
                    derived = FALSE))
})


test_that("orderly_develop_status with changelog", {
  path <- prepare_orderly_example("changelog", testing = TRUE)

  tmp <- tempfile()
  path_example <- file.path(path, "src", "example")
  path_cl <- path_changelog_txt(path_example)

  writeLines(c("[label1]", "value1"), path_cl)
  status <- orderly_develop_status("example", root = path)

  cmp <- data_frame(
    filename = c("orderly.yml", "script.R", "changelog.txt", "mygraph.png"),
    type = c("orderly", "script", "changelog", "artefact"),
    present = c(TRUE, TRUE, TRUE, FALSE),
    derived = c(FALSE, FALSE, FALSE, TRUE))
  class(cmp) <- c("orderly_status", "data.frame")
  expect_equal(status, cmp)
})


test_that("Can read malformed orderly.yml in develop start", {
  path <- prepare_orderly_example("minimal")
  p <- orderly_new("partial", root = path, quiet = TRUE)
  expect_log_message(
    res <- orderly_develop_start("partial", root = path),
    "[ warning    ]  At least one artefact required",
    fixed = TRUE)
  expect_true(is_directory(res))

  cmp <- data_frame(
    filename = "orderly.yml",
    type = "orderly",
    present = TRUE,
    derived = FALSE)
  class(cmp) <- c("orderly_status", class(cmp))
  expect_equal(orderly_develop_status("partial", root = path), cmp)

  expect_error(
    orderly_develop_clean("partial", root = path),
    NA) # (no error)
})


test_that("can load environment variables during develop", {
  path <- prepare_orderly_example("minimal")
  p <- file.path(path, "src", "example")
  writeLines(c("MY_A: a", "ORDERLY_B: b"), file.path(path, "orderly_envir.yml"))
  on.exit(Sys.unsetenv(c("MY_A", "ORDERLY_B")))
  orderly_develop_start("example", root = path)
  expect_equal(Sys.getenv("MY_A"), "a")
  expect_equal(Sys.getenv("ORDERLY_B"), "b")
})


test_that("Can develop a report with parameters and dependencies", {
  skip_on_cran_windows()
  root <- prepare_orderly_example("demo")

  p <- file.path(root, "src", "use_dependency", "orderly.yml")
  txt <- sub(": latest$", ": latest(parameter:nmin > nmin)", readLines(p))
  txt <- c(txt, "parameters:", "  nmin: ~")
  writeLines(txt, p)

  id1 <- orderly_run("other", list(nmin = 0.5), root = root, echo = FALSE)
  id2 <- orderly_run("other", list(nmin = 0.2), root = root, echo = FALSE)

  orderly_develop_start("use_dependency", list(nmin = 0), root = root,
                        use_draft = TRUE)
  expect_equivalent(
    hash_files(file.path(root, "src", "use_dependency", "incoming.csv")),
    hash_files(file.path(root, "draft", "other", id2, "summary.csv")))

  orderly_develop_start("use_dependency", list(nmin = 0.3), root = root,
                        use_draft = TRUE)
  expect_equivalent(
    hash_files(file.path(root, "src", "use_dependency", "incoming.csv")),
    hash_files(file.path(root, "draft", "other", id1, "summary.csv")))
})


test_that("can load environment variables during develop", {
  path <- prepare_orderly_example("minimal")
  p <- file.path(path, "src", "example")
  on.exit(Sys.unsetenv("ORDERLY_TEST_VARIABLE"))
  writeLines("ORDERLY_TEST_VARIABLE: hello",
             file.path(path, "orderly_envir.yml"))
  append_lines(c("environment:", "  a: ORDERLY_TEST_VARIABLE"),
               file.path(p, "orderly.yml"))
  e <- new.env()
  orderly_develop_start("example", envir = e, root = path)
  expect_equal(e$a, "hello")
})


test_that("don't delete artefacts that are resources", {
  path <- prepare_orderly_example("minimal")
  name <- "example"
  p <- file.path(path, "src", name)

  path_orderly <- file.path(p, "orderly.yml")
  dat <- yaml_read(path_orderly)
  dat$artefacts <- list(dat$artefacts,
                        list(data = list(description = "data",
                                         filenames = "data.csv")))
  dat$resources <- "data.csv"
  yaml_write(dat, path_orderly)

  file.create(file.path(p, "data.csv"))

  d <- orderly_develop_status(name, root = path)
  expect_false(d$derived[d$filename == "data.csv" & d$type == "artefact"])

  orderly_develop_clean(name, root = path)
  expect_true(file.exists(file.path(p, "data.csv")))
})
