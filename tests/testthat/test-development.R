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


test_that("status can detect dependencies", {
  path <- prepare_orderly_example("demo")
  p <- file.path(path, "src", "use_dependency")

  cmp <- data_frame(
    filename = c("orderly.yml", "script.R", "incoming.csv",
                 "graph.png", "info.rds"),
    type = c("orderly", "script", "dependency", "artefact", "artefact"),
    present = c(TRUE, TRUE, FALSE, FALSE, FALSE),
    derived = c(FALSE, FALSE, TRUE, TRUE, TRUE))
  class(cmp) <- c("orderly_status", "data.frame")

  expect_equal(orderly_status(p), cmp)

  file.create(file.path(p, "incoming.csv"))
  cmp$present[[3]] <- TRUE
  expect_equal(orderly_status(p), cmp)

  file.create(file.path(p, "info.rds"))
  cmp$present[[5]] <- TRUE
  expect_equal(orderly_status(p), cmp)
})


test_that("orderly_develop_location", {
  path <- prepare_orderly_example("demo")

  cfg <- orderly_config(path)
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
