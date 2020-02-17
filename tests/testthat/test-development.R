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
