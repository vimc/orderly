context("shiny")


test_that("shiny app", {
  path <- prepare_orderly_example("shiny")
  id <- orderly_run("example", config = path, echo = FALSE)
  p_shiny <- file.path(path, "draft", "example", id, "shiny")
  expect_true(file.exists(p_shiny))
  expect_true(file.exists(file.path(p_shiny, "app.R")))
  expect_true(file.exists(file.path(p_shiny, "data.RData")))

  dest <- file.path(path, "shiny")
  expect_error(orderly_deploy_shiny(dest, config = path),
               "Did not find any archive reports for example")
  orderly_commit(id, config = path)
  orderly_deploy_shiny(dest, config = path)

  expect_true(file.exists(file.path(dest, "index.html")))
  expect_true(file.exists(file.path(dest, "example-shiny-app")))

  p <- file.path(dest, "example-shiny-app")
  q <- file.path(path, "archive", "example", id, "shiny")
  expect_true(file.exists(file.path(p, "orderly_id")))
  expect_equal(readLines(file.path(p, "orderly_id")), id)

  expect_true(all(dir(q) %in% dir(p)))
  expect_equal(hash_files(file.path(p, dir(q)), FALSE),
               hash_files(file.path(q, dir(q)), FALSE))
})


test_that("redeploy", {
  path <- prepare_orderly_example("shiny")
  dest <- file.path(path, "shiny")
  id1 <- orderly_run("example", config = path, echo = FALSE)
  orderly_commit(id1, config = path)
  orderly_deploy_shiny(dest, config = path)
  expect_equal(
    readLines(file.path(dest, "example-shiny-app", "orderly_id")),
    id1)

  tmp <- file.path(dest, "example-shiny-app", "extra-file")
  file.create(tmp)

  id2 <- orderly_run("example", config = path, echo = FALSE)
  orderly_commit(id2, config = path)
  orderly_deploy_shiny(dest, config = path)
  expect_false(file.exists(tmp))
  expect_equal(
    readLines(file.path(dest, "example-shiny-app", "orderly_id")),
    id2)
})


test_that("deploy failure: missing report", {
  path <- prepare_orderly_example("shiny")
  dest <- file.path(path, "shiny")

  path_shiny <- file.path(path, "shiny.yml")
  d <- yaml_read(path_shiny)
  d$apps$"example-shiny-app"$id <- new_report_id()
  yaml_write(d, path_shiny)

  expect_error(orderly_deploy_shiny(dest, config = path),
              "archived orderly report does not exist: example")
})


test_that("deploy failure: impossible report", {
  path <- prepare_orderly_example("shiny")
  dest <- file.path(path, "shiny")
  id <- orderly_run("example", config = path, echo = FALSE)
  orderly_commit(id, config = path)

  unlink(file.path(path, "archive", "example", id, "shiny"), recursive = TRUE)
  expect_error(orderly_deploy_shiny(dest, config = path),
              "archived orderly report does not include shiny: example")
})


test_that("deploy failure: impossible destination", {
  path <- prepare_orderly_example("shiny")
  dest <- file.path(path, "shiny")
  id <- orderly_run("example", config = path, echo = FALSE)
  orderly_commit(id, config = path)
  orderly_deploy_shiny(dest, config = path)

  unlink(file.path(dest, "example-shiny-app", "orderly_id"))
  expect_error(orderly_deploy_shiny(dest, config = path),
              "did not find id file - not continuing")
})
