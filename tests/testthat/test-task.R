context("tasks")

test_that("pack task", {
  path <- prepare_orderly_example("minimal")
  on.exit(unlink(path, recursive = TRUE))

  path_tasks <- tempfile()

  res <- orderly_task_pack(path_tasks, "example", root = path)
  expect_equal(dir(path_tasks), basename(res$path))
  expect_equal(basename(res$path), paste0(res$id, ".zip"))

  ## Move the orderly root to prevent any file references being valid:
  path2 <- paste0(path, "-moved")
  file.rename(path, path2)

  workdir <- tempfile()
  zip <- orderly_task_run(res$path, workdir, echo = FALSE)
  expect_equal(dir(workdir), basename(zip$path))
  orderly_task_import(zip$path, root = path2)

  expect_equal(orderly_list_archive(path2),
               data_frame(name = "example", id = res$id))
})


test_that("can run a task in place if wanted", {
  path <- prepare_orderly_example("minimal")
  on.exit(unlink(path, recursive = TRUE))

  path_tasks <- tempfile()

  res <- orderly_task_pack(path_tasks, "example", root = path)
  expect_equal(dir(path_tasks), basename(res$path))
  expect_equal(basename(res$path), paste0(res$id, ".zip"))

  l1 <- orderly_task_list(path_tasks)
  expect_equal(l1$id, res$id)
  expect_equal(l1$status, "incomplete")
  expect_equal(l1$name, "example")
  expect_equal(l1$parameters, I(list(NULL)))

  zip <- orderly_task_run(res$path, path_tasks, echo = FALSE)
  expect_true(same_path(zip$path, res$path))

  l2 <- orderly_task_list(path_tasks)
  l1$status <- "complete"
  expect_equal(l1, l2)
})


test_that("pack a task that requires parameters", {
  path_src <- prepare_orderly_example("demo")
  path_tasks <- tempfile()
  path_workdir <- tempfile()

  res <- orderly_task_pack(path_tasks, "other", parameters = list(nmin = 0.5),
                           root = path_src)
  info <- orderly_task_info(res$path)
  expect_equal(info$parameters, list(nmin = 0.5))
  expect_true(all(info$data$data$extract$number >= 0.5))

  zip <- orderly_task_run(res$path, path_workdir, echo = FALSE)
  orderly_task_import(zip$path, root = path_src)

  dat <- readRDS(path_orderly_run_rds(
    file.path(path_src, "archive", "other", res$id)))
  expect_match(dat$meta$data$query, "number > 0.5", fixed = TRUE)
  expect_equal(dat$meta$parameters, list(nmin = 0.5))
})


test_that("list a directory of tasks", {
  path <- prepare_orderly_example("demo")
  on.exit(unlink(path, recursive = TRUE))

  path_tasks <- tempfile()
  path_work <- tempfile()

  ## If we export a number at once, we should avoid collisions, but
  ## this will be slow enough on almost any computer to avoid that:
  res1 <- orderly_task_pack(path_tasks, "other", parameters = list(nmin = 0),
                            root = path)
  res2 <- orderly_task_pack(path_tasks, "other", parameters = list(nmin = 0.5),
                            root = path)

  info1 <- orderly_task_list(path_tasks)
  expect_equal(info1$id, c(res1$id, res2$id))
  expect_equal(info1$status, rep("incomplete", 2))
  expect_equal(info1$name, rep("other", 2))
  expect_equal(info1$parameters, I(list(list(nmin = 0), list(nmin = 0.5))))
  expect_is(info1$time, "POSIXt")

  ## This fails for reasons....
  zip1 <- orderly_task_run(res1$path, path_tasks, echo = FALSE)
  info2 <- orderly_task_list(path_tasks)
  info1$status[[1]] <- "complete"
  expect_equal(info2, info1)
})


test_that("can't run a task twice", {
  path <- prepare_orderly_example("minimal")
  on.exit(unlink(path, recursive = TRUE))

  path_tasks <- tempfile()

  res <- orderly_task_pack(path_tasks, "example", root = path)
  expect_equal(dir(path_tasks), basename(res$path))
  expect_equal(basename(res$path), paste0(res$id, ".zip"))

  zip <- orderly_task_run(res$path, path_tasks, echo = FALSE)
  expect_error(orderly_task_run(res$path, path_tasks, echo = FALSE),
               sprintf("Task '%s' has already been run", res$id))
})


test_that("Can't import a task twice", {
  path <- prepare_orderly_example("minimal")
  on.exit(unlink(path, recursive = TRUE))

  path_tasks <- tempfile()

  res <- orderly_task_pack(path_tasks, "example", root = path)
  expect_equal(dir(path_tasks), basename(res$path))
  expect_equal(basename(res$path), paste0(res$id, ".zip"))

  zip <- orderly_task_run(res$path, path_tasks, echo = FALSE)
  orderly_task_import(zip$path, root = path)
  expect_error(
    orderly_task_import(zip$path, root = path),
    sprintf("example:%s already exists", res$id))
})


test_that("Can't extract a task onto itself", {
  path <- prepare_orderly_example("minimal")
  on.exit(unlink(path, recursive = TRUE))
  path_tasks <- tempfile()
  res <- orderly_task_pack(path_tasks, "example", root = path)
  dir.create(file.path(path_tasks, res$id))
  expect_error(
    orderly_task_run(res$path, path_tasks, echo = FALSE),
    sprintf("Can't unpack task '%s' here; it has already been extracted",
            res$id))
})


test_that("can run a task with dependencies", {
  path <- orderly::orderly_example("demo")
  orderly::orderly_run_internal("other", parameters = list(nmin = 0),
                                root = path, commit = TRUE, echo = FALSE)

  path_tasks <- tempfile()
  path_work <- tempfile()

  res1 <- orderly_task_pack(path_tasks, "use_dependency", root = path)
  expect_true(
    file.path(res1$id, "pack/incoming.csv") %in%
    zip::zip_list(res1$path)$filename)

  res2 <- orderly_task_run(res1$path, path_work, echo = FALSE)

  ## We can run this and import into the db:
  orderly_task_import(res2$path, root = path)

  con <- orderly_db("destination", root = path)
  on.exit(DBI::dbDisconnect(con))
  depends <- DBI::dbReadTable(con, "depends")
  expect_equal(depends$report_version, res2$id)
  expect_equal(depends$as, "incoming.csv")
})
