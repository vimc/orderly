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
  expect_equal(dir(workdir), basename(zip))
  orderly_task_import(zip, root = path2)

  expect_equal(orderly_list_archive(path2),
               data_frame(name = "example", id = res$id))
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
  orderly_task_import(zip, root = path_src)

  dat <- readRDS(path_orderly_run_rds(
    file.path(path_src, "archive", "other", res$id)))
  expect_match(dat$meta$data$query, "number > 0.5", fixed = TRUE)
  expect_equal(dat$meta$parameters, list(nmin = 0.5))
})
