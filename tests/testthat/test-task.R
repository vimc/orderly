context("tasks")

test_that("pack task", {
  path <- prepare_orderly_example("minimal")
  on.exit(unlink(path, recursive = TRUE))

  res <- orderly_task_pack("example", root = path)

  zip1 <- tempfile()
  file.copy(res$path, zip1)

  ## Move the orderly root to prevent any file references being valid:
  path2 <- paste0(path, "-moved")
  file.rename(path, path2)
  workdir <- tempfile()
  zip2 <- orderly_task_run(zip1, workdir, echo = FALSE)
  expect_equal(dir(workdir), basename(zip2))
  orderly_task_import(zip2, root = path2)

  expect_equal(orderly_list_archive(path2),
               data_frame(name = "example", id = res$id))
})
