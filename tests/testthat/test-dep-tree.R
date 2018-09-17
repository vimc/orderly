context("dep_tree")

test_that("dep_tree", {
  path <- orderly:::prepare_orderly_example("minimal")
  
  tmp <- tempfile()
  orderly:::orderly_run("example", config = path, id_file = tmp, echo = FALSE)
  print(path)
  print(getwd())
  
  print(list.files(file.path(path, "src", "example")))
  print(list.files(file.path(path, "draft")))
  print(list.files(file.path(path, "archive")))
  print(orderly:::orderly_list(path))
  orderly:::print_dep_tree("example", draft = TRUE, config = path)
  expect_true(FALSE)
})
