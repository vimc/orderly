context("dep_tree")

# check a report with no dependencies
test_that("no dependencies", {
  path <- prepare_orderly_example("minimal")
  tmp <- tempfile()
  id <- orderly_run("example", root = path, echo = FALSE)
  orderly_commit(id, root = path)
  file.remove(file.path(path, "orderly.sqlite"))
  orderly_rebuild(path)

  messages <- capture_messages(
    print_dep_tree("example", root = path)
  )
  expect_equal(messages,
               c("\033[32m+++++DOWNSTREAM+++++\033[39m\n",
                sprintf("\033[34mexample [%s]\033[39m\n", id)))
})

# check reports with recursive dependencies
# example
# - depend
#   - depend3
test_that("has dependencies downstream", {
  path <- prepare_orderly_example("depends")
  tmp <- tempfile()
  r1 <- orderly_run("example", root = path, echo = FALSE)
  r2 <- orderly_run("depend", root = path, echo = FALSE)
  r3 <- orderly_run("depend3", root = path, echo = FALSE)
  orderly_commit(r1, root = path)
  orderly_commit(r2, root = path)
  orderly_commit(r3, root = path)
  file.remove(file.path(path, "orderly.sqlite"))
  orderly_rebuild(path)

  messages <- capture_messages(
    print_dep_tree("example", root = path)
  )
  exp_message <- c("\033[32m+++++DOWNSTREAM+++++\033[39m\n",
                   sprintf("\033[34mexample [%s]\033[39m\n", r1),
                   sprintf("\033[34m|___depend [%s]\033[39m\n", r2),
                   sprintf("\033[34m  |___depend3 [%s]\033[39m\n", r3))
  expect_equal(messages, exp_message)
  
  messages <- capture_messages(
    print_dep_tree("depend", root = path)
  )
  exp_message <- c("\033[32m+++++DOWNSTREAM+++++\033[39m\n",
                   sprintf("\033[34mdepend [%s]\033[39m\n", r2),
                   sprintf("\033[34m|___depend3 [%s]\033[39m\n", r3))
  
  expect_equal(messages, exp_message)
})

test_that("has dependencies upstream", {
  path <- prepare_orderly_example("depends")
  tmp <- tempfile()
  r1 <- orderly_run("example", root = path, echo = FALSE)
  r2 <- orderly_run("depend", root = path, echo = FALSE)
  r3 <- orderly_run("depend3", root = path, echo = FALSE)
  orderly_commit(r1, root = path)
  orderly_commit(r2, root = path)
  orderly_commit(r3, root = path)
  file.remove(file.path(path, "orderly.sqlite"))
  orderly_rebuild(path)

  messages <- capture_messages(
    print_dep_tree("example", root = path, upstream = TRUE)
  )
  exp_message <- c("\033[32m++++++UPSTREAM++++++\033[39m\n",
                   sprintf("\033[34mexample [%s]\033[39m\n", r1),
                   sprintf("\033[34m|___depend [%s]\033[39m\n", r2),
                   sprintf("\033[34m  |___depend3 [%s]\033[39m\n", r3))
  expect_equal(messages, exp_message)
  
  messages <- capture_messages(
    print_dep_tree("depend3", root = path, upstream = TRUE)
  )
  exp_message <- c("\033[32m++++++UPSTREAM++++++\033[39m\n",
                   sprintf("\033[34mdepend [%s]\033[39m\n", r2),
                   sprintf("\033[34m|___depend3 [%s]\033[39m\n", r3))
  
  expect_equal(messages, exp_message)
})