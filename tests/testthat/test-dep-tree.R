context("dep_tree")

# check a report with no dependencies
test_that("no dependencies", {
  path <- prepare_orderly_example("minimal")
  tmp <- tempfile()
  orderly_run("example", config = path, id_file = tmp, echo = FALSE)

  messages <- capture_messages(
    print_dep_tree("example", draft = TRUE, config = path)
  )
  expect_equal(messages, "[ dep tree   ]  Nothing to update.\n")
})

# check reports with recursive dependencies
# example
# - depend
#   - depend3
test_that("has dependencies", {
  path <- prepare_orderly_example("depends")
  tmp <- tempfile()
  r1 <- orderly_run("example", config = path, id_file = tmp, echo = FALSE)
  r2 <- orderly_run("depend", config = path, id_file = tmp, echo = FALSE)
  r3 <- orderly_run("depend3", config = path, id_file = tmp, echo = FALSE)
  
  messages <- capture_messages(
    print_dep_tree("example", draft = TRUE, config = path)
  )
  exp_message <- c(sprintf("[ dep tree   ]  - depend [%s]\n", r2),
                   sprintf("[ dep tree   ]    - depend3 [%s]\n", r3))
  expect_equal(messages, exp_message)
  
  messages <- capture_messages(
    print_dep_tree("depend", draft = TRUE, config = path)
  )
  exp_message <- sprintf("[ dep tree   ]  - depend3 [%s]\n", r3)
  
  expect_equal(messages, exp_message)
})