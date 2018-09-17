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

# check reports with dependencies
# example
# - depend
#   - depend3
test_that("has dependencies", {
  path <- prepare_orderly_example("depends")
  tmp <- tempfile()
  orderly_run("example", config = path, id_file = tmp, echo = FALSE)
  orderly_run("depend", config = path, id_file = tmp, echo = FALSE)
  orderly_run("depend3", config = path, id_file = tmp, echo = FALSE)
  
  messages <- capture_messages(
    print_dep_tree("example", draft = TRUE, config = path)
  )
  expect_equal(messages, c("[ depends    ]  - depend\n",
                           "[ depends    ]    - depend3\n"))
  
  messages <- capture_messages(
    print_dep_tree("depend", draft = TRUE, config = path)
  )
  expect_equal(messages, "[ depends    ]  - depend3\n")
})