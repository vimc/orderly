context("dep_tree")

# check a report with no dependencies
test_that("no dependencies", {
  path <- prepare_orderly_example("minimal")
  tmp <- tempfile()
  id <- orderly_run("example", root = path, echo = FALSE)
  orderly_commit(id, root = path)

  messages <- capture_messages(
    orderly_print_dep_tree("example", root = path)
  )
  exp_message <-
    c(paste(crayon::green("+++++DOWNSTREAM+++++"), "\n", sep=""),
      paste(crayon::blue(sprintf("example [%s]", id)), "\n", sep=""))
  expect_equal(messages, exp_message)
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

  messages <- capture_messages(
    orderly_print_dep_tree("example", root = path)
  )
  exp_message <-
    c(paste(crayon::green("+++++DOWNSTREAM+++++"), "\n", sep=""),
      paste(crayon::blue(sprintf("example [%s]", r1)), "\n", sep=""),
      paste(crayon::blue(sprintf("|___depend [%s]", r2)), "\n", sep=""),
      paste(crayon::blue(sprintf("  |___depend3 [%s]", r3)), "\n", sep=""))

  expect_equal(messages, exp_message)
  
  messages <- capture_messages(
    orderly_print_dep_tree("depend", root = path)
  )
  exp_message <- c("\033[32m+++++DOWNSTREAM+++++\033[39m\n",
                   sprintf("\033[34mdepend [%s]\033[39m\n", r2),
                   sprintf("\033[34m|___depend3 [%s]\033[39m\n", r3))

  exp_message <-
    c(paste(crayon::green("+++++DOWNSTREAM+++++"), "\n", sep=""),
      paste(crayon::blue(sprintf("depend [%s]", r2)), "\n", sep=""),
      paste(crayon::blue(sprintf("|___depend3 [%s]", r3)), "\n", sep=""))
  
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

  messages <- capture_messages(
    orderly_print_dep_tree("example", root = path, upstream = TRUE)
  )

  exp_message <-
    c(paste(crayon::yellow("++++++UPSTREAM++++++"), "\n", sep=""),
      paste(crayon::blue(sprintf("example [%s]", r1)), "\n", sep=""))
  expect_equal(messages, exp_message)
  
  messages <- capture_messages(
    orderly_print_dep_tree("depend3", root = path, upstream = TRUE)
  )
  exp_message <-
    c(paste(crayon::yellow("++++++UPSTREAM++++++"), "\n", sep=""),
      paste(crayon::blue(sprintf("depend3 [%s]", r3)), "\n", sep=""),
      paste(crayon::blue(sprintf("|___depend [%s]", r2)), "\n", sep=""),
      paste(crayon::blue(sprintf("  |___example [%s]", r1)), "\n", sep=""))
  
  expect_equal(messages, exp_message)
})

test_that("out of date dependencies", {
  path <- prepare_orderly_example("depends")
  tmp <- tempfile()
  r1 <- orderly_run("example", root = path, echo = FALSE)
  r2 <- orderly_run("depend", root = path, echo = FALSE)
  r3_1 <- orderly_run("depend3", root = path, echo = FALSE)
  ## We now add a mysterious sleep command to ensure that report 3_2 is later
  ## than report 3_1. Without this pause the test can fail non-deterministicly!!
  Sys.sleep(1.0)
  r3_2 <- orderly_run("depend3", root = path, echo = FALSE)
  orderly_commit(r1, root = path)
  orderly_commit(r2, root = path)
  orderly_commit(r3_1, root = path)
  orderly_commit(r3_2, root = path)

  messages <- capture_messages(
    orderly_print_dep_tree("example", root = path)
  )
  exp_message <-
    c(paste(crayon::green("+++++DOWNSTREAM+++++"), "\n", sep=""),
      paste(crayon::blue(sprintf("example [%s]", r1)), "\n", sep=""),
      paste(crayon::blue(sprintf("|___depend [%s]", r2)), "\n", sep=""),
      ## this row should be red because this report has been re-run
      paste(crayon::red( sprintf("  |___depend3 [%s]", r3_1)), "\n", sep=""),
      paste(crayon::blue(sprintf("  |___depend3 [%s]", r3_2)), "\n", sep=""))

  expect_equal(messages, exp_message)
})