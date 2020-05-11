context("workflow")

test_that("workflow can be run", {
  path <- orderly_example("demo")
  expect_type(orderly_workflow("my_workflow", root = path), "character")
})

test_that("orderly_workflow throws an error if it cannot locate workflow", {
  path <- orderly_example("demo")
  expect_error(
    orderly_workflow("missing_workflow", root = path),
    "workflow configuration does not exist: 'missing_workflow.yml'",
    fixed = TRUE)
})

test_that("steps can be parsed", {
  test_steps <- list(
    steps = list(
      list(
        name = "step1"
      ),
      list(
        name = "step2"
      ),
      list(
        name = "step3"
      )
    )
  )
  expect_equal(parse_steps(test_steps), c("step1", "step2", "step3"))
})
