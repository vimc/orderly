context("workflow")

test_that("workflow can be run", {
  path <- prepare_orderly_example("demo")
  expect_error(orderly_workflow("my_workflow", root = path),
               "Workflow running not yet implemented")
})

test_that("orderly_workflow throws an error if it cannot locate workflow", {
  path <- prepare_orderly_example("demo")
  expect_error(
    orderly_workflow("missing_workflow", root = path),
    "workflow configuration does not exist: 'missing_workflow.yml'",
    fixed = TRUE)
})

test_that("orderly_workflow throws error if report does not exist", {
  path <- prepare_orderly_example("workflow", testing = TRUE)
  expect_error(
    orderly_workflow("missing_report", path),
    paste0("Cannot run workflow 'missing_report' as reports missing: ",
           "'missing', 'missing2'"))
})

test_that("orderly_workflow throws error if steps do not exist", {
  path <- prepare_orderly_example("workflow", testing = TRUE)
  expect_error(
    orderly_workflow("missing_steps", path),
    "Fields missing from [\\w/]+/workflows/missing_steps.yml: steps",
    perl = TRUE)
})

test_that("orderly_workflow throws error if steps are misconfigured", {
  path <- prepare_orderly_example("workflow", testing = TRUE)
  expect_error(
    orderly_workflow("missing_name", path),
    "Fields missing from [\\w/]+/workflows/missing_name.yml:steps: name",
    perl = TRUE)
  expect_error(
    orderly_workflow("broken_steps", path),
    "Unknown fields in [\\w/]+/workflows/broken_steps.yml:steps: field",
    perl = TRUE)
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
