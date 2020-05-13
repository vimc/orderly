context("workflow")

test_that("workflow can be run", {
  path <- prepare_orderly_example("demo")
  mock_new_report_id <- mockery::mock("report_id_1", "report_id_2")
  mock_random_id <- mockery::mock("workflowid")

  with_mock(
    "orderly:::new_report_id" = mock_new_report_id,
    "ids::random_id" = mock_random_id, {
    output <- evaluate_promise(orderly_workflow("my_workflow", root = path))
  })

  expect_output <- function(message) {
    expect_true(message %in% output$messages)
  }
  expect_output(
    "[ workflow   ]  Running workflow 'my_workflow' with ID 'workflowid'\n")
  expect_output("[ workflow   ]  Running report 'minimal'\n")
  expect_output("[ workflow   ]  Finished running report 'minimal'\n")
  expect_output("[ workflow   ]  Committing report 'minimal'\n")
  expect_output("[ workflow   ]  Finished committing report 'minimal'\n")
  expect_output("[ workflow   ]  Running report 'global'\n")
  expect_output("[ workflow   ]  Finished running report 'global'\n")
  expect_output("[ workflow   ]  Committing report 'global'\n")
  expect_output("[ workflow   ]  Finished committing report 'global\n")
  expect_output(paste0("[ workflow   ]  Completed running workflow",
                       "'my_workflow' with ID 'workflowid'\n"))

  expect_equal(output$result, c("report_id_1", "report_id_2"))

  expect_true(file.exists(path_orderly_run_rds(
    file.path(path, "archive", "minimal", output$result[1]))))
  expect_true(file.exists(path_orderly_run_rds(
    file.path(path, "archive", "global", output$result[2]))))
})

test_that("workflow returns completed IDs if a report fails", {
  path <- prepare_orderly_example("demo")

  ## Ensure one of the reports fails
  p <- file.path(path, "src", "minimal", "script.R")
  txt <- c(readLines(p), "stop('got an error')")
  writeLines(txt, p)

  mock_new_report_id <- mockery::mock("report_id_1", "report_id_2")
  mock_random_id <- mockery::mock("workflowid")
  with_mock(
    "orderly:::new_report_id" = mock_new_report_id,
    "ids::random_id" = mock_random_id, {
      output <- evaluate_promise(orderly_workflow("my_workflow", root = path))
  })

  expect_true("[ workflow   ]  Running report 'minimal'\n" %in% output$messages)
  expect_true(paste0("[ error      ]  Running report 'minimal' ",
                     "failed with message \ngot an error\n")
              %in% output$messages)
  expect_true("[ workflow   ]  Running report 'global'\n" %in% output$messages)
  expect_true(paste0(
    "[ workflow   ]  Completed running workflow ",
    "'my_workflow' with ID 'workflowid'\n with 1 failure(s)\n")
    %in% output$messages)

  expect_equal(output$result, "report_id_2")

  expect_false(file.exists(file.path(path, "archive", "minimal")))
  expect_true(file.exists(path_orderly_run_rds(
    file.path(path, "archive", "global", output$result[1]))))
})

test_that("envir, message, instance, remote get passed to orderly_run", {
  path <- prepare_orderly_example("demo")
  mock_orderly_run <- mockery::mock("report_id_1", "report_id_2")
  with_mock("orderly::orderly_run" = mock_orderly_run, {
    orderly_workflow("my_workflow", root = path, envir = "envir",
                     message = "message", instance = "instance",
                     remote = "remote")
  })
  mockery::expect_called(mock_orderly_run, 2)
  args <- mockery::mock_args(mock_orderly_run)
  expect_equal(args[[1]]$envir, "envir")
  expect_equal(args[[1]]$message, "message")
  expect_equal(args[[1]]$instance, "instance")
  expect_equal(args[[1]]$remote, "remote")
  expect_equal(args[[2]]$envir, "envir")
  expect_equal(args[[2]]$message, "message")
  expect_equal(args[[2]]$instance, "instance")
  expect_equal(args[[2]]$remote, "remote")
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
    orderly_workflow("missing_report", root = path),
    paste0("Cannot run workflow 'missing_report' as reports missing: ",
           "'missing', 'missing2'"))
})

test_that("orderly_workflow throws error if steps do not exist", {
  path <- prepare_orderly_example("workflow", testing = TRUE)
  expect_error(
    orderly_workflow("missing_steps", root = path),
    "Fields missing from .+/workflows/missing_steps.yml: steps")
})

test_that("orderly_workflow throws error if steps are misconfigured", {
  path <- prepare_orderly_example("workflow", testing = TRUE)
  expect_error(
    orderly_workflow("missing_name", root = path),
    "Fields missing from .+/workflows/missing_name.yml:steps: name")
  expect_error(
    orderly_workflow("broken_steps", root = path),
    "Unknown fields in .+/workflows/broken_steps.yml:steps: field")
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
