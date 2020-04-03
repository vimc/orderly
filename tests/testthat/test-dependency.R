context("dependency")

test_that("basic tree example", {
  path <- prepare_orderly_example("depends", testing = TRUE)

  demo <- c("- name: example",
            "- name: depend2",
            "- name: depend3",
            "- name: depend3")
  writeLines(demo, file.path(path, "demo.yml"))
  run_orderly_demo(path)

  tree <- orderly_graph("example", root = path, show_all = TRUE)

  root <- tree$root
  readable_root <- root$format()
  expect_match(readable_root,
               "example \\[[0-9]{8}-[0-9]{6}-[a-f0-9]{8}\\]")
  expect_true(length(root$children) == 1)

  child_1 <- root$children[[1]]
  readable_child_1 <- child_1$format()
  expect_match(readable_child_1,
               "depend2 \\[[0-9]{8}-[0-9]{6}-[a-f0-9]{8}\\]")
  expect_true(length(child_1$children) == 2)

  child_2 <- child_1$children[[1]]
  readable_child_2 <- child_2$format()
  expect_match(readable_child_2,
               "depend3 \\[[0-9]{8}-[0-9]{6}-[a-f0-9]{8}\\]")
  expect_true(length(child_2$children) == 0)

  bad_reports <- orderly_graph_out_of_date(tree)
  expect_equal(length(bad_reports), 0)
})

# check a report with no dependencies
test_that("no dependendent reports", {
  path <- prepare_orderly_example("depends", testing = TRUE)

  demo <- c("- name: example")
  writeLines(demo, file.path(path, "demo.yml"))
  run_orderly_demo(path)

  tree <- orderly_graph("example", root = path)

  root <- tree$root
  readble_root <- root$format()
  expect_match(readble_root,
               "example \\[[0-9]{8}-[0-9]{6}-[a-f0-9]{8}\\]")
  expect_true(length(root$children) == 0)
})

test_that("nonexistant reports ", {
  path <- prepare_orderly_example("depends", testing = TRUE)

  demo <- c("- name: example",
            "- name: depend2")
  writeLines(demo, file.path(path, "demo.yml"))
  run_orderly_demo(path)
  other_id <- dir(file.path(path, "archive", "example"))

  expect_error(orderly_graph("bad_report", root = path),
               "This report does not exist")

  expect_error(orderly_graph("example", id = "bad_id", root = path),
               "No report with id bad_id in the database")

  expect_error(orderly_graph("depend2", id = other_id, root = path),
               sprintf("id %s does not match report name depend2", other_id))
})

test_that("has dependencies upstream", {
  path <- prepare_orderly_example("depends", testing = TRUE)

  demo <- c("- name: example",
            "- name: depend2",
            "- name: depend3")
  writeLines(demo, file.path(path, "demo.yml"))
  run_orderly_demo(path)

  ## top report so has no dependencies upstream
  tree <- orderly_graph("example", root = path, direction = "upstream")
  root <- tree$root
  expect_true(length(root$children) == 0)

  tree <- orderly_graph("depend3", root = path, direction = "upstream")
  root <- tree$root
  expect_true(length(root$children) == 1)
  child_1 <- root$children[[1]]
  expect_true(length(child_1$children) == 1)
  child_2 <- child_1$children[[1]]
  expect_true(length(child_2$children) == 0)
})

## this is a a bit hacky since reports only get flagged as out of date if the
## artefact we depend on has changed
test_that("out of date dependencies", {
  path <- prepare_orderly_example("depends", testing = TRUE)

  demo <- c("- name: example",
            "- name: depend2",
            "- name: depend3",
            "- name: example")
  writeLines(demo, file.path(path, "demo.yml"))
  run_orderly_demo(path)

  tree <- orderly_graph("example", id = "previous", root = path)

  root <- tree$root
  ## this report SHOULD NOT be out of date - there is a newer version of this
  ## report, but none of ancestor reports have changed
  expect_false(root$out_of_date)

  dep_1 <- root$children[[1]]
  ## this report should be out of date - this report depends on other which has
  ## been re-run
  expect_true(dep_1$out_of_date)

  dep_2 <- dep_1$children[[1]]
  ## this report should be out of date - this report depends on other which is
  ## out-of-date and by default we propogate out-of-date status
  expect_true(dep_2$out_of_date)

})

test_that("propagate", {
  ## What happens if we do not propagate the out-of-date status
  path <- prepare_orderly_example("depends", testing = TRUE)

  demo <- c("- name: example",
            "- name: depend2",
            "- name: depend3",
            "- name: example")
  writeLines(demo, file.path(path, "demo.yml"))
  run_orderly_demo(path)

  tree <- orderly_graph("example", id = "previous", root = path,
                        propagate = FALSE)

  root <- tree$root
  ## SHOULD NOT be out of date, although there is a more recent version of this
  ## report - this version does not depend2 on any out-of-date artefacts
  expect_false(root$out_of_date)

  dep_1 <- root$children[[1]]
  ## SHOULD be out of date since it depends on a report that has a more recent
  ## version
  expect_true(dep_1$out_of_date)

  dep_2 <- dep_1$children[[1]]
  ## SHOULD NOT be out of date since we did not propagate out of dateness
  expect_false(dep_2$out_of_date)

  ##
  tree <- orderly_graph("depend3", root = path, propagate = FALSE,
                        direction = "upstream")
  ## SHOULD NOT be out of date since we did not propagate out of dateness
  root <- tree$root
  expect_false(root$out_of_date)

  ## use_dependency IS out-of-date  since it depends on a report that has a more
  ## recent version
  dep_1_1 <- root$children[[1]]
  expect_true(dep_1_1$out_of_date)

  ## other IS NOT out-of-date since this version does not depend2 on anything
  ## else so can never be out of date
  dep_2_1 <- dep_1_1$children[[1]]
  expect_false(dep_2_1$out_of_date)
})

test_that("circular dependency", {
  ## A circular dependency is difficult to create
  ## we need two reports A, B s.t. A -> B
  ## run A; then modify A so that B -> A then run B
  path <- prepare_orderly_example("depends", testing = TRUE)
  ## run report other
  demo <- c("- name: example")
  writeLines(demo, file.path(path, "demo.yml"))
  run_orderly_demo(path)

  ## add dependency to report other
  other_path <- file.path(path, "src", "example")
  dep_str <- c("depends:",
               "  depend2:",
               "    id: latest",
               "    use:",
               "      info: output.rds")
  write(dep_str, file = file.path(other_path, "orderly.yml"), append = TRUE)

  ## run use_dependency and re-run the modified other
  demo <- c("- name: depend2",
            "- name: example")
  writeLines(demo, file.path(path, "demo.yml"))
  run_orderly_demo(path)

  tree <- orderly_graph("example", id = "previous", root = path)
  circ_tree <- tree$format()[1]

  expect_match(circ_tree,
               "There appears to be a circular dependency.")
})

test_that("infinite recursion", {
  path <- prepare_orderly_example("depends", testing = TRUE)

  demo <- c("- name: example",
            "- name: depend2",
            "- name: depend3",
            "- name: depend3")
  writeLines(demo, file.path(path, "demo.yml"))
  run_orderly_demo(path)

  expect_error(orderly_graph("example", max_depth = 1, root = path),
               "The tree is very large or degenerate.")
})

test_that("multiple dependencies", {
  path <- prepare_orderly_example("depends", testing = TRUE)

  demo <- c("- name: example",
            "- name: depend2",
            "- name: depend3",
            "- name: depend2",
            "- name: depend3")
  writeLines(demo, file.path(path, "demo.yml"))
  run_orderly_demo(path)

  tree <- orderly_graph("example", root = path,
                                  propagate = FALSE, show_all = TRUE)

  tree_print <- tree$format()

  ## make sure we print out the correct indentation
  expect_match(tree_print, "example \\[[0-9]{8}-[0-9]{6}-[a-f0-9]{8}\\]")
  expect_match(tree_print, "\\+--.*depend2")
  expect_match(tree_print, "\\|   \\+--.*depend3")
  expect_match(tree_print, "    \\+--.*depend3")
})

test_that("List out of date upstream", {
  path <- prepare_orderly_example("depends", testing = TRUE)

  demo <- c("- name: example",
            "- name: depend2",
            "- name: depend3",
            "- name: example")
  writeLines(demo, file.path(path, "demo.yml"))
  run_orderly_demo(path)

  tree <- orderly_graph("depend3", root = path, direction = "upstream",
                        propagate = TRUE)

  bad_reports <- orderly_graph_out_of_date(tree)
  expect_equal(bad_reports, c("depend3", "depend2"))
})

test_that("List out of date with duplicates", {
  path <- prepare_orderly_example("depends", testing = TRUE)

  demo <- c("- name: example",
            "- name: depend2",
            "- name: depend3",
            "- name: depend2",
            "- name: depend3",
            "- name: example")
  writeLines(demo, file.path(path, "demo.yml"))
  run_orderly_demo(path)

  tree <- orderly_graph("example", id = "previous", show_all = TRUE,
                        root = path, propagate = TRUE)

  bad_reports <- orderly_graph_out_of_date(tree)
  expect_equal(bad_reports, c("depend2", "depend3"))
})

test_that("R6 errorMessages", {
  tree <- "Not an R6 object"
    expect_error(orderly_graph_out_of_date(tree),
                 "'tree' must be a report_tree")
})

test_that("Only one report - previous", {
  path <- prepare_orderly_example("depends", testing = TRUE)

  demo <- c("- name: example",
            "- name: depend2",
            "- name: depend3")
  writeLines(demo, file.path(path, "demo.yml"))
  run_orderly_demo(path)

  expect_error(orderly_graph("example", root = path, id = "previous",
                             direction = "downstream"),
               "There is only one version of example")
})

test_that("Pinned reports", {
  ## There is logic in orderly_graph for different behaviour when a
  ## report uses an artefact from a pinned version of a report. We also
  ## distinguish between pinned to the latest and pinned to anything else.

  ## To test this we need to update the yaml to point to specfic versions

  ## Run the first report twice
  path <- prepare_orderly_example("depends", testing = TRUE)
  id_1 <- orderly_run("example", root = path, echo = FALSE)
  orderly_commit(id_1, root = path)
  id_2 <- orderly_run("example", root = path, echo = FALSE)
  orderly_commit(id_2, root = path)

  ## Update the second report to depend2 on the out of date version of the first
  ## report
  path_example <- file.path(path, "src", "depend2")
  yml <- file.path(path_example, "orderly.yml")
  txt <- yaml_read(yml)
  txt$depends[[1]]$example$id <- id_1
  yaml_write(txt, file.path(path_example, "orderly.yml"))

  ## Run the second report twice
  id_3 <- orderly_run("depend2", root = path, echo = FALSE)
  orderly_commit(id_3, root = path)
  id_4 <- orderly_run("depend2", root = path, echo = FALSE)
  orderly_commit(id_4, root = path)

  ## Update the third report to depend2 on the latest version of the second
  ## report
  path_example <- file.path(path, "src", "depend3")
  yml <- file.path(path_example, "orderly.yml")
  txt <- yaml_read(yml)
  txt$depends[[1]]$depend2$id <- id_4
  yaml_write(txt, file.path(path_example, "orderly.yml"))

  ## Run the third report
  id_5 <- orderly_run("depend3", root = path, echo = FALSE)
  orderly_commit(id_5, root = path)

  tree <- orderly_graph("example", root = path, id = id_1,
                        direction = "downstream")
  tree_print <- tree$format()
  ## We don't represent the pinned status of a report in the tree so the only
  ## thing we can reallly chack is the version id
  expect_match(tree_print, id_1)
  expect_match(tree_print, id_4)
  expect_match(tree_print, id_5)
})


test_that("source - downstream", {
  path <- prepare_orderly_example("depends", testing = TRUE)

  config <- orderly_config$new(path)
  g <- orderly_graph_src("example", config, "downstream")

  children <- g$root$children
  expect_equal(length(children), 2)
  nms <- vcapply(children, "[[", "name")
  expect_setequal(nms, c("depend", "depend2"))
  names(children) <- nms

  expect_equal(children$depend$children, list())
  expect_equal(length(children$depend2$children), 1)
  expect_equal(children$depend2$children[[1]]$name, "depend3")
})


test_that("source - upstream", {
  path <- prepare_orderly_example("depends", testing = TRUE)

  config <- orderly_config$new(path)
  g <- orderly_graph_src("depend3", config, "upstream")

  expect_equal(length(g$root$children), 1)
  expect_equal(g$root$children[[1]]$name, "depend2")

  expect_equal(length(g$root$children[[1]]$children), 1)
  expect_equal(g$root$children[[1]]$children[[1]]$name, "example")
})


test_that("can't get dependencies for nonexistant report", {
  path <- prepare_orderly_example("minimal")
  config <- orderly_config$new(path)
  expect_error(
    orderly_graph_src("missing", config, "upstream"),
    "Unknown source report 'missing'")
})


test_that("prevent excessive recursion", {
  path <- prepare_orderly_example("depends", testing = TRUE)

  config <- orderly_config$new(path)
  expect_error(
    orderly_graph_src("depend3", config, "upstream", max_depth = 1),
    "The tree is very large or degenerate")
})
