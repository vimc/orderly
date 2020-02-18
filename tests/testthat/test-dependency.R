context("dependency")

test_that("basic tree example", {
  path <- prepare_orderly_example("demo")

  demo <- c("- name: other", "  parameters:", "    nmin: 0",
            "- name: use_dependency",
            "- name: use_dependency_2",
            "- name: use_dependency_2")
  writeLines(demo, file.path(path, "demo.yml"))
  run_orderly_demo(path)

  tree <- orderly_dependency_tree("other", root = path, show_all = TRUE)

  root <- tree$root
  readable_root <- root$format()
  expect_match(readable_root,
               "other \\[[0-9]{8}-[0-9]{6}-[a-f0-9]{8}\\]")
  expect_true(length(root$children) == 1)

  child_1 <- root$children[[1]]
  readable_child_1 <- child_1$format()
  expect_match(readable_child_1,
               "use_dependency \\[[0-9]{8}-[0-9]{6}-[a-f0-9]{8}\\]")
  expect_true(length(child_1$children) == 2)

  child_2 <- child_1$children[[1]]
  readable_child_2 <- child_2$format()
  expect_match(readable_child_2,
               "use_dependency_2 \\[[0-9]{8}-[0-9]{6}-[a-f0-9]{8}\\]")
  expect_true(length(child_2$children) == 0)

  bad_reports <- orderly_out_of_date_reports(tree)
  expect_equal(length(bad_reports), 0)
})

# check a report with no dependencies
test_that("no dependendent reports", {
  path <- prepare_orderly_example("demo")

  demo <- c("- name: other", "  parameters:", "    nmin: 0")
  writeLines(demo, file.path(path, "demo.yml"))
  run_orderly_demo(path)

  tree <- orderly_dependency_tree("other", root = path)

  root <- tree$root
  readble_root <- root$format()
  expect_match(readble_root,
               "other \\[[0-9]{8}-[0-9]{6}-[a-f0-9]{8}\\]")
  expect_true(length(root$children) == 0)
})

test_that("nonexistant reports ", {
  path <- prepare_orderly_example("demo")

  demo <- c("- name: other", "  parameters:", "    nmin: 0",
            "- name: use_dependency")
  writeLines(demo, file.path(path, "demo.yml"))
  run_orderly_demo(path)
  other_id <- dir(file.path(path, "archive", "other"))

  expect_error(orderly_dependency_tree("bad_report", root = path),
               "This report does not exist")

  expect_error(orderly_dependency_tree("other", id = "bad_id", root = path),
               "No report with id bad_id in the database")

  expect_error(orderly_dependency_tree("use_dependency", id = other_id, root = path),
               sprintf("id %s does not match report name use_dependency", other_id))
})

test_that("has dependencies upstream", {
  path <- prepare_orderly_example("demo")

  demo <- c("- name: other", "  parameters:", "    nmin: 0",
            "- name: use_dependency",
            "- name: use_dependency_2")
  writeLines(demo, file.path(path, "demo.yml"))
  run_orderly_demo(path)

  ## top report so has no dependencies upstream
  tree <- orderly_dependency_tree("other", root = path, direction = "upstream")
  root <- tree$root
  expect_true(length(root$children) == 0)

  tree <- orderly_dependency_tree("use_dependency_2", root = path,
                                 direction = "upstream")
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
  path <- prepare_orderly_example("demo")

  demo <- c("- name: other", "  parameters:", "    nmin: 0",
            "- name: use_dependency",
            "- name: use_dependency_2",
            "- name: other", "  parameters:", "    nmin: 0")
  writeLines(demo, file.path(path, "demo.yml"))
  run_orderly_demo(path)

  tree <- orderly_dependency_tree("other", id = "previous", root = path)

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
  path <- prepare_orderly_example("demo")

  demo <- c("- name: other", "  parameters:", "    nmin: 0",
            "- name: use_dependency",
            "- name: use_dependency_2",
            "- name: other", "  parameters:", "    nmin: 0")
  writeLines(demo, file.path(path, "demo.yml"))
  run_orderly_demo(path)

  tree <- orderly_dependency_tree("other", id = "previous", root = path,
                                 propagate = FALSE)

  root <- tree$root
  ## SHOULD NOT be out of date, although there is a more recent version of this
  ## report - this version does not depend on any out-of-date artefacts
  expect_false(root$out_of_date)

  dep_1<- root$children[[1]]
  ## SHOULD be out of date since it depends on a report that has a more recent
  ## version
  expect_true(dep_1$out_of_date)

  dep_2 <- dep_1$children[[1]]
  ## SHOULD NOT be out of date since we did not propagate out of dateness
  expect_false(dep_2$out_of_date)

  ##
  tree <- orderly_dependency_tree("use_dependency_2", root = path,
                                 propagate = FALSE, direction = "upstream")
  ## SHOULD NOT be out of date since we did not propagate out of dateness
  root <- tree$root
  expect_false(root$out_of_date)

  ## use_dependency IS out-of-date  since it depends on a report that has a more
  ## recent version
  dep_1_1 <- root$children[[1]]
  expect_true(dep_1_1$out_of_date)

  ## other IS NOT out-of-date since this version does not depend on anything
  ## else so can never be out of date
  dep_2_1 <- dep_1_1$children[[1]]
  expect_false(dep_2_1$out_of_date)
})

test_that("circular dependency", {
  ## A circular dependency is difficult to create
  ## we need two reports A,B s.t A -> B
  ## run A; then modify A so that B -> A then run B
  path <- prepare_orderly_example("demo")
  ## run report other
  demo <- c("- name: other", "  parameters:", "    nmin: 0")
  writeLines(demo, file.path(path, "demo.yml"))
  run_orderly_demo(path)

  ## add dependency to report other
  other_path <- file.path(path, "src", "other")
  dep_str <- c("depends:",
               "  use_dependency:",
               "    id: latest",
               "    use:",
               "      info: info.rds")
  write(dep_str, file = file.path(other_path, "orderly.yml"), append = TRUE)

  ## run use_dependency and re-run the modified other
  demo <- c("- name: use_dependency",
            "- name: other", "  parameters:", "    nmin: 0")
  writeLines(demo, file.path(path, "demo.yml"))
  run_orderly_demo(path)

  tree <- orderly_dependency_tree("other", id = "previous", root = path)
  circ_tree <- tree$format()[1]

  expect_match(circ_tree,
               "There appears to be a circular dependency.")
})

test_that("infinite recursion", {
  path <- prepare_orderly_example("demo")

  demo <- c("- name: other", "  parameters:", "    nmin: 0",
            "- name: use_dependency",
            "- name: use_dependency_2",
            "- name: use_dependency_2")
  writeLines(demo, file.path(path, "demo.yml"))
  run_orderly_demo(path)

  expect_error(orderly_dependency_tree("other", max_depth = 1, root = path),
               "The tree is very large or degenerate.")
})

test_that("multiple dependencies", {
  path <- prepare_orderly_example("demo")

  demo <- c("- name: other", "  parameters:", "    nmin: 0",
            "- name: use_dependency",
            "- name: use_dependency_2",
            "- name: use_dependency",
            "- name: use_dependency_2")
  writeLines(demo, file.path(path, "demo.yml"))
  run_orderly_demo(path)

  tree <- orderly_dependency_tree("other", root = path,
                                 propagate = FALSE, show_all = TRUE)

  tree_print <- tree$format()

  ## make sure we print out the correct indentation
  expect_match(tree_print,
               "other \\[[0-9]{8}-[0-9]{6}-[a-f0-9]{8}\\]")
  expect_match(tree_print,
               "\\+--.*use_dependency")
  expect_match(tree_print,
               "\\|   \\+--.*use_dependency_2")
  expect_match(tree_print,
               "    \\+--.*use_dependency_2")
})

test_that("List out of date upstream", {
  path <- prepare_orderly_example("demo")

  demo <- c("- name: other", "  parameters:", "    nmin: 0",
            "- name: use_dependency",
            "- name: use_dependency_2",
            "- name: other", "  parameters:", "    nmin: 0")
  writeLines(demo, file.path(path, "demo.yml"))
  run_orderly_demo(path)

  tree <- orderly_dependency_tree("use_dependency_2", root = path,
                                 direction = "upstream",
                                 propagate = TRUE)

  bad_reports <- orderly_out_of_date_reports(tree)
  expect_equal(bad_reports, c("use_dependency_2","use_dependency"))
})

test_that("R6 errorMessages", {
  tree <- "Not an R6 object"
    expect_error(orderly_out_of_date_reports(tree),
                 "'tree' must be a Tree object")
})

test_that("Only one report - previous", {
  path <- prepare_orderly_example("demo")

  demo <- c("- name: other", "  parameters:", "    nmin: 0",
            "- name: use_dependency",
            "- name: use_dependency_2")
  writeLines(demo, file.path(path, "demo.yml"))
  run_orderly_demo(path)

  expect_error(orderly_dependency_tree("other", root = path, id = "previous",
                                      direction = "downstream"),
               "There is only one version of other")
})