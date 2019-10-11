context("dep_tree")

test_that("basic tree example", {
  path <- prepare_orderly_example("demo")

  demo <- c("- name: other", "  parameters:", "    nmin: 0",
            "- name: use_dependency",
            "- name: use_dependency_2")
  writeLines(demo, file.path(path, "demo.yml"))
  run_orderly_demo(path)

  tree <- orderly_build_dep_tree("other", root = path)

  root <- tree$root
  readble_root <- root$format()
  expect_match(readble_root,
               "other \\[[0-9]{8}-[0-9]{6}-[a-f0-9]{8}\\]")
  expect_true(length(root$children) == 1)

  child_1 <- root$children[[1]]
  readable_child_1 <- child_1$format()
  expect_match(readable_child_1,
               "use_dependency \\[[0-9]{8}-[0-9]{6}-[a-f0-9]{8}\\]")
  expect_true(length(child_1$children) == 1)

  child_2 <- child_1$children[[1]]
  readable_child_2 <- child_2$format()
  expect_match(readable_child_2,
               "use_dependency_2 \\[[0-9]{8}-[0-9]{6}-[a-f0-9]{8}\\]")
  expect_true(length(child_2$children) == 0)
})

# check a report with no dependencies
test_that("no dependendent reports", {
  path <- prepare_orderly_example("demo")

  demo <- c("- name: other", "  parameters:", "    nmin: 0")
  writeLines(demo, file.path(path, "demo.yml"))
  run_orderly_demo(path)

  tree <- orderly_build_dep_tree("other", root = path)

  root <- tree$root
  readble_root <- root$format()
  expect_match(readble_root,
               "other \\[[0-9]{8}-[0-9]{6}-[a-f0-9]{8}\\]")
  expect_true(length(root$children) == 0)
})

# check reports with recursive dependencies
# example
# - depend
#   - depend3
test_that("has dependencies downstream", {
  path <- prepare_orderly_example("demo")

  demo <- c("- name: other", "  parameters:", "    nmin: 0",
            "- name: use_dependency",
            "- name: use_dependency_2")
  writeLines(demo, file.path(path, "demo.yml"))
  run_orderly_demo(path)

  tree <- orderly_build_dep_tree("use_dependency", root = path)
  root <- tree$root
  readble_root <- root$format()
  expect_match(readble_root,
               "use_dependency \\[[0-9]{8}-[0-9]{6}-[a-f0-9]{8}\\]")
  expect_true(length(root$children) == 1)

  child_1 <- root$children[[1]]
  readable_child_1 <- child_1$format()
  expect_match(readable_child_1,
               "use_dependency_2 \\[[0-9]{8}-[0-9]{6}-[a-f0-9]{8}\\]")
  expect_true(length(child_1$children) == 0)

  tree <- orderly_build_dep_tree("use_dependency_2", root = path)
  root <- tree$root
  readble_root <- root$format()
  expect_match(readble_root,
               "use_dependency_2 \\[[0-9]{8}-[0-9]{6}-[a-f0-9]{8}\\]")
  expect_true(length(root$children) == 0)
})

test_that("has dependencies upstream", {
  path <- prepare_orderly_example("demo")

  demo <- c("- name: other", "  parameters:", "    nmin: 0",
            "- name: use_dependency",
            "- name: use_dependency_2",
            "- name: use_dependency_2")
  writeLines(demo, file.path(path, "demo.yml"))
  run_orderly_demo(path)

  tree <- orderly_build_dep_tree("other", root = path, upstream = TRUE)

  tree <- orderly_build_dep_tree("use_dependency_2", root = path, upstream = TRUE)
})

test_that("out of date dependencies", {
  path <- prepare_orderly_example("demo")

  demo <- c("- name: other", "  parameters:", "    nmin: 0",
            "- name: use_dependency",
            "- name: use_dependency_2",
            "- name: use_dependency",
            "- name: use_dependency_2",
            "- name: use_dependency_2")
  writeLines(demo, file.path(path, "demo.yml"))
  run_orderly_demo(path)

  tree <- orderly_build_dep_tree("other", root = path, upstream = FALSE)
})

test_that("propagate", {
  path <- prepare_orderly_example("demo")

  demo <- c("- name: other", "  parameters:", "    nmin: 0",
            "- name: use_dependency",
            "- name: use_dependency_2",
            "- name: use_dependency")
  writeLines(demo, file.path(path, "demo.yml"))
  run_orderly_demo(path)

  tree <- orderly_build_dep_tree("other", root = path, propagate = TRUE)
})