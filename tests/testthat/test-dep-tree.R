context("dep_tree")

test_that("basic tree example", {
  path <- prepare_orderly_example("demo")

  demo <- c("- name: other", "  parameters:", "    nmin: 0",
            "- name: use_dependency",
            "- name: use_dependency_2",
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
  expect_true(length(child_1$children) == 2)

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

test_that("nonexistant reports ", {
  path <- prepare_orderly_example("demo")

  demo <- c("- name: other", "  parameters:", "    nmin: 0",
            "- name: use_dependency")
  writeLines(demo, file.path(path, "demo.yml"))
  run_orderly_demo(path)
  other_id <- dir(file.path(path, "archive", "other"))

  expect_error(orderly_build_dep_tree("bad_report", root = path),
               "This report does not exist")

  expect_error(orderly_build_dep_tree("other", id = "bad_id", root = path),
               "no report with this id in the database")

  expect_error(orderly_build_dep_tree("use_dependency", id = other_id, root = path),
               "id does not match report name")
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
            "- name: use_dependency_2")
  writeLines(demo, file.path(path, "demo.yml"))
  run_orderly_demo(path)

  ## top report so has no dependencies upstream
  tree <- orderly_build_dep_tree("other", root = path, upstream = TRUE)
  root <- tree$root
  expect_true(length(root$children) == 0)

  tree <- orderly_build_dep_tree("use_dependency_2", root = path, upstream = TRUE)
  root <- tree$root
  expect_true(length(root$children) == 1)
  child_1 <- root$children[[1]]
  expect_true(length(child_1$children) == 1)
  child_2 <- child_1$children[[1]]
  expect_true(length(child_2$children) == 0)
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

  tree <- orderly_build_dep_tree("other", root = path)

  root <- tree$root
  expect_true(!root$out_of_date)

  dep_1_1 <- root$children[[1]]
  expect_true(dep_1_1$out_of_date)

  dep_1_2 <- root$children[[2]]
  expect_true(!dep_1_2$out_of_date)

  dep_2_1 <- dep_1_1$children[[1]]
  expect_true(dep_2_1$out_of_date)

  dep_2_2 <- dep_1_2$children[[1]]
  expect_true(dep_2_2$out_of_date)

  dep_2_3 <- dep_1_2$children[[2]]
  expect_true(!dep_2_3$out_of_date)
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
  root <- tree$root
  expect_false(root$out_of_date)

  dep_1_1 <- root$children[[1]]
  expect_true(dep_1_1$out_of_date)

  dep_2_1 <- dep_1_1$children[[1]]
  expect_true(dep_2_1$out_of_date)

  dep_1_2 <- root$children[[2]]
  expect_false(dep_1_2$out_of_date)

  tree <- orderly_build_dep_tree("use_dependency_2", root = path,
                                 propagate = TRUE, upstream = TRUE)
print(tree)
  root <- tree$root
  expect_false(!root$out_of_date)

  dep_1_1 <- root$children[[1]]
  expect_true(dep_1_1$out_of_date)

  dep_2_1 <- dep_1_1$children[[1]]
  expect_false(dep_2_1$out_of_date)
})