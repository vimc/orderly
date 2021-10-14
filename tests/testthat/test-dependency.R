context("dependency")

test_that("basic tree example", {
  path <- test_prepare_orderly_example("depends", testing = TRUE)

  demo <- c("- name: example",
            "- name: depend2",
            "- name: depend3",
            "- name: depend3")
  writeLines(demo, file.path(path, "demo.yml"))
  run_orderly_demo(path)

  tree <- orderly_graph("example", root = path, show_all = TRUE)

  roots <- tree$roots
  expect_length(roots, 1)
  expect_equal(roots[[1]]$name, "example")
  expect_match(roots[[1]]$id, "[0-9]{8}-[0-9]{6}-[a-f0-9]{8}")
  expect_false(roots[[1]]$out_of_date)

  tree_list <- tree$get_tree("example")

  child_1 <- tree_list$dependencies[[1]]
  expect_equal(child_1$name, "depend2")
  expect_match(child_1$id, "[0-9]{8}-[0-9]{6}-[a-f0-9]{8}")
  expect_false(child_1$out_of_date)
  expect_true(length(child_1$dependencies) == 2)

  child_2 <- child_1$dependencies[[1]]
  expect_equal(child_2$name, "depend3")
  expect_match(child_2$id, "[0-9]{8}-[0-9]{6}-[a-f0-9]{8}")
  expect_false(child_2$out_of_date)
  expect_true(length(child_2$dependencies) == 0)

  child_3 <- child_1$dependencies[[2]]
  expect_equal(child_3$name, "depend3")
  expect_match(child_3$id, "[0-9]{8}-[0-9]{6}-[a-f0-9]{8}")
  expect_false(child_3$out_of_date)
  expect_true(length(child_3$dependencies) == 0)
  expect_true(child_3$id != child_2$id)

  bad_reports <- orderly_graph_out_of_date(tree)
  expect_equal(length(bad_reports), 0)
})

# check a report with no dependencies
test_that("no dependendent reports", {
  path <- test_prepare_orderly_example("depends", testing = TRUE)

  demo <- c("- name: example")
  writeLines(demo, file.path(path, "demo.yml"))
  run_orderly_demo(path)

  tree <- orderly_graph("example", root = path)

  root <- tree$roots
  expect_length(roots, 1)
  expect_equal(roots[[1]]$name, "example")
  expect_match(roots[[1]]$id, "[0-9]{8}-[0-9]{6}-[a-f0-9]{8}")
  expect_false(roots[[1]]$out_of_date)
  tree_list <- tree$get_tree("example")
  expect_length(tree_list$dependencies, 0)
})

test_that("nonexistant reports ", {
  path <- test_prepare_orderly_example("depends", testing = TRUE)

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
  path <- test_prepare_orderly_example("depends", testing = TRUE)

  demo <- c("- name: example",
            "- name: depend2",
            "- name: depend3")
  writeLines(demo, file.path(path, "demo.yml"))
  run_orderly_demo(path)

  ## top report so has no dependencies upstream
  tree <- orderly_graph("example", root = path, direction = "upstream")
  tree_list <- tree$get_tree("example")
  expect_true(length(tree_list$dependencies) == 0)

  tree <- orderly_graph("depend3", root = path, direction = "upstream")
  tree_list <- tree$get_tree("depend3")
  expect_true(length(tree_list$dependencies) == 1)
  child_1 <- tree_list$dependencies[[1]]
  expect_true(length(child_1$dependencies) == 1)
  child_2 <- child_1$dependencies[[1]]
  expect_true(length(child_2$dependencies) == 0)
})

## this is a a bit hacky since reports only get flagged as out of date if the
## artefact we depend on has changed
test_that("out of date dependencies", {
  path <- test_prepare_orderly_example("depends", testing = TRUE)

  demo <- c("- name: example",
            "- name: depend2",
            "- name: depend3",
            "- name: example")
  writeLines(demo, file.path(path, "demo.yml"))
  run_orderly_demo(path)

  tree <- orderly_graph("example", id = "previous", root = path)
  tree_list <- tree$get_tree("example")

  ## this report SHOULD NOT be out of date - there is a newer version of this
  ## report, but none of ancestor reports have changed
  expect_false(tree_list$out_of_date)

  dep_1 <- tree_list$dependencies[[1]]
  ## this report should be out of date - this report depends on other which has
  ## been re-run
  expect_true(dep_1$out_of_date)

  dep_2 <- dep_1$dependencies[[1]]
  ## this report should be out of date - this report depends on other which is
  ## out-of-date and by default we propagate out-of-date status
  expect_true(dep_2$out_of_date)
})

test_that("propagate", {
  ## What happens if we do not propagate the out-of-date status
  path <- test_prepare_orderly_example("depends", testing = TRUE)

  demo <- c("- name: example",
            "- name: depend2",
            "- name: depend3",
            "- name: example")
  writeLines(demo, file.path(path, "demo.yml"))
  run_orderly_demo(path)

  tree <- orderly_graph("example", id = "previous", root = path,
                        propagate = FALSE)

  tree_list <- tree$get_tree("example")
  ## SHOULD NOT be out of date, although there is a more recent version of this
  ## report - this version does not depend2 on any out-of-date artefacts
  expect_false(tree_list$out_of_date)

  dep_1 <- tree_list$dependencies[[1]]
  ## SHOULD be out of date since it depends on a report that has a more recent
  ## version
  expect_true(dep_1$out_of_date)

  dep_2 <- dep_1$dependencies[[1]]
  ## SHOULD NOT be out of date since we did not propagate out of dateness
  expect_false(dep_2$out_of_date)

  ##
  tree <- orderly_graph("depend3", root = path, propagate = FALSE,
                        direction = "upstream")
  ## SHOULD NOT be out of date since we did not propagate out of dateness
  tree_list <- tree$get_tree("depend3")
  expect_false(tree_list$out_of_date)

  ## use_dependency IS out-of-date  since it depends on a report that has a more
  ## recent version
  dep_1_1 <- tree_list$dependencies[[1]]
  expect_true(dep_1_1$out_of_date)

  ## other IS NOT out-of-date since this version does not depend2 on anything
  ## else so can never be out of date
  dep_2_1 <- dep_1_1$dependencies[[1]]
  expect_false(dep_2_1$out_of_date)
})

test_that("circular dependency", {
  ## A circular dependency is difficult to create
  ## we need two reports A, B s.t. A -> B
  ## run A; then modify A so that B -> A then run B
  path <- test_prepare_orderly_example("depends", testing = TRUE)
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
  circ_tree <- strsplit(tree$format(), "\n")[[1]][1]

  expect_match(circ_tree,
               "There appears to be a circular dependency.")
})

test_that("infinite recursion", {
  path <- test_prepare_orderly_example("depends", testing = TRUE)

  demo <- c("- name: example",
            "- name: depend2",
            "- name: depend3",
            "- name: depend3")
  writeLines(demo, file.path(path, "demo.yml"))
  run_orderly_demo(path)

  expect_error(orderly_graph("example", recursion_limit = 1, root = path),
               "The tree is very large or degenerate.")
})

test_that("multiple dependencies", {
  path <- test_prepare_orderly_example("depends", testing = TRUE)

  demo <- c("- name: example",
            "- name: depend2",
            "- name: depend3",
            "- name: depend2",
            "- name: depend3")
  writeLines(demo, file.path(path, "demo.yml"))
  run_orderly_demo(path)

  tree <- orderly_graph("example", root = path,
                                  propagate = FALSE, show_all = TRUE)

  tree_print <- withr::with_options(
    list(crayon.enabled = FALSE),
    tree$format(FALSE))

  ## make sure we print out the correct indentation
  expect_match(tree_print, "example \\[[0-9]{8}-[0-9]{6}-[a-f0-9]{8}\\]")
  expect_match(tree_print, "\\+--.*depend2")
  expect_match(tree_print, "\\|   `--.*depend3")
  expect_match(tree_print, "    `--.*depend3")
})

test_that("multiple dependencies only shows 1 when show_all FALSE", {
  path <- test_prepare_orderly_example("depends", testing = TRUE)

  demo <- c("- name: example",
            "- name: depend2",
            "- name: depend3",
            "- name: depend2",
            "- name: depend3")
  writeLines(demo, file.path(path, "demo.yml"))
  run_orderly_demo(path)

  tree <- orderly_graph("example", root = path,
                        propagate = FALSE, show_all = FALSE)
  tree_print <- tree$format()
  tree_lines <- strsplit(tree_print, "\n")[[1]]
  expect_length(tree_lines, 3)
  expect_match(tree_lines[1], "example")
  expect_match(tree_lines[2], "depend2")
  expect_match(tree_lines[3], "depend3")

  tree_list <- tree$get_tree("example", show_all = FALSE)
  expect_equal(tree_list$name, "example")
  expect_length(tree_list$dependencies, 1)
  expect_equal(tree_list$dependencies[[1]]$name, "depend2")
  expect_length(tree_list$dependencies[[1]]$dependencies, 1)
  expect_equal(tree_list$dependencies[[1]]$dependencies[[1]]$name, "depend3")

  tree_list <- tree$get_tree("example", show_all = TRUE)
  expect_equal(tree_list$name, "example")
  expect_length(tree_list$dependencies, 2)
  expect_equal(tree_list$dependencies[[1]]$name, "depend2")
  expect_length(tree_list$dependencies[[1]]$dependencies, 1)
  expect_equal(tree_list$dependencies[[2]]$name, "depend2")
  expect_length(tree_list$dependencies[[2]]$dependencies, 1)
  expect_equal(tree_list$dependencies[[1]]$dependencies[[1]]$name, "depend3")
  expect_equal(tree_list$dependencies[[2]]$dependencies[[1]]$name, "depend3")
})

test_that("List out of date upstream", {
  path <- test_prepare_orderly_example("depends", testing = TRUE)

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
  path <- test_prepare_orderly_example("depends", testing = TRUE)

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
  path <- test_prepare_orderly_example("depends", testing = TRUE)

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
  path <- test_prepare_orderly_example("depends", testing = TRUE)
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
  path <- test_prepare_orderly_example("depends", testing = TRUE)

  config <- orderly_config_$new(path)
  g <- orderly_graph("example", root = path,
                     direction = "downstream", use = "src")

  tree <- g$get_tree("example")
  deps <- tree$dependencies
  expect_equal(length(deps), 2)
  nms <- vcapply(deps, "[[", "name")
  expect_setequal(nms, c("depend", "depend2"))
  names(deps) <- nms

  expect_equal(deps$depend$dependencies, list())
  expect_equal(length(deps$depend2$dependencies), 1)
  expect_equal(deps$depend2$dependencies[[1]]$name, "depend3")
})


test_that("source - upstream", {
  path <- test_prepare_orderly_example("depends", testing = TRUE)

  g <- orderly_graph("depend3", root = path,
                     direction = "upstream", use = "src")
  tree <- g$get_tree("depend3")
  expect_equal(length(tree$dependencies), 1)
  expect_equal(tree$dependencies[[1]]$name, "depend2")

  expect_equal(length(tree$dependencies[[1]]$dependencies), 1)
  expect_equal(tree$dependencies[[1]]$dependencies[[1]]$name, "example")
})


test_that("can't get dependencies for nonexistant report", {
  path <- test_prepare_orderly_example("minimal")
  config <- orderly_config_$new(path)
  expect_error(
    orderly_graph_src("missing", config, "upstream"),
    "Unknown source report 'missing'")

  expect_error(
    orderly_graph_src(c("missing1", "example", "missing2"), config, "upstream"),
    "Unknown source reports 'missing1', 'missing2'.")
})


test_that("error thrown if trying to pass ref in repo without git", {
  path <- test_prepare_orderly_example("minimal")
  expect_error(
    orderly_graph("example", root = path, direction = "upstream", ref = "HEAD"),
    "Non-null ref arg only supported when use = \"src\"")
  expect_error(
    orderly_graph("example", root = path, direction = "upstream", ref = "HEAD",
                  use = "src"),
    "git not supported for this orderly repo")
})


test_that("validate only supported when reading graph from src", {
  path <- test_prepare_orderly_example("minimal")
  expect_error(
    orderly_graph("example", root = path, direction = "upstream",
                  validate = FALSE),
    "Building graph with validate = FALSE not supported when use = \"archive\"")
})


test_that("building graph from archive only supports 1 report at a time", {
  path <- test_prepare_orderly_example("minimal")
  expect_error(
    orderly_graph(c("example", "minimal"), root = path, direction = "upstream"),
    "Graph can only be generated for a single report when use = \"archive\"")
})


test_that("prevent excessive recursion", {
  path <- test_prepare_orderly_example("depends", testing = TRUE)

  ## TODO: What does recursion limit mean for us now we have moved this into
  ## the object? Building src graph as edge matrix requires no recusion until
  ## we try and print it
  config <- orderly_config_$new(path)
  expect_error(
    orderly_graph_src("depend3", config, "upstream", recursion_limit = 1),
    "The tree is very large or degenerate")
})


test_that("id attribution", {
  path <- test_prepare_orderly_example("depends", testing = TRUE)

  p <- file.path(path, "src", "depend", "orderly.yml")
  dat <- yaml::read_yaml(p)
  id <- new_report_id()
  dat$depends$example$id <- id
  yaml::write_yaml(dat, p)

  config <- orderly_config_$new(path)
  g <- orderly_graph_src("example", config, "downstream")

  children <- g$root$children
  expect_equal(length(children), 2)
  nms <- vcapply(children, "[[", "name")
  expect_setequal(nms, c("depend", "depend2"))
  names(children) <- nms

  expect_equal(children$depend$id, id)
  expect_equal(children$depend2$id, "latest")

  g <- orderly_graph_src("depend", config, "upstream")
  expect_equal(g$root$id, "latest")
  expect_equal(g$root$children[[1]]$id, id)
})


test_that("detect loop", {
  path <- test_prepare_orderly_example("depends", testing = TRUE)
  append_lines(c("depends:",
                 "  - depend2:",
                 "      id: latest",
                 "      use:",
                 "        output.rds: output.rds"),
               file.path(path, "src", "example", "orderly.yml"))

  config <- orderly_config_$new(path)
  expect_error(
    orderly_graph_src("depend3", config, "upstream"),
    "Detected circular dependency: 'depend2' -> 'example' -> 'depend2'")
  expect_error(
    orderly_graph_src("example", config, "downstream"),
    "Detected circular dependency: 'example' -> 'depend2' -> 'example'")
})

test_that("archive: set depth of dependencies", {
  path <- test_prepare_orderly_example("depends", testing = TRUE)

  demo <- c("- name: example",
            "- name: depend2",
            "- name: depend3",
            "- name: depend2",
            "- name: depend3")
  writeLines(demo, file.path(path, "demo.yml"))
  run_orderly_demo(path)

  tree <- orderly_graph("example", root = path,
                        propagate = FALSE, show_all = TRUE,
                        max_depth = 1)

  root <- tree$root
  expect_equal(root$name, "example")
  expect_length(root$children, 2)
  child_1 <- root$children[[1]]
  child_2 <- root$children[[2]]
  expect_equal(child_1$name, "depend2")
  expect_length(child_1$children, 0)
  expect_equal(child_2$name, "depend2")
  expect_length(child_2$children, 0)
})

test_that("src: set depth of dependencies", {
  path <- test_prepare_orderly_example("depends", testing = TRUE)

  tree <- orderly_graph("example", root = path, use = "src",
                        propagate = FALSE, show_all = TRUE,
                        max_depth = 1)

  root <- tree$root
  expect_equal(root$name, "example")
  expect_length(root$children, 2)
  child_1 <- root$children[[1]]
  child_2 <- root$children[[2]]
  expect_equal(child_1$name, "depend")
  expect_length(child_1$children, 0)
  expect_equal(child_2$name, "depend2")
  expect_length(child_2$children, 0)
})

test_that("archive: set depth of dependencies upstream", {
  path <- test_prepare_orderly_example("depends", testing = TRUE)

  demo <- c("- name: example",
            "- name: depend2",
            "- name: depend3",
            "- name: depend2",
            "- name: depend3")
  writeLines(demo, file.path(path, "demo.yml"))
  run_orderly_demo(path)

  tree <- orderly_graph("depend3", root = path, direction = "upstream",
                        propagate = FALSE, show_all = TRUE,
                        max_depth = 1)

  root <- tree$root
  expect_equal(root$name, "depend3")
  expect_length(root$children, 1)
  child_1 <- root$children[[1]]
  expect_equal(child_1$name, "depend2")
  expect_length(child_1$children, 0)
})

test_that("src: set depth of dependencies upstream", {
  path <- test_prepare_orderly_example("depends", testing = TRUE)

  tree <- orderly_graph("depend3", root = path, use = "src",
                        propagate = FALSE, show_all = TRUE,
                        direction = "upstream", max_depth = 1)

  root <- tree$root
  expect_equal(root$name, "depend3")
  expect_length(root$children, 1)
  child_1 <- root$children[[1]]
  expect_equal(child_1$name, "depend2")
  expect_length(child_1$children, 0)
})

test_that("archive: set depth of dependencies and recursion limit", {
  path <- test_prepare_orderly_example("depends", testing = TRUE)

  demo <- c("- name: example",
            "- name: depend2",
            "- name: depend3",
            "- name: depend2",
            "- name: depend3")
  writeLines(demo, file.path(path, "demo.yml"))
  run_orderly_demo(path)

  tree <- orderly_graph("depend3", root = path, direction = "upstream",
                        propagate = FALSE, show_all = TRUE,
                        max_depth = 1, recursion_limit = 1)

  root <- tree$root
  expect_equal(root$name, "depend3")
  expect_length(root$children, 1)
  child_1 <- root$children[[1]]
  expect_equal(child_1$name, "depend2")
  expect_length(child_1$children, 0)

  expect_error(orderly_graph("depend3", root = path, direction = "upstream",
                propagate = FALSE, show_all = TRUE,
                max_depth = 1, recursion_limit = 0),
               "The tree is very large or degenerate")
})

test_that("src: set depth of dependencies and recursion limit", {
  path <- test_prepare_orderly_example("depends", testing = TRUE)

  tree <- orderly_graph("depend3", root = path, use = "src",
                        propagate = FALSE, show_all = TRUE,
                        direction = "upstream", max_depth = 1,
                        recursion_limit = 1)

  root <- tree$root
  expect_equal(root$name, "depend3")
  expect_length(root$children, 1)
  child_1 <- root$children[[1]]
  expect_equal(child_1$name, "depend2")
  expect_length(child_1$children, 0)

  expect_error(orderly_graph("depend3", root = path, use = "src",
                             propagate = FALSE, show_all = TRUE,
                             direction = "upstream", max_depth = 1,
                             recursion_limit = 0),
               "The tree is very large or degenerate")
})

test_that("dependency latest with search query", {
  dat <- prepare_orderly_query_example()
  root <- dat$root

  config <- orderly_config_$new(root)

  p <- file.path(root, "src", "use_dependency", "orderly.yml")
  txt <- readLines(p)
  txt <- sub("latest", "latest(parameter:nmin < x)", txt, fixed = TRUE)
  txt <- c(txt, c("parameters:",
                  "  x:",
                  "    default: 0.25"))
  writeLines(txt, p)

  tree <- orderly_graph("use_dependency", root = root, use = "src",
                        propagate = FALSE, show_all = TRUE,
                        direction = "upstream")
  root <- tree$root
  expect_equal(root$name, "use_dependency")
  expect_length(root$children, 1)
  child_1 <- root$children[[1]]
  expect_equal(child_1$name, "other")
  expect_length(child_1$children, 0)
})


test_that("Sensible error message if query fails", {
  dat <- prepare_orderly_query_example()
  remote <- orderly_remote_path(dat$root)

  path_local <- test_prepare_orderly_example("demo")
  config <- orderly_config_$new(path_local)

  p <- file.path(path_local, "src", "use_dependency", "orderly.yml")
  txt <- readLines(p)
  txt <- sub("latest", "latest(parameter:nmin < x)", txt, fixed = TRUE)
  txt <- c(txt, c("parameters:",
                  "  x:",
                  "    default: 0.25"))
  writeLines(txt, p)

  expect_error(
    orderly_pull_dependencies("use_dependency", parameters = list(x = 0.05),
                              remote = remote, root = path_local),
    "Failed to find suitable version of 'other' with query:")

  expect_error(
    orderly_pull_archive("other", "latest(parameter:nmin < x)",
                         parameters = list(x = 0.05),
                         remote = remote, root = path_local),
    "Failed to find suitable version of 'other' with query:")
})

test_that("orderly_graph: can get upstream dependencies in quick mode", {
  path <- prepare_orderly_example("depends", testing = TRUE, git = TRUE)

  deps <- orderly_graph(c("example", "depend", "depend2"),
                        root = path, direction = "upstream",
                        use = "src", validate = FALSE)
  expect_equal(deps, list(
    example = NULL,
    depend = "example",
    depend2 = "example"
  ))

  ## Setup another report on a branch to check works with different ref
  master <- git_checkout_branch("example", root = path, create = TRUE)
  new_report <- file.path(path, "src/depend4")
  dir.create(new_report)
  file.copy(list.files(file.path(path, "src/depend2"), full.names = TRUE),
            file.path(path, "src/depend4"), recursive = TRUE)
  git_run(c("add", "."), root = path, check = TRUE)
  git_run(c("commit", "-m", "'Add depend4 report'"), root = path, check = TRUE)
  hash <- git_run(c("rev-parse", "--short", "HEAD"), root = path, check = TRUE)
  git_checkout_branch(master, root = path)

  deps <- orderly_graph(c("example", "depend", "depend4"),
                        root = path, ref = hash$output,
                        direction = "upstream", use = "src",
                        validate = FALSE)
  expect_equal(deps, list(
    example = NULL,
    depend = "example",
    depend4 = "example"
  ))
})

test_that("orderly_dependencies: can get downstream dependencies", {
  path <- prepare_orderly_example("depends", testing = TRUE, git = TRUE)

  deps <- orderly_graph(c("example", "depend", "depend2"),
                        root = path, use = "src",
                        direction = "downstream", validate = FALSE)
  expect_equal(deps, list(
    example = c("depend", "depend2"),
    depend = NULL,
    depend2 = "depend3"
  ))

  ## Setup another report on a branch to check works with different ref
  master <- git_checkout_branch("example", root = path, create = TRUE)
  new_report <- file.path(path, "src/depend4")
  dir.create(new_report)
  file.copy(list.files(file.path(path, "src/depend2"), full.names = TRUE),
            file.path(path, "src/depend4"), recursive = TRUE)
  git_run(c("add", "."), root = path, check = TRUE)
  git_run(c("commit", "-m", "'Add depend4 report'"), root = path, check = TRUE)
  hash <- git_run(c("rev-parse", "--short", "HEAD"), root = path, check = TRUE)
  git_checkout_branch(master, root = path)

  deps <- orderly_graph(c("example", "depend", "depend4"),
                        root = path, use = "src", ref = hash$output,
                        direction = "downstream", validate = FALSE)
  expect_equal(deps, list(
    example = c("depend", "depend2", "depend4"),
    depend = NULL,
    depend4 = NULL
  ))
})
