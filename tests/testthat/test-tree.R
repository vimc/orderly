context("tree_class")

test_that("report_tree", {
  tree <- report_tree$new(list(list(name = "root_node", id = "0")), "upstream",
                          depth = 1)

  message_init <- tree$set_message("message_1")
  expect_null(message_init)
  new_message <- tree$set_message("message_2")
  expect_match(new_message, "message_1")

  tree$add_edges(data_frame(name = "root_node",
                            id = "0",
                            child = "leaf_node",
                            child_id = "1",
                            out_of_date = FALSE))

  printed_tree <- tree$format()
  expect_match(printed_tree, "message_2")
  expect_match(printed_tree, "root_node \\[0\\]")
  expect_match(printed_tree, "leaf_node \\[1\\]")
})

test_that("more complicated tree with multiple roots", {
  tree <- report_tree$new(list(list(name = "root_node", id = "0"),
                               list(name = "root2", id = "1")),
                          "upstream", depth = 2)

  tree$add_edges(data_frame(name = c("root_node", "root_node", "child_1"),
                            id = c("0", "0", "c1"),
                            child = c("child_1", "child_2", "child_2"),
                            child_id = c("c1", "c2", "c2"),
                            out_of_date = rep(FALSE, 3)))

  printed_tree <- tree$format()
  tree_lines <- strsplit(printed_tree, "\n")[[1]]
  expect_equal(grep("root_node", tree_lines), 1)
  expect_equal(grep("child_1", tree_lines), 2)
  expect_equal(grep("child_2", tree_lines), c(3, 4))
  expect_equal(grep("root2", tree_lines), 6)
  expect_length(tree_lines, 6)

  tree$add_edges(data_frame(name = c("root2", "child_1"),
                            id = c("1", "c1"),
                            child = c("child_1", "child_2"),
                            child_id = c("c1", "c2"),
                            out_of_date = rep(FALSE, 2)))

  printed_tree <- tree$format()
  tree_lines_2 <- strsplit(printed_tree, "\n")[[1]]
  expect_length(tree_lines_2, 8)
  ## First part is identical to previous
  expect_equal(tree_lines_2[1:6], tree_lines)
  expect_equal(grep("child_1", tree_lines_2), c(2, 7))
  expect_equal(grep("child_2", tree_lines_2), c(3, 4, 8))
})

test_that("can get tree in nested structure", {
  tree <- report_tree$new(list(list(name = "root_node", id = "0"),
                               list(name = "root2", id = "1")),
                          "upstream", depth = 2)

  tree$add_edges(data_frame(name = c("root_node", "root_node", "child_1"),
                            id = c("0", "0", "c1"),
                            child = c("child_1", "child_2", "child_2"),
                            child_id = c("c1", "c2", "c2"),
                            out_of_date = rep(FALSE, 3)))

  out <- tree$get_tree("root_node")
  expect_equal(out,
               list(
                 name = "root_node",
                 id = "0",
                 out_of_date = NULL,
                 dependencies = list(
                   list(
                     name = "child_1",
                     id = "c1",
                     out_of_date = FALSE,
                     dependencies = list(
                       list(
                         name = "child_2",
                         id = "c2",
                         out_of_date = FALSE,
                         dependencies = list()
                       )
                     )
                   ),
                   list(
                     name = "child_2",
                     id = "c2",
                     out_of_date = FALSE,
                     dependencies = list()
                   )
                 )
               ))
})
