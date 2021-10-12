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
