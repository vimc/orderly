context("tree_class")

test_that("report_vertex", {
  vert <- report_vertex$new(NULL, "node", "0", TRUE)
  expect_null(vert$parent)
  expect_match(vert$name, "node")
  expect_match(vert$id, "0")
  expect_true(vert$out_of_date)
  expect_true(length(vert$children) == 0)

  printed_vertex <- vert$format()
  expect_match(printed_vertex, "node \\[0\\]")

  child <- report_vertex$new(vert, "child", "1", FALSE)
  expect_false(child$out_of_date)
  expect_match(child$parent$name, "node")

  vert$add_child(child)
  expect_true(length(vert$children) == 1)
})

test_that("report_tree", {
  root <- report_vertex$new(NULL, "root_node", "0", TRUE)
  tree <- report_tree$new(root, "upstream")

  message_init <- tree$set_message("message_1")
  expect_null(message_init)
  new_message <- tree$set_message("message_2")
  expect_match(new_message, "message_1")

  direction <- tree$get_direction()
  expect_match(direction, "upstream")

  tree$add_child(root, "leaf_node", "1", FALSE)

  printed_tree <- tree$format()
  expect_match(printed_tree, "message_2")
  expect_match(printed_tree, "root_node \\[0\\]")
  expect_match(printed_tree, "leaf_node \\[1\\]")
})
