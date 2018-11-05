context("changelog")


test_that("parse empty", {
  expect_equal(changelog_parse(character(0)),
               data_frame(label = character(0), value = character(0)))
})


test_that("parse single", {
  txt <- c("[label]", "entry1")
  expect_equal(changelog_parse(txt),
               data_frame(label = c("label"),
                          value = c("entry1"),
                          from_file = TRUE))
})


test_that("parse multiple", {
  txt <- c("[label]", "entry1", "[other]", "entry2")
  expect_equal(changelog_parse(txt),
               data_frame(label = c("label", "other"),
                          value = c("entry1", "entry2"),
                          from_file = TRUE))
})


test_that("parse multiline", {
  txt <- c("[label]", "entry1", "extra line", "[other]", "entry2")
  expect_equal(changelog_parse(txt),
               data_frame(label = c("label", "other"),
                          value = c("entry1\nextra line", "entry2"),
                          from_file = TRUE))
})


test_that("parse failures", {
  expect_error(changelog_parse(c("some", "text")),
               "Invalid changelog - first line is not a label")

  expect_error(changelog_parse(c("[consecutive]", "[labels]", "value")),
               "Invalid changelog - empty entry on line 1")

  expect_error(changelog_parse(c("[consecutive]", "[labels]", "value",
                                 "[again]", "[another]", "value")),
               "Invalid changelog - empty entries on lines 1, 4")
})


test_that("append changelog", {
  path <- prepare_orderly_example("minimal")

  tmp <- tempfile()
  path_example <- file.path(path, "src", "example")
  path_cl <- path_changelog_txt(path_example)

  writeLines(c("[label]", "value"), path_cl)

  id1 <- orderly_run("example", config = path, echo = FALSE)
  p1 <- orderly_commit(id1, config = path)

  expect_equal(changelog_read_json(p1),
               data_frame(label = "label",
                          value = "value",
                          from_file = TRUE,
                          id = id1))

  txt <- c("[label2]", "value2", readLines(path_cl))
  writeLines(txt, path_cl)

  id2 <- orderly_run("example", config = path, echo = FALSE)
  p2 <- orderly_commit(id2, config = path)

  expect_equal(changelog_read_json(p2),
               data_frame(label = c("label2", "label"),
                          value = c("value2", "value"),
                          from_file = TRUE,
                          id = c(id2, id1)))
})
