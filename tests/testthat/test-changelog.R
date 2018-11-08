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


test_that("changelog consistency: no new entries", {
  old <- data_frame(from_file = TRUE, label = "a", value = "x")
  new <- data_frame(from_file = TRUE, label = "a", value = "x")
  expect_equal(changelog_compare(old, new), old[integer(0), ])
})


test_that("changelog consistency: all from file", {
  d <- data_frame(from_file = c(TRUE, TRUE, TRUE),
                  label = c("a", "b", "c"),
                  value = c("x", "y", "z"))

  ## All four possible cases here:
  expect_equal(changelog_compare(d, d), d[integer(0), ])
  expect_equal(changelog_compare(d, d[2:3, ]), d[1, ])
  expect_equal(changelog_compare(d, d[3, ]), d[1:2, ])
  expect_equal(changelog_compare(d, NULL), d)
})


test_that("changelog consistency: incl from file", {
  d <- data_frame(from_file = c(TRUE, FALSE, TRUE),
                  label = c("a", "b", "c"),
                  value = c("x", "y", "z"))
  e <- d[-2, ]

  ## All four possible cases here:
  expect_equal(changelog_compare(e, d), d[integer(0), ])
  expect_equal(changelog_compare(e, d[2:3, ]), d[1, ])
  ## This can't actully happen
  expect_equal(changelog_compare(e, d[3, ]), d[1, ])
  expect_equal(changelog_compare(e, NULL), e)
})


test_that("changelog inconsistency: complete mismatch", {
  d <- data_frame(from_file = c(TRUE, TRUE, TRUE),
                  label = c("a", "b", "c"),
                  value = c("x", "y", "z"))
  e <- data_frame(from_file = TRUE,
                  label = "A",
                  value = "X")
  expect_error(changelog_compare(e, d),
               paste("Missing previously existing changelog entries:",
                     "[a]: x", "[b]: y", "[c]: z", sep = "\n"),
               fixed = TRUE)
})


test_that("changelog inconsistency: altered past", {
  d <- data_frame(from_file = TRUE,
                  label = c("b", "c"),
                  value = c("y", "z"))
  e <- data_frame(from_file = TRUE,
                  label = c("a", "b", "c"),
                  value = c("x", "y", "Z"))
  expect_error(changelog_compare(e, d),
               paste("Missing previously existing changelog entries:",
                     "[c]: z", sep = "\n"),
               fixed = TRUE)
})


test_that("changelog inconsistency: inserted past", {
  d <- data_frame(from_file = TRUE,
                  label = c("b", "c"),
                  value = c("y", "z"))
  e <- data_frame(from_file = TRUE,
                  label = c("a", "b", "!", "c"),
                  value = c("x", "y", "@", "z"))
  expect_error(changelog_compare(e, d),
               paste("Invalidly added historical changelog entries:",
                     "[!]: @", sep = "\n"),
               fixed = TRUE)
})


test_that("test badly formed messages", {
  expect_error(changelog_message_parse("test message"),
               "message must be of the form '[<label>] <message>'",
               fixed = TRUE)
  expect_error(changelog_message_parse(c("[a] test message", "b")),
               "message must be of the form '[<label>] <message>'",
               fixed = TRUE)
})


test_that("parse messages", {
  expect_equal(changelog_message_parse("[a] x"),
               data_frame(label = "a", value = "x", from_file = FALSE))
  expect_equal(changelog_message_parse(c("[a] x", "[b] y")),
               data_frame(label = c("a", "b"), value = c("x", "y"),
                          from_file = FALSE))
  expect_equal(changelog_message_parse(c("[a ] x  ", "[b]   y")),
               data_frame(label = c("a", "b"), value = c("x", "y"),
                          from_file = FALSE))
})


test_that("append changelog", {
  path <- prepare_orderly_example("changelog")

  tmp <- tempfile()
  path_example <- file.path(path, "src", "example")
  path_cl <- path_changelog_txt(path_example)

  writeLines(c("[label1]", "value1"), path_cl)

  id1 <- orderly_run("example", config = path, echo = FALSE)
  p1 <- orderly_commit(id1, config = path)

  l1 <- changelog_read_json(p1)
  expect_equal(l1,
               data_frame(label = "label1",
                          value = "value1",
                          from_file = TRUE,
                          report_version = id1))

  con <- orderly_db("destination", config = path)
  d <- DBI::dbReadTable(con, "changelog")
  d$from_file <- as.logical(d$from_file)
  expect_equal(d[names(l1)], l1)
  expect_setequal(names(d), c(names(l1), "id"))

  txt <- c("[label2]", "value2", readLines(path_cl))
  writeLines(txt, path_cl)

  id2 <- orderly_run("example", config = path, echo = FALSE)
  p2 <- orderly_commit(id2, config = path)

  l2 <- changelog_read_json(p2)
  expect_equal(changelog_read_json(p2),
               data_frame(label = c("label2", "label1"),
                          value = c("value2", "value1"),
                          from_file = TRUE,
                          report_version = c(id2, id1)))

  id3 <- orderly_run("example", config = path, echo = FALSE)
  p3 <- orderly_commit(id3, config = path)
  expect_equal(changelog_read_json(p3),
               changelog_read_json(p2))
})


test_that("label change requires rebuild", {
  path <- prepare_orderly_example("changelog")

  tmp <- tempfile()
  path_example <- file.path(path, "src", "example")
  path_cl <- path_changelog_txt(path_example)

  writeLines(c("[label1]", "value1"), path_cl)
  id1 <- orderly_run("example", config = path, echo = FALSE)
  p1 <- orderly_commit(id1, config = path)

  d <- yaml_read(file.path(path, "orderly_config.yml"))
  d$changelog <- d$changelog[1]
  yaml_write(d, file.path(path, "orderly_config.yml"))

  writeLines(c("[label1]", "value2", "[label1]", "value1"), path_cl)
  id2 <- orderly_run("example", config = path, echo = FALSE)
  expect_error(
    orderly_commit(id2, config = path),
    "changelog labels have changed: rebuild with orderly::orderly_rebuild")

  orderly_rebuild(config = path)
  p2 <- orderly_commit(id2, config = path)
})
