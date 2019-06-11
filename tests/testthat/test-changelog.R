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
  d <- data_frame(id = ids::random_id(3),
                  from_file = c(TRUE, TRUE, TRUE),
                  label = c("a", "b", "c"),
                  value = c("x", "y", "z"))

  ## All four possible cases here:
  expect_equal(changelog_compare(d, d), d[integer(0), ])

  res <- changelog_compare(d[-1], d[2:3, ])
  expect_equal(res[-1], d[1, -1])
  expect_match(res$id, "^[[:xdigit:]]{32}")

  res <- changelog_compare(d[-1], d[3, ])
  expect_equal(res[-1], d[1:2, -1])
  expect_match(res$id, "^[[:xdigit:]]{32}")

  res <- changelog_compare(d[-1], NULL)
  expect_equal(res[-1], d[-1])
  expect_match(res$id, "^[[:xdigit:]]{32}")
})


test_that("changelog consistency: incl from file", {
  d <- data_frame(id = ids::random_id(3),
                  from_file = c(TRUE, FALSE, TRUE),
                  label = c("a", "b", "c"),
                  value = c("x", "y", "z"))
  e <- d[-2, ]

  ## All four possible cases here:
  expect_equal(changelog_compare(e, d), d[integer(0), ])

  res <- changelog_compare(e[-1], d[2:3, ])
  expect_equal(res[-1], d[1, -1])
  expect_match(res$id, "^[[:xdigit:]]{32}")

  res <- changelog_compare(e[-1], d[3, ])
  expect_equal(res[-1], d[1, -1])
  expect_match(res$id, "^[[:xdigit:]]{32}")
  expect_false(res$id %in% d$id)

  res <- changelog_compare(e[-1], NULL)
  expect_equal(res[-1], e[-1])
  expect_match(res$id, "^[[:xdigit:]]{32}")
  expect_false(any(res$id %in% d$id))
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

  id1 <- orderly_run("example", root = path, echo = FALSE)
  p1 <- orderly_commit(id1, root = path)

  l1 <- readRDS(path_orderly_run_rds(p1))$meta$changelog
  expect_equal(l1[names(l1) != "id"],
               data_frame(label = "label1",
                          value = "value1",
                          from_file = TRUE,
                          report_version = id1))
  expect_match(l1$id, "^[[:xdigit:]]{32}$")

  con <- orderly_db("destination", root = path)
  on.exit(DBI::dbDisconnect(con))
  d <- DBI::dbReadTable(con, "changelog")
  d$from_file <- as.logical(d$from_file)
  expect_equal(d[names(l1)], l1)
  expect_setequal(names(d), c(names(l1), "ordering", "report_version_public"))
  expect_equal(d$ordering, 1L)

  txt <- c("[label2]", "value2", readLines(path_cl))
  writeLines(txt, path_cl)

  id2 <- orderly_run("example", root = path, echo = FALSE)
  p2 <- orderly_commit(id2, root = path)

  l2 <- readRDS(path_orderly_run_rds(p2))$meta$changelog
  expect_equal(l2[names(l2) != "id"],
               data_frame(label = c("label2", "label1"),
                          value = c("value2", "value1"),
                          from_file = TRUE,
                          report_version = c(id2, id1)))
  expect_equal(l2$id[[2]], l1$id)

  expect_setequal(DBI::dbReadTable(con, "changelog")$ordering, 1:2)

  id3 <- orderly_run("example", root = path, echo = FALSE)
  p3 <- orderly_commit(id3, root = path)
  l3 <- readRDS(path_orderly_run_rds(p3))$meta$changelog
  expect_equal(l3, l2)
})


test_that("label change requires rebuild", {
  path <- prepare_orderly_example("changelog")

  tmp <- tempfile()
  path_example <- file.path(path, "src", "example")
  path_cl <- path_changelog_txt(path_example)

  writeLines(c("[label1]", "value1"), path_cl)
  id1 <- orderly_run("example", root = path, echo = FALSE)
  p1 <- orderly_commit(id1, root = path)

  d <- yaml_read(file.path(path, "orderly_config.yml"))
  d$changelog <- d$changelog[1]
  yaml_write(d, file.path(path, "orderly_config.yml"))

  writeLines(c("[label1]", "value2", "[label1]", "value1"), path_cl)
  id2 <- orderly_run("example", root = path, echo = FALSE)
  expect_error(
    orderly_commit(id2, root = path),
    "changelog labels have changed: rebuild with orderly::orderly_rebuild")

  orderly_rebuild(root = path)
  p2 <- orderly_commit(id2, root = path)
})


test_that("label values are checked", {
  path <- prepare_orderly_example("changelog")

  tmp <- tempfile()
  path_example <- file.path(path, "src", "example")
  path_cl <- path_changelog_txt(path_example)

  writeLines(c("[label]", "value1"), path_cl)
  expect_error(
    orderly_run("example", root = path, echo = FALSE),
    "Unknown changelog label: 'label'. Use one of 'label1', 'label2'")
})


test_that("reports can't use changelogs if not enabled", {
  path <- prepare_orderly_example("minimal")
  tmp <- tempfile()
  path_example <- file.path(path, "src", "example")
  path_cl <- path_changelog_txt(path_example)
  writeLines(c("[label1]", "value1"), path_cl)
  expect_error(
    orderly_run("example", root = path, echo = FALSE),
    "report 'example' uses changelog, but this is not enabled",
    fixed = TRUE)
})


test_that("public changelog", {
  path <- prepare_orderly_example("changelog")

  name <- "example"
  ids <- character(10)
  for (i in seq_along(ids)) {
    ids[[i]] <- orderly_run(name, root = path, echo = FALSE,
                            message = sprintf("[label1] %d", i))
    orderly_commit(ids[[i]], root = path)
  }

  con <- orderly_db("destination", root = path)
  on.exit(DBI::dbDisconnect(con))

  orderly_publish(ids[[2]], root = path)
  expect_equal(
    DBI::dbReadTable(con, "changelog")$report_version_public,
    rep(c(ids[[2]], NA_character_), c(2, length(ids) - 2)))

  orderly_publish(ids[[6]], root = path)
  expect_equal(
    DBI::dbReadTable(con, "changelog")$report_version_public,
    rep(c(ids[[2]], ids[[6]], NA_character_), c(2, 4, 4)))

  orderly_publish(ids[[5]], root = path)
  expect_equal(
    DBI::dbReadTable(con, "changelog")$report_version_public,
    rep(c(ids[[2]], ids[[5]], ids[[6]], NA_character_), c(2, 3, 1, 4)))

  orderly_publish(ids[[2]], FALSE, root = path)
  expect_equal(
    DBI::dbReadTable(con, "changelog")$report_version_public,
    rep(c(ids[[5]], ids[[6]], NA_character_), c(5, 1, 4)))

  ## This should all survive a rebuild
  prev <- DBI::dbReadTable(con, "changelog")
  orderly_rebuild(root = path)
  expect_equal(DBI::dbReadTable(con, "changelog"), prev)
})


test_that("public changelog with multiple entries", {
  path <- prepare_orderly_example("changelog")

  name <- "example"
  ids <- character(3)

  ids[[1]] <- orderly_run(name, root = path, echo = FALSE,
                          message = paste("[label1]", c("a", "b", "c")))
  ids[[2]] <- orderly_run(name, root = path, echo = FALSE,
                          message = paste("[label1]", c("d")))
  ids[[3]] <- orderly_run(name, root = path, echo = FALSE,
                          message = paste("[label1]", c("e", "f")))
  for (i in ids) {
    orderly_commit(i, root = path)
  }

  con <- orderly_db("destination", root = path)
  on.exit(DBI::dbDisconnect(con))

  orderly_publish(ids[[2]], root = path)
  expect_equal(
    DBI::dbReadTable(con, "changelog")$report_version_public,
    rep(c(ids[[2]], NA_character_), c(4, 2)))

  orderly_publish(ids[[1]], root = path)
  expect_equal(
    DBI::dbReadTable(con, "changelog")$report_version_public,
    rep(c(ids[[1]], ids[[2]], NA_character_), c(3, 1, 2)))

  orderly_publish(ids[[3]], root = path)
  expect_equal(
    DBI::dbReadTable(con, "changelog")$report_version_public,
    rep(c(ids[[1]], ids[[2]], ids[[3]]), c(3, 1, 2)))

  orderly_publish(ids[[2]], FALSE, root = path)
  expect_equal(
    DBI::dbReadTable(con, "changelog")$report_version_public,
    rep(c(ids[[1]], ids[[3]]), c(3, 3)))
})


test_that("public version has no changelog", {
  path <- prepare_orderly_example("changelog")

  name <- "example"
  ids <- character(4)
  for (i in seq_along(ids)) {
    msg <- if (i != 3) sprintf("[label1] %d", i)
    ids[[i]] <- orderly_run(name, root = path, echo = FALSE, message = msg)
    orderly_commit(ids[[i]], root = path)
  }

  con <- orderly_db("destination", root = path)
  DBI::dbReadTable(con, "changelog")

  orderly_publish(ids[[3]], root = path)
  expect_equal(
    DBI::dbReadTable(con, "changelog")$report_version_public,
    rep(c(ids[[3]], NA_character_), c(2, 1)))
})
