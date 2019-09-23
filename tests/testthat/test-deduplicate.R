context("deduplicate")

test_that("deduplicate demo", {
  skip_on_cran()
  path <- create_orderly_demo()
  config <- orderly_config_get(path, locate)
  x <- orderly_deduplicate_info(config)

  expect_true(all(x$untracked$internal))
  expect_true("orderly_run.rds" %in% basename(x$untracked$path))

  repr <- format(x)
  expect_match(repr, "- \\d{2,} tracked files", all = FALSE)
  expect_match(repr, "- \\d{2,} duplicate files", all = FALSE)
  expect_match(repr, "- 0 deduplicated files", all = FALSE)
  expect_match(repr, "- 0 B deduplicated size", all = FALSE)
  expect_match(repr, "- 0 untracked files", all = FALSE)
  expect_match(repr, "- 0 B untracked size", all = FALSE)

  expect_output(res <- withVisible(print(x)),
                paste0(repr, collapse = "\n"),
                fixed = TRUE)
  expect_equal(res, list(value = x, visible = FALSE))

  plan <- orderly_deduplicate_prepare(x)
  expect_is(plan, "data.frame")
  expect_equal(names(plan), c("from", "to"))

  ## This will need updating every time we change the demo pretty
  ## much.

  expect_equal(nrow(plan), 43)

  orderly_deduplicate_run(plan)

  y <- orderly_deduplicate_info(config)
  expect_equal(sum(y$files$state == "linked"), 43)
  expect_equal(sum(y$files$state == "duplicated"), 0)
  expect_equal(x$files$state == "distinct",
               y$files$state == "distinct")
})


test_that("simpler use", {
  skip_on_cran()
  path <- orderly_example("demo")
  id1 <- orderly_run("minimal", root = path, echo = FALSE)
  id2 <- orderly_run("minimal", root = path, echo = FALSE)
  id3 <- orderly_run("minimal", root = path, echo = FALSE)
  orderly_commit(id1, root = path)
  orderly_commit(id2, root = path)
  info1 <- orderly_deduplicate(path, dry_run = FALSE, quiet = TRUE)
  info2 <- orderly_deduplicate(path, dry_run = FALSE, quiet = TRUE)
  orderly_commit(id3, root = path)
  info3 <- orderly_deduplicate(path, dry_run = FALSE, quiet = TRUE)
  info4 <- orderly_deduplicate(path, dry_run = FALSE, quiet = TRUE)

  expect_equal(sum(info1$files$state == "duplicated"), 3)
  expect_equal(sum(info2$files$state == "duplicated"), 0)
  expect_equal(sum(info3$files$state == "duplicated"), 3)
  expect_equal(sum(info4$files$state == "duplicated"), 0)

  expect_equal(sum(info1$files$state == "linked"), 0)
  expect_equal(sum(info2$files$state == "linked"), 3)
  expect_equal(sum(info3$files$state == "linked"), 3)
  expect_equal(sum(info4$files$state == "linked"), 6)
})


test_that("print on run", {
  skip_on_cran()
  path <- orderly_example("demo")
  id1 <- orderly_run("minimal", root = path, echo = FALSE)
  id2 <- orderly_run("minimal", root = path, echo = FALSE)
  orderly_commit(id1, root = path)
  orderly_commit(id2, root = path)

  expect_output(
    orderly_deduplicate(path, dry_run = TRUE, quiet = FALSE),
    "Deduplication information for")
  expect_silent(
    orderly_deduplicate(path, dry_run = TRUE, quiet = TRUE))
})


test_that("deduplicate empty", {
  skip_on_cran()
  path <- orderly_example("demo")
  x <- orderly_deduplicate_info(orderly_config(path))
  expect_is(x, "orderly_deduplicate_info")
  expect_equal(nrow(x$files), 0)
  expect_equal(orderly_deduplicate(path, quiet = TRUE), x)
})


test_that("don't deduplicate across filesystem boundaries", {
  skip_on_cran()
  path <- orderly_example("demo")
  id1 <- orderly_run("minimal", root = path, echo = FALSE)
  id2 <- orderly_run("minimal", root = path, echo = FALSE)
  orderly_commit(id1, root = path)
  orderly_commit(id2, root = path)
  info <- orderly_deduplicate_info(orderly_config(path))
  info$files$device_id[[2]] <- info$files$device_id[[1]] + 1
  expect_error(
    orderly_deduplicate_prepare(info),
    "Can't deduplicate as your orderly archive spans multiple devices")
})


test_that("don't deduplicate hardlinked files", {
  skip_on_cran()
  path <- orderly_example("demo")
  id1 <- orderly_run("minimal", root = path, echo = FALSE)
  id2 <- orderly_run("minimal", root = path, echo = FALSE)
  orderly_commit(id1, root = path)
  orderly_commit(id2, root = path)
  info <- orderly_deduplicate_info(orderly_config(path))
  info$files$hard_links[[5]] <- 2
  expect_error(
    orderly_deduplicate_prepare(info),
    "Can't deduplicate files that have been hardlinked elsewhere")
})


test_that("don't deduplicate after file modification", {
  skip_on_cran()
  path <- orderly_example("demo")
  id1 <- orderly_run("minimal", root = path, echo = FALSE)
  id2 <- orderly_run("minimal", root = path, echo = FALSE)
  orderly_commit(id1, root = path)
  orderly_commit(id2, root = path)

  ## truncate the file
  file.create(file.path(path, "archive", "minimal", id1, "script.R"))

  info <- orderly_deduplicate_info(orderly_config(path))
  expect_equal(sum(!info$files$unchanged), 1)

  expect_error(
    orderly_deduplicate_prepare(info),
    "Can't deduplicate files that have been modified:.+- minimal/.+/script.R")
})

test_that("relink basic case", {
  skip_on_cran()
  path <- tempfile()
  from <- file.path(path, "a")
  to <- file.path(path, "b")
  dir.create(path)
  writeLines("a", from)
  writeLines("b", to)
  info <- fs::file_info(c(from, to))$inode
  relink(from, to)
  expect_true(all(fs::file_info(c(from, to))$inode == info[[2]]))
})


test_that("relink error handling", {
  skip_on_cran()
  path <- tempfile()
  from <- file.path(path, "a")
  to <- file.path(path, "b")
  dir.create(path)
  writeLines("a", from)
  writeLines("b", to)

  mockery::stub(relink, "fs::link_create",
                function(...) stop("Some error linking"))
  info <- fs::file_info(c(from, to))$inode
  expect_error(relink(from, to), "Some error linking")
  expect_true(all(fs::file_info(c(from, to))$inode == info))
})
