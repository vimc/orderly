context("query (search)")

test_that("parse query filter", {
  expect_equal(parse_query_filter(quote(a == 1), NULL),
               list(type = "parameter",
                    expr = quote(parameter[["a"]] == 1)))
  expect_equal(
    parse_query_filter(quote(parameter:a == "value"), NULL),
    list(type = "parameter",
         expr = quote(parameter[["a"]] == "value")))
  expect_equal(
    parse_query_filter(quote(is.null(parameter:a)), NULL),
    list(type = "parameter",
         expr = quote(is.null(parameter[["a"]]))))

  expect_equal(
    parse_query_filter(quote(tag:mytag), NULL),
    list(type = "tag", expr = quote("mytag" %in% tag)))

  expect_error(
    parse_query_filter(quote(parameter:a == list(1, 2)), NULL),
    "Query value must be atomic (logical, numeric, string)",
    fixed = TRUE)
  expect_error(
    parse_query_filter(quote(parameter:a %in% 1), NULL),
    "Query relationship '%in%' not allowed",
    fixed = TRUE)
  expect_error(
    parse_query_filter(quote(f(a, b, c)), NULL),
    "Invalid query expression 'f(a, b, c)'",
    fixed = TRUE)
  expect_error(
    parse_query_filter(quote(f()), NULL),
    "Invalid query expression 'f()'",
    fixed = TRUE)

  expect_error(
    parse_query_filter(quote(parameter:a == b), NULL),
    "Query parameter 'b' not found in supplied parameters")
  expect_equal(
    parse_query_filter(quote(parameter:a == b), list(b = 1)),
    list(type = "parameter",
         expr = quote(parameter[["a"]] == 1)))
  expect_equal(
    parse_query_filter(quote(parameter:a == a), list(a = 1, b = 2)),
    list(type = "parameter",
         expr = quote(parameter[["a"]] == 1)))
})


test_that("parse query expression", {
  expect_equal(
    parse_query_expr(quote(a == 1), NULL),
    list(type = "parameter",
         expr = quote(parameter[["a"]] == 1)))
  expect_equal(
    parse_query_expr(quote(a == 1 && parameter:b == "value"), NULL),
    list(type = c("parameter", "parameter"),
         expr = quote(parameter[["a"]] == 1 && parameter[["b"]] == "value")))
  expect_equal(
    parse_query_expr(quote(a == 1 && (b == "value" || b == "other")), NULL),
    list(type = c("parameter", "parameter", "parameter"),
         expr = quote(parameter[["a"]] == 1 &&
                      (parameter[["b"]] == "value" ||
                       parameter[["b"]] == "other"))))

  expect_equal(
    parse_query_expr(quote(tag:mytag), NULL),
    list(type = "tag", expr = quote("mytag" %in% tag)))
  ## Scary but correctt:
  expect_equal(
    parse_query_expr(quote(!tag:mytag), NULL),
    list(type = "tag", expr = quote(!"mytag" %in% tag)))
  expect_equal(
    parse_query_expr(quote(a > 1 && (tag:mytag || parameter:b == "use")), NULL),
    list(type = c("parameter", "tag", "parameter"),
         expr = quote(parameter[["a"]] > 1 &&
                      ("mytag" %in% tag || parameter[["b"]] == "use"))))
})


test_that("parse query", {
  res <- parse_query('a > 1', NULL)
  expect_false(res$latest)
  expect_equal(res$use, list(parameter = TRUE, tag = FALSE))
  expect_equal(res$expr, quote(parameter[["a"]] > 1))

  res <- parse_query('a > 1 && parameter:b == "value"', NULL)
  expect_false(res$latest)
  expect_equal(res$use, list(parameter = TRUE, tag = FALSE))
  expect_equal(res$expr,
               quote(parameter[["a"]] > 1 && parameter[["b"]] == "value"))

  res <- parse_query('latest(a > 1 && (b == "value" || b == "other"))', NULL)
  expect_true(res$latest)
  expect_equal(res$use, list(parameter = TRUE, tag = FALSE))
  expect_equal(res$expr,
               quote(parameter[["a"]] > 1 &&
                     (parameter[["b"]] == "value" ||
                      parameter[["b"]] == "other")))

  res <- parse_query('a > 1 && tag:weekly')
})


test_that("integration", {
  skip_on_cran_windows()
  root <- prepare_orderly_example("demo")

  f <- function(nmin) {
    id <- orderly_run("other", root = root, parameters = list(nmin = nmin),
                      echo = FALSE)
    orderly_commit(id, root = root)
    id
  }

  ids <- c(f(0.1), f(0.2), f(0.3))

  expect_equal(
    orderly_search("nmin > 0.15", "other", root = root),
    ids[2:3])
  expect_equal(
    orderly_search("nmin > 0.0", "other", root = root),
    ids)
  expect_equal(
    orderly_search("latest(nmin > 0.0)", "other", root = root),
    ids[[3]])
  expect_equal(
    orderly_search("nmin > 1.0", "other", root = root),
    character(0))
  expect_equal(
    orderly_search("is.null(nmin)", "other", root = root),
    character(0))
  expect_equal(
    orderly_search("nmin > x", "other", list(x = 0.25), root = root),
    ids[[3]])
})


test_that("query on a tag", {
  skip_on_cran_windows()
  root <- prepare_orderly_example("minimal")
  append_lines(
    c("tags:",
      "  - weekly",
      "  - monthly"),
    file.path(root, "orderly_config.yml"))

  f <- function(tag) {
    id <- orderly_run("example", root = root, tags = tag, echo = FALSE)
    orderly_commit(id, root = root)
    id
  }

  ids <- c(f(c("weekly", "monthly")), f("monthly"), f(NULL), f("weekly"))

  expect_equal(
    orderly_search('tag:weekly', "example", root = root),
    ids[c(1, 4)])
  expect_equal(
    orderly_search('tag:monthly', "example", root = root),
    ids[c(1, 2)])
  expect_equal(
    orderly_search('tag:monthly && !tag:weekly', "example", root = root),
    ids[2])
  expect_equal(
    orderly_search('tag:monthly || tag:weekly', "example", root = root),
    ids[c(1, 2, 4)])
  expect_equal(
    orderly_search('tag:yearly', "example", root = root),
    character(0))

  expect_equal(
    orderly_search('latest(tag:weekly)', "example", root = root),
    ids[[4]])
  expect_equal(
    orderly_search('latest(tag:monthly)', "example", root = root),
    ids[[2]])
  expect_equal(
    orderly_search('latest(tag:monthly && !tag:weekly)', "example",
                   root = root),
    ids[[2]])
  expect_equal(
    orderly_search('latest(tag:monthly || tag:weekly)', "example", root = root),
    ids[[4]])
  expect_equal(
    orderly_search('latest(tag:yearly)', "example", root = root),
    NA_character_)
})


test_that("Query environment tricks", {
  env <- orderly_search_env()
  expect_false(eval(quote(1 == 2), env))
  expect_true(eval(quote(1 == 1), env))
  expect_false(eval(quote(1 == NULL), env))
})


test_that("search in drafts", {
  skip_on_cran_windows()
  root <- prepare_orderly_example("demo")

  f <- function(nmin, tags = NULL) {
    orderly_run("other", root = root, echo = FALSE,
                parameters = list(nmin = nmin), tags = tags)
  }

  ids <- c(f(0.1), f(0.2, "plot"), f(0.3))

  expect_equal(
    orderly_search("nmin > 0.15", "other", root = root, draft = TRUE),
    ids[2:3])
  expect_equal(
    orderly_search("tag:plot", "other", root = root, draft = TRUE),
    ids[2])

  ## then commit the last one
  orderly_commit(ids[[3]], root = root)
  expect_equal(
    orderly_search("nmin > 0.15", "other", root = root, draft = TRUE),
    ids[2])
  expect_equal(
    orderly_search("nmin > 0.15", "other", root = root, draft = FALSE),
    ids[3])
  expect_equal(
    orderly_search("nmin > 0.15", "other", root = root, draft = "newer"),
    ids[2:3])
})


test_that("all together from a report", {
  root <- prepare_orderly_example("demo")

  p <- file.path(root, "src", "use_dependency", "orderly.yml")
  txt <- readLines(p)
  writeLines(sub("latest", "latest(nmin < 0.25)", txt, fixed = TRUE), p)

  f <- function(nmin) {
    id <- orderly_run("other", root = root, parameters = list(nmin = nmin),
                      echo = FALSE)
    orderly_commit(id, root = root)
    id
  }

  ids <- c(f(0.1), f(0.2), f(0.3))
  id <- orderly_run("use_dependency", root = root, echo = FALSE)
  p <- path_orderly_run_rds(file.path(root, "draft", "use_dependency", id))
  d <- readRDS(p)
  expect_equal(d$meta$depends$id, ids[[2]])

  expect_error(
    orderly_run("use_dependency", root = root, echo = FALSE,
                use_draft = TRUE),
    "Query '.+' did not find suitable version")

  id_draft <- orderly_run("other", root = root, parameters = list(nmin = 0.1),
                          echo = FALSE)
  id_new <- orderly_run("use_dependency", root = root, echo = FALSE,
                        use_draft = TRUE)
  p <- path_orderly_run_rds(file.path(root, "draft", "use_dependency", id_new))
  d <- readRDS(p)
  expect_equal(d$meta$depends$id, id_draft)
})


test_that("Query resolution using parameter", {
  root <- prepare_orderly_example("demo")

  p <- file.path(root, "src", "use_dependency", "orderly.yml")
  txt <- readLines(p)
  txt <- sub("latest", "latest(nmin < p)", txt, fixed = TRUE)
  txt <- c(txt,
           "parameters:",
           "  p: ~")
  writeLines(txt, p)

  f <- function(nmin) {
    id <- orderly_run("other", root = root, parameters = list(nmin = nmin),
                      echo = FALSE)
    orderly_commit(id, root = root)
    id
  }

  ids <- c(f(0.1), f(0.2), f(0.3))
  id <- orderly_run("use_dependency", parameters = list(p = 0.25),
                    root = root, echo = FALSE)
  p <- path_orderly_run_rds(file.path(root, "draft", "use_dependency", id))
  d <- readRDS(p)
  expect_equal(d$meta$depends$id, ids[[2]])
})
