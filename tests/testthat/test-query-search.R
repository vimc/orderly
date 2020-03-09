context("query (search)")

test_that("parse query filter", {
  expect_equal(parse_query_filter(quote(a == 1)),
               list(type = "parameter",
                    expr = quote(parameter[["a"]] == 1)))
  expect_equal(
    parse_query_filter(quote(parameter:a == "value")),
    list(type = "parameter",
         expr = quote(parameter[["a"]] == "value")))
  expect_equal(
    parse_query_filter(quote(is.null(parameter:a))),
    list(type = "parameter",
         expr = quote(is.null(parameter[["a"]]))))

  expect_equal(
    parse_query_filter(quote(tag:mytag)),
    list(type = "tag", expr = quote("mytag" %in% tag)))

  expect_error(
    parse_query_filter(quote(parameter:a == value)),
    "Query value must be atomic (logical, numeric, string)",
    fixed = TRUE)
  expect_error(
    parse_query_filter(quote(parameter:a %in% 1)),
    "Query relationship '%in%' not allowed",
    fixed = TRUE)
  expect_error(
    parse_query_filter(quote(f(a, b, c))),
    "Invalid query expression 'f(a, b, c)'",
    fixed = TRUE)
  expect_error(
    parse_query_filter(quote(f())),
    "Invalid query expression 'f()'",
    fixed = TRUE)
})


test_that("parse query expression", {
  expect_equal(
    parse_query_expr(quote(a == 1)),
    list(type = "parameter",
         expr = quote(parameter[["a"]] == 1)))
  expect_equal(
    parse_query_expr(quote(a == 1 && parameter:b == "value")),
    list(type = c("parameter", "parameter"),
         expr = quote(parameter[["a"]] == 1 && parameter[["b"]] == "value")))
  expect_equal(
    parse_query_expr(quote(a == 1 && (b == "value" || b == "other"))),
    list(type = c("parameter", "parameter", "parameter"),
         expr = quote(parameter[["a"]] == 1 &&
                      (parameter[["b"]] == "value" ||
                       parameter[["b"]] == "other"))))

  expect_equal(
    parse_query_expr(quote(tag:mytag)),
    list(type = "tag", expr = quote("mytag" %in% tag)))
  ## Scary but correctt:
  expect_equal(
    parse_query_expr(quote(!tag:mytag)),
    list(type = "tag", expr = quote(!"mytag" %in% tag)))
  expect_equal(
    parse_query_expr(quote(a > 1 && (tag:mytag || parameter:b == "use"))),
    list(type = c("parameter", "tag", "parameter"),
         expr = quote(parameter[["a"]] > 1 &&
                      ("mytag" %in% tag || parameter[["b"]] == "use"))))
})


test_that("parse query", {
  res <- parse_query('a > 1')
  expect_equal(res$info, list(parameter = TRUE, tag = FALSE))
  expect_equal(res$expr, quote(parameter[["a"]] > 1))

  res <- parse_query('a > 1 && parameter:b == "value"')
  expect_equal(res$info, list(parameter = TRUE, tag = FALSE))
  expect_equal(res$expr,
               quote(parameter[["a"]] > 1 && parameter[["b"]] == "value"))

  res <- parse_query('a > 1 && (b == "value" || b == "other")')
  expect_equal(res$info, list(parameter = TRUE, tag = FALSE))
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
    orderly_search("nmin > 1.0", "other", root = root),
    character(0))
  expect_equal(
    orderly_search("is.null(nmin)", "other", root = root),
    character(0))
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
    c(ids[c(1, 4)]))
  expect_equal(
    orderly_search('tag:monthly', "example", root = root),
    c(ids[c(1, 2)]))
  expect_equal(
    orderly_search('tag:monthly && !tag:weekly', "example", root = root),
    c(ids[2]))
  expect_equal(
    orderly_search('tag:monthly || tag:weekly', "example", root = root),
    c(ids[c(1, 2, 4)]))
})


test_that("Query environment tricks", {
  env <- orderly_search_env()
  expect_false(eval(quote(1 == 2), env))
  expect_true(eval(quote(1 == 1), env))
  expect_false(eval(quote(1 == NULL), env))
})
