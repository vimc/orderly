context("query (search)")

test_that("parse query filter", {
  expect_equal(parse_query_filter(quote(a == 1)),
               list(type = "parameter", key = "a",
                    expr = quote(parameter[["a"]] == 1)))
  expect_equal(
    parse_query_filter(quote(parameter:a == "value")),
    list(type = "parameter", key = "a",
         expr = quote(parameter[["a"]] == "value")))
  expect_equal(
    parse_query_filter(quote(is.null(parameter:a))),
    list(type = "parameter",
         key = "a",
         expr = quote(is.null(parameter[["a"]]))))

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
    list(type = "parameter", key = "a",
         expr = quote(parameter[["a"]] == 1)))
  expect_equal(
    parse_query_expr(quote(a == 1 && parameter:b == "value")),
    list(type = c("parameter", "parameter"), key = c("a", "b"),
         expr = quote(parameter[["a"]] == 1 && parameter[["b"]] == "value")))
  expect_equal(
    parse_query_expr(quote(a == 1 && (b == "value" || b == "other"))),
    list(type = c("parameter", "parameter", "parameter"),
         key = c("a", "b", "b"),
         expr = quote(parameter[["a"]] == 1 &&
                      (parameter[["b"]] == "value" ||
                       parameter[["b"]] == "other"))))
})


test_that("parse query", {
  res <- parse_query('a > 1')
  expect_equal(res$info, data_frame(type = "parameter", key = "a"))
  expect_equal(res$expr, quote(parameter[["a"]] > 1))

  res <- parse_query('a > 1 && parameter:b == "value"')
  expect_equal(res$info, data_frame(type = "parameter", key = c("a", "b")))
  expect_equal(res$expr,

               quote(parameter[["a"]] > 1 && parameter[["b"]] == "value"))

  res <- parse_query('a > 1 && (b == "value" || b == "other")')
  expect_equal(res$info, data_frame(type = "parameter",
                                    key = c("a", "b", "b")))
  expect_equal(res$expr,
               quote(parameter[["a"]] > 1 &&
                     (parameter[["b"]] == "value" ||
                      parameter[["b"]] == "other")))
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
