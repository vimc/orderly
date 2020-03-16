context("query (search)")

test_that("parse query filter", {
  expect_equal(parse_query_filter(quote(parameter:a == 1), NULL),
               list(namespace = "parameter",
                    expr = quote(parameter[["a"]] == 1)))
  expect_equal(
    parse_query_filter(quote(is.null(parameter:a)), NULL),
    list(namespace = "parameter",
         expr = quote(is.null(parameter[["a"]]))))

  expect_equal(
    parse_query_filter(quote(tag:mytag), NULL),
    list(namespace = "tag", expr = quote("mytag" %in% tag)))

  expect_error(
    parse_query_filter(quote(parameter:a == list(1, 2)), NULL),
    "Expected symbol, namespaced query element or literal value",
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
    list(namespace = "parameter",
         expr = quote(parameter[["a"]] == 1)))
  expect_equal(
    parse_query_filter(quote(parameter:a == a), list(a = 1, b = 2)),
    list(namespace = "parameter",
         expr = quote(parameter[["a"]] == 1)))
})


test_that("parse query expression", {
  expect_equal(
    parse_query_expr(quote(parameter:a == 1), NULL),
    list(namespace = "parameter",
         expr = quote(parameter[["a"]] == 1)))
  expect_equal(
    parse_query_expr(quote(parameter:a == 1 && parameter:b == "value"), NULL),
    list(namespace = c("parameter", "parameter"),
         expr = quote(parameter[["a"]] == 1 && parameter[["b"]] == "value")))
  expect_equal(
    parse_query_expr(quote(
      parameter:a == 1 && (parameter:b == "value"
        || parameter:b == "other")), NULL),
    list(namespace = c("parameter", "parameter", "parameter"),
         expr = quote(parameter[["a"]] == 1 &&
                      (parameter[["b"]] == "value" ||
                       parameter[["b"]] == "other"))))

  expect_equal(
    parse_query_expr(quote(tag:mytag), NULL),
    list(namespace = "tag", expr = quote("mytag" %in% tag)))
  ## Scary but correctt:
  expect_equal(
    parse_query_expr(quote(!tag:mytag), NULL),
    list(namespace = "tag", expr = quote(!"mytag" %in% tag)))
  expect_equal(
    parse_query_expr(quote(parameter:a > 1 &&
                           (tag:mytag || parameter:b == "use")), NULL),
    list(namespace = c("parameter", "tag", "parameter"),
         expr = quote(parameter[["a"]] > 1 &&
                      ("mytag" %in% tag || parameter[["b"]] == "use"))))
})


test_that("parse query", {
  res <- parse_query("parameter:a > 1", NULL)
  expect_false(res$latest)
  expect_equal(res$use, list(parameter = TRUE, tag = FALSE))
  expect_equal(res$expr, quote(parameter[["a"]] > 1))

  res <- parse_query('parameter:a > 1 && parameter:b == "value"', NULL)
  expect_false(res$latest)
  expect_equal(res$use, list(parameter = TRUE, tag = FALSE))
  expect_equal(res$expr,
               quote(parameter[["a"]] > 1 && parameter[["b"]] == "value"))

  res <- parse_query('latest(parameter:a > 1 &&
    (parameter:b == "value" || parameter:b == "other"))', NULL)
  expect_true(res$latest)
  expect_equal(res$use, list(parameter = TRUE, tag = FALSE))
  expect_equal(res$expr,
               quote(parameter[["a"]] > 1 &&
                     (parameter[["b"]] == "value" ||
                      parameter[["b"]] == "other")))

  res <- parse_query("parameter:a > 1 && tag:weekly")
  expect_false(res$latest)
  expect_equal(res$use, list(parameter = TRUE, tag = TRUE))
  expect_equal(res$expr,
               quote(parameter[["a"]] > 1 && "weekly" %in% tag))
})


test_that("search an archive", {
  dat <- prepare_orderly_query_example()
  root <- dat$root
  ids <- dat$ids

  expect_equal(
    orderly_search("parameter:nmin > 0.15", "other", root = root),
    ids[2:3])
  expect_equal(
    orderly_search("parameter:nmin > 0.0", "other", root = root),
    ids)
  expect_equal(
    orderly_search("latest(parameter:nmin > 0.0)", "other", root = root),
    ids[[3]])
  expect_equal(
    orderly_search("parameter:nmin > 1.0", "other", root = root),
    character(0))
  expect_equal(
    orderly_search("is.null(parameter:nmin)", "other", root = root),
    character(0))
  expect_equal(
    orderly_search("parameter:nmin > x", "other", list(x = 0.25), root = root),
    ids[[3]])
})


test_that("query on a tag", {
  skip_on_cran_windows()
  ## NOTE: not using the pre-made example as there's more extensive
  ## tagging done here.
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
    orderly_search("tag:weekly", "example", root = root),
    ids[c(1, 4)])
  expect_equal(
    orderly_search("tag:monthly", "example", root = root),
    ids[c(1, 2)])
  expect_equal(
    orderly_search("tag:monthly && !tag:weekly", "example", root = root),
    ids[2])
  expect_equal(
    orderly_search("tag:monthly || tag:weekly", "example", root = root),
    ids[c(1, 2, 4)])
  expect_equal(
    orderly_search("tag:yearly", "example", root = root),
    character(0))

  expect_equal(
    orderly_search("latest(tag:weekly)", "example", root = root),
    ids[[4]])
  expect_equal(
    orderly_search("latest(tag:monthly)", "example", root = root),
    ids[[2]])
  expect_equal(
    orderly_search("latest(tag:monthly && !tag:weekly)", "example",
                   root = root),
    ids[[2]])
  expect_equal(
    orderly_search("latest(tag:monthly || tag:weekly)", "example", root = root),
    ids[[4]])
  expect_equal(
    orderly_search("latest(tag:yearly)", "example", root = root),
    NA_character_)
})


test_that("Query environment tricks", {
  env <- orderly_search_env()
  expect_false(eval(quote(1 == 2), env))
  expect_true(eval(quote(1 == 1), env))
  expect_false(eval(quote(1 == NULL), env))
})


test_that("search in drafts", {
  dat <- prepare_orderly_query_example(TRUE)
  root <- dat$root
  ids <- dat$ids

  expect_equal(
    orderly_search("parameter:nmin > 0.15", "other", root = root, draft = TRUE),
    ids[2:3])
  expect_equal(
    orderly_search("tag:plot", "other", root = root, draft = TRUE),
    ids[2])

  ## then commit the last one
  orderly_commit(ids[[3]], root = root)
  expect_equal(
    orderly_search("parameter:nmin > 0.15", "other", root = root,
                   draft = TRUE),
    ids[2])
  expect_equal(
    orderly_search("parameter:nmin > 0.15", "other", root = root,
                   draft = FALSE),
    ids[3])
  expect_equal(
    orderly_search("parameter:nmin > 0.15", "other", root = root,
                   draft = "newer"),
    ids[2:3])
})


test_that("all together from a report", {
  dat <- prepare_orderly_query_example()
  root <- dat$root
  ids <- dat$ids

  p <- file.path(root, "src", "use_dependency", "orderly.yml")
  txt <- readLines(p)
  writeLines(
    sub("latest", "latest(parameter:nmin < 0.25)", txt, fixed = TRUE),
    p)

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
  dat <- prepare_orderly_query_example()
  root <- dat$root
  ids <- dat$ids

  p <- file.path(root, "src", "use_dependency", "orderly.yml")
  txt <- readLines(p)
  txt <- sub("latest", "latest(parameter:nmin < p)", txt, fixed = TRUE)
  txt <- c(txt,
           "parameters:",
           "  p: ~")
  writeLines(txt, p)

  id <- orderly_run("use_dependency", parameters = list(p = 0.25),
                    root = root, echo = FALSE)
  p <- path_orderly_run_rds(file.path(root, "draft", "use_dependency", id))
  d <- readRDS(p)
  expect_equal(d$meta$depends$id, ids[[2]])
})


test_that("unknown namespace raises error", {
  expect_error(
    parse_query("something:abc", NULL),
    "Query namespace (used as 'something') must be one of 'tag', 'parameter'",
    fixed = TRUE)
  expect_error(
    parse_query("something:abc > 1", NULL),
    "Query namespace (used as 'something') must be one of 'tag', 'parameter'",
    fixed = TRUE)
  ## TODO:
  expect_error(
    parse_query("tag:abc > 1", NULL),
    "In '.+', query namespace must be 'parameter' but found 'tag'")
})


test_that("Single expression is required", {
  expect_error(
    parse_query("a > 1; b > 2"),
    "Expected a single expression")
  expect_error(
    parse_query("a > 1\nb > 2"),
    "Expected a single expression")
  expect_error(
    parse_query(""),
    "Expected a single expression")
})


test_that("Provided query must be an expression", {
  expect_error(
    parse_query("TRUE"),
    "Invalid query 'TRUE'; expected some sort of expression")
  expect_error(
    parse_query("1"),
    "Invalid query '1'; expected some sort of expression")
  expect_error(
    parse_query("x"),
    "Invalid query 'x'; expected some sort of expression")
  expect_error(
    parse_query("parameter:x > 1 && y"),
    "Invalid query 'y'; expected some sort of expression")
})


test_that("Can't use a parameter without a filter operator", {
  expect_error(
    parse_query("parameter:a"),
    "Invalid query expression 'parameter:a' requires operator")
})


test_that("Namespace and key must be symbols", {
  expect_error(
    parse_query("1:a > 1"),
    "Invalid namespaced query element '1:a'; expected symbol for namespace")
  expect_error(
    parse_query("a:1 > 1"),
    "Invalid namespaced query element 'a:1'; expected symbol for key")
})


## NOTE: this duplicates some of the run code
test_that("query parameter validation", {
  expect_null(query_check_parameters(NULL))
  expect_equal(query_check_parameters(list()), list())
  expect_equal(query_check_parameters(list(a = 1)), list(a = 1))

  expect_error(
    query_check_parameters(list(a = 1:2, b = 2)),
    "Invalid parameters: 'a' - must be scalar")
  expect_error(
    query_check_parameters(list(a = sin, b = 2)),
    "Invalid parameters: 'a' - must be character, numeric or logical")
})


test_that("order of operands", {
  expect_equal(
    parse_query("parameter:x > 1", NULL),
    list(latest = FALSE,
         use = list(parameter = TRUE, tag = FALSE),
         expr = quote(parameter[["x"]] > 1)))
  expect_equal(
    parse_query("1 > parameter:x", NULL),
    list(latest = FALSE,
         use = list(parameter = TRUE, tag = FALSE),
         expr = quote(1 > parameter[["x"]])))
  expect_equal(
    parse_query("parameter:x > x", list(x = 1)),
    list(latest = FALSE,
         use = list(parameter = TRUE, tag = FALSE),
         expr = quote(parameter[["x"]] > 1)))
  expect_equal(
    parse_query("1 > parameter:x", list(x = 1)),
    list(latest = FALSE,
         use = list(parameter = TRUE, tag = FALSE),
         expr = quote(1 > parameter[["x"]])))
})


test_that("is.null requires a namespace", {
  expect_error(
    parse_query("is.null(x)"),
    "Expected namespaced query element but received 'x'")
  expect_error(
    parse_query("is.null(tag:thing)"),
    paste("In 'is.null(tag:thing)', query namespace must be 'parameter'",
          "but found 'tag'"),
    fixed = TRUE)
})
