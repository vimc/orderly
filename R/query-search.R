orderly_search <- function(query, name, parameters = NULL,
                           root = NULL, locate = TRUE) {
  config <- orderly_config_get(root, locate)
  query_parameters_check(parameters)
  dat <- parse_query(query, parameters)

  assert_scalar_character(name)
  versions <- orderly_list_dir(file.path(config$root, "archive", name))

  if (dat$use$parameter) {
    parameters <- search_parameters_archive(name, config)
  } else {
    parameters <- NULL
  }
  if (dat$use$tag) {
    tags <- search_tags_archive(name, config)
  } else {
    tags <- NULL
  }

  search1 <- function(v, expr, base) {
    env <- new.env(parent = base)
    env$parameter <- parameters[[v]]
    env$tag <- tags[[v]]
    eval(expr, env)
  }

  env <- orderly_search_env()
  i <- vlapply(versions, search1, dat$expr, env)
  matches <- names(i)[i]
  if (dat$latest) {
    latest_id(matches)
  } else {
    matches
  }
}


orderly_search_env <- function() {
  env <- new.env()
  for (op in c("(", "is.null", parse_query_join)) {
    env[[op]] <- get(op)
  }
  safe_op <- function(op) {
    op <- match.fun(op)
    function(a, b) {
      !is.null(a) && !is.null(b) && op(a, b)
    }
  }
  for (op in setdiff(parse_query_operators, "is.null")) {
    env[[op]] <- safe_op(op)
  }

  env
}


search_parameters_archive <- function(name, config) {
  con <- orderly_db("destination", config)
  on.exit(DBI::dbDisconnect(con))

  sql <- c("SELECT parameters.*",
           "  FROM parameters",
           "  JOIN report_version",
           "    ON report_version.id = parameters.report_version",
           " WHERE report_version.report = $1")
  res <- DBI::dbGetQuery(con, paste(sql, collapse = "\n"), name)

  value <- as.list(res$value)
  i <- res$type == "number"
  value[i] <- as.numeric(res$value[i])
  i <- res$type == "boolean"
  value[i] <- as.logical(res$value[i])
  names(value) <- res$name
  split(value, res$report_version)
}


search_tags_archive <- function(name, config) {
  con <- orderly_db("destination", config)
  on.exit(DBI::dbDisconnect(con))

  sql <- c("SELECT report_version_tag.*",
           "  FROM report_version_tag",
           "  JOIN report_version",
           "    ON report_version.id = report_version_tag.report_version",
           " WHERE report_version.report = $1")
  res <- DBI::dbGetQuery(con, paste(sql, collapse = "\n"), name)
  split(res$tag, res$report_version)
}


parse_query_operators <- c("is.null", "==", "!=", ">", ">=", "<", "<=")
parse_query_join <- c("(", "&&", "||", "!")


parse_query <- function(x, parameters) {
  expr <- parse(text = x)
  if (length(expr) != 1L) {
    stop("Expected a single expression")
  }
  expr <- expr[[1L]]

  latest <- is_call(expr, "latest")
  if (latest) {
    stopifnot(length(expr) == 2L)
    expr <- expr[[2L]]
  }

  dat <- parse_query_expr(expr, parameters)

  test <- c("parameter", "tag")
  use <- set_names(as.list(test %in% dat$type), test)
  expr <- dat$expr
  list(latest = latest, use = use, expr = expr)
}


parse_query_expr <- function(expr, parameters) {
  if (!is.recursive(expr)) {
    stop("Invalid expression!")
  }

  if (is.call(expr) && deparse(expr[[1L]]) %in% parse_query_join) {
    fn <- deparse(expr[[1]])

    a <- parse_query_expr(expr[[2]], parameters)
    expr[[2]] <- a$expr

    if (fn %in% c("&&", "||")) {
      b <- parse_query_expr(expr[[3]], parameters)
      expr[[3]] <- b$expr
    } else {
      b <- NULL
    }

    list(type = c(a$type, b$type), expr = expr)
  } else {
    parse_query_filter(expr, parameters)
  }
}

## examples
##
## * id == 1
## * parameter:id == 1
## * tag:weekly
parse_query_filter <- function(expr, parameters) {
  if (!is.call(expr)) {
    stop("Expected an expression")
  }

  if (is_call(expr, ":") && length(expr) == 3L) {
    type <- expr[[2L]]
    stopifnot(is.symbol(type))
    if (!identical(type, quote(tag))) {
      stop(sprintf("Invalid query expression '%s' requires operator",
                   paste(deparse(expr), collapse = "\n")),
           call. = FALSE)
    }
    tag <- expr[[3L]]
    stopifnot(is.symbol(tag))
    return(list(type = "tag", expr = bquote(.(as.character(tag)) %in% tag)))
  }

  if (length(expr) < 2L || length(expr) > 3L) {
    stop(sprintf("Invalid query expression '%s'",
                 paste(deparse(expr), collapse = "\n")),
         call. = FALSE)
  }

  type <- "parameter"
  rel <- as.character(expr[[1L]])
  key <- expr[[2L]]

  if (is.symbol(key)) {
    key <- as.character(key)
  } else if (is_call(key, ":")) {
    type <- deparse(key[[2L]])
    match_value(type, "parameter", "Query parameter type")
    key <- deparse(key[[3L]])
  }

  if (!(rel %in% parse_query_operators)) {
    stop(sprintf("Query relationship '%s' not allowed", rel))
  }

  if (rel == "is.null") {
    stopifnot(length(expr) == 2L)
  } else {
    stopifnot(length(expr) == 3L)
    value <- expr[[3L]]
    if (is.name(value)) {
      name <- as.character(value)
      value <- parameters[[name]]
      if (is.null(value)) {
        stop(sprintf(
          "Query parameter '%s' not found in supplied parameters",
          name), call. = FALSE)
      }
      expr[[3L]] <- value
    }
    if (!is.atomic(value)) {
      stop("Query value must be atomic (logical, numeric, string)")
    }
  }

  expr[[2L]] <- bquote(.(as.name(type))[[.(key)]])

  list(type = type, expr = expr)
}


query_parameters_check <- function(parameters) {
  if (!is.null(parameters)) {
    nonscalar <- lengths(parameters) != 1
    if (any(nonscalar)) {
      stop(sprintf(
        "Invalid parameters: %s - must be scalar",
        pasteq(names(nonscalar[nonscalar]))))
    }

    err <- !vlapply(parameters, function(x)
      is.character(x) || is.numeric(x) || is.logical(x))
    if (any(err)) {
      stop(sprintf(
        "Invalid parameters: %s - must be character, numeric or logical",
        pasteq(names(err[err]))))
    }
  }
}
