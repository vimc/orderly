orderly_search <- function(query, name, root = NULL, locate = TRUE) {
  config <- orderly_config_get(root, locate)

  dat <- parse_query(query)
  parameters <- search_parameters_archive(name, config)

  search1 <- function(p, expr, base) {
    env <- new.env(parent = base)
    env$parameter <- p
    eval(expr, env)
  }

  env <- orderly_search_env()
  res <- vlapply(parameters, search1, dat$expr, env)
  names(res)[res]
}


orderly_search_env <- function() {
  env <- new.env()
  for (op in c("(", parse_query_operators)) {
    env[[op]] <- get(op)
  }
  safe_op <- function(op) {
    force(op)
    function(a, b) {
      !is.null(a) && !is.null(b) && op(a, b)
    }
  }
  for (op in parse_query_join) {
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


parse_query_operators <- c("is.null", "==", "!=", ">", ">=", "<", "<=")
parse_query_join <- c("(", "&&", "||", "!")


parse_query <- function(x) {
  expr <- parse(text = x)
  if (length(expr) != 1L) {
    stop("Expected a single expression")
  }
  dat <- parse_query_expr(expr[[1L]])

  info <- data_frame(type = dat$type, key = dat$key)
  expr <- dat$expr
  list(info = info, expr = expr)
}


parse_query_expr <- function(expr) {
  if (!is.recursive(expr)) {
    stop("Invalid expression!")
  }

  fn <- deparse(expr[[1]])
  if (fn %in% parse_query_operators) {
    return(parse_query_filter(expr))
  }

  if (!(fn %in% parse_query_join)) {
    stop("Invalid operation")
  }

  a <- parse_query_expr(expr[[2]])
  expr[[2]] <- a$expr

  if (fn %in% c("&&", "||")) {
    b <- parse_query_expr(expr[[3]])
    expr[[3]] <- b$expr
  } else {
    b <- NULL
  }

  list(type = c(a$type, b$type),
       key = c(a$key, b$key),
       expr = expr)
}


parse_query_filter <- function(expr) {
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
    if (!is.atomic(value)) {
      stop("Query value must be atomic (logical, numeric, string)")
    }
  }

  expr[[2]] <- bquote(.(as.name(type))[[.(key)]])

  list(type = type, key = key, expr = expr)
}
