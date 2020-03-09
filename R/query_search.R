##' Search for orderly reports matching criteria.  This can be used to
##' find reports where a particular parameter or tag was used (it will
##' likely be expanded as time goes on - let us know if that would be
##' useful).  We search within versions of a single report only.
##'
##' The query syntax is deliberately very simple; it may expand a bit
##' later.  At this point you can search for parameters and for tags,
##' and these can be combined.  Note that if you are using OrderlyWeb,
##' then only orderly (and not OrderlyWeb) tags are searched.
##'
##' The idea here is that they queries can be used to find ids that
##' match certain criteria for use as dependencies.  This function
##' lets you work out what would be resolved by the query, and using
##' this query string in a \code{depends:} section will let you select
##' a report that matches some criteria.  For example, suppose that
##' you have report \code{A} that takes a parameter "fruit" with
##' values like "apple", "banana", and a report \code{B} that depends
##' on A.  You could then write:
##'
##' \preformatted{
##' depends:
##'   A:
##'     id: latest(fruit == "apple")
##'     uses:
##'       summary.csv: summary.csv
##' }
##'
##' To get the \code{summary.csv} file out of the latest report
##' \code{A} that was run with the "fruit" parameter set to "apple".
##' If "B" itself takes parameters, you can use those parameters in
##' these query expressions like
##'
##' \preformatted{
##' depends:
##'   A:
##'     id: latest(fruit == target_fruit)
##'     uses:
##'       summary.csv: summary.csv
##' }
##'
##' (assuming that \code{B} takes a parameter \code{target_fruit}).
##'
##' The syntax for tags is simpler, one uses \code{tag:tagname} to
##' test for presence of a tag called "tagname".
##'
##' Search queries can be joined by \code{&&} and \code{||} and
##' grouped using parentheses, these groups (or tags) can be negated
##' with \code{!}, so a complicated query expression might look like:
##'
##' \preformatted{
##' (fruit == "apple" && !tag:weekly) | fruit == "banana"
##' }
##'
##' For clarity, parameters may be prefixed with \code{parameter:}
##' (so, \code{parameter:fruit} in the above), though this is optional.
##'
##' @title Search for orderly reports matching criteria
##'
##' @param query The query string - see details and examples
##'
##' @param name Name of the report to search.  Only a single report
##'   can be searched at once.
##'
##' @param draft Should draft reports be used searched?  This should
##'   be used only in development.  Valid values are logical
##'   (\code{TRUE}, \code{FALSE}) or use the string \code{newer} to
##'   use draft reports where they are newer than archive reports.
##'   For consistency, \code{always} and \code{never} are equivalent
##'   to \code{TRUE} and \code{FALSE}, respectively.
##'
##' @inheritParams orderly_list
##'
##' @return A character vector of matching report ids, possibly
##'   zero-length.  If the query is a "latest" query, then exactly one
##'   report id, possibly NA.
##'
##' @export
##' @examples
##' # We need a few reports here to actually query.  There is a report in
##' # the "demo" example called "other" that takes a parameter "nmin",
##' # which is used to filter data - it's not terribly important what it
##' # does here, but it can give us a set of reports to use.
##'
##' # The demo set also includes configuration for two tags, called
##' # "dataset" and "plot" - the "dataset" tag will always be applied
##' # as it is listed in the orderly.yml but we can still add the
##' # "plot" tag interactively
##' root <- orderly::orderly_example("demo")
##'
##' # A helper function to mass-produce reports will reduce noise a bit
##' run1 <- function(nmin, tags = NULL) {
##'   id <- orderly_run("other", root = root, echo = FALSE,
##'                     parameters = list(nmin = nmin), tags = tags)
##'   orderly_commit(id, root = root)
##'   id
##' }
##'
##' ids <- c(run1(0.1), run1(0.2, "plot"), run1(0.3))
##'
##' # We can then ask for all reports where the parameter nmin was more
##' # than some value
##' orderly::orderly_search("nmin > 0.15", "other", root = root)
##'
##' # Or use "&&" to find tags within a range
##' orderly::orderly_search("nmin > 0.1 && nmin < 0.3", "other", root = root)
##'
##' # We can look for tags
##' orderly::orderly_search("tag:plot", "other", root = root)
##'
##' # or exclude them
##' orderly::orderly_search("!tag:plot", "other", root = root)
##'
##' # or combine that with the presence/absence of a tag
##' orderly::orderly_search("nmin > 0.15 && !tag:plot", "other", root = root)
##'
##' # Use latest() over a query to find the latest report matching the
##' # query expression.
##' orderly::orderly_search("latest(nmin > 0.15)", "other", root = root)
##'
##' # If no reports are found, then a zero-length character vector is returned
##' orderly::orderly_search("nmin > 0.4", "other", root = root)
##'
##' # Or, in the case of latest(), NA
##' orderly::orderly_search("latest(nmin > 0.4)", "other", root = root)
orderly_search <- function(query, name, parameters = NULL, draft = FALSE,
                           root = NULL, locate = TRUE) {
  config <- orderly_config_get(root, locate)
  assert_scalar_character(name)
  parameters <- query_check_parameters(parameters)
  draft <- query_check_draft(draft)

  dat <- parse_query(query, parameters)

  matches <- switch(
    draft,
    never  = orderly_search_do_search(dat, name, FALSE, config),
    always = orderly_search_do_search(dat, name, TRUE, config),
    newer = sort(c(
      orderly_search_do_search(dat, name, TRUE, config),
      orderly_search_do_search(dat, name, FALSE, config))))

  if (dat$latest) {
    latest_id(matches)
  } else {
    matches
  }
}


orderly_search_do_search <- function(dat, name, draft, config) {
  if (draft) {
    where <- "draft"
    search_parameters <- search_parameters_draft
    search_tags <- search_tags_draft
  } else {
    where <- "archive"
    search_parameters <- search_parameters_archive
    search_tags <- search_tags_archive
  }
  versions <- orderly_list_dir(file.path(config$root, where, name))

  if (dat$use$parameter) {
    parameters <- search_parameters(name, config)
  } else {
    parameters <- NULL
  }
  if (dat$use$tag) {
    tags <- search_tags(name, config)
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
  names(i)[i]
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


search_x_draft <- function(name, config, extract) {
  path <- file.path(config$root, "draft", name)
  versions <- orderly_list_dir(path)
  f <- function(p) {
    extract(readRDS(path_orderly_run_rds(p)))
  }
  set_names(lapply(file.path(path, versions), f), versions)
}


search_parameters_draft <- function(name, config) {
  search_x_draft(name, config, function(x) x$meta$parameters)
}


search_tags_draft <- function(name, config) {
  search_x_draft(name, config, function(x) x$meta$tags)
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


query_check_parameters <- function(parameters) {
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
  parameters
}


query_check_draft <- function(draft, as) {
  if (is.logical(draft)) {
    assert_scalar(draft)
    draft <- if (draft) "always" else "never"
  } else {
    match_value(draft, c("always", "newer", "never"))
  }
  draft
}
