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
##' The idea here is that the queries can be used to find ids that
##' match certain criteria for use as dependencies.  This function
##' lets you work out what would be resolved by the query, and using
##' this query string in a `depends:` section will let you select
##' a report that matches some criteria.  For example, suppose that
##' you have report `A` that takes a parameter "fruit" with
##' values like "apple", "banana", and a report `B` that depends
##' on A.  You could then write:
##'
##' ```
##' depends:
##'   A:
##'     id: latest(parameter:fruit == "apple")
##'     uses:
##'       summary.csv: summary.csv
##' ```
##'
##' To get the `summary.csv` file out of the latest report
##' `A` that was run with the "fruit" parameter set to "apple".
##' If "B" itself takes parameters, you can use those parameters in
##' these query expressions like
##'
##' ```
##' depends:
##'   A:
##'     id: latest(parameter:fruit == target_fruit)
##'     uses:
##'       summary.csv: summary.csv
##' ```
##'
##' (assuming that `B` takes a parameter `target_fruit`).
##'
##' The syntax for tags is simpler, one uses `tag:tagname` to
##' test for presence of a tag called "tagname".
##'
##' Search queries can be joined by `&&` and `||` and
##' grouped using parentheses, these groups (or tags) can be negated
##' with `!`, so a complicated query expression might look like:
##'
##' ```
##' (parameter:fruit == "apple" && !tag:weekly) || parameter:fruit == "banana"
##' ```
##'
##' Be careful of comparing floating point numbers with `==` or
##' `!=` as they may not always return what you expect (for example
##' `sqrt(3)^2 == 3` is `FALSE`).
##'
##' In the documentation and error messages we may refer to the
##' left-hand-side of `:` as a "namespace".  At this point the
##' only supported namespaces are `tag` and `parameter`.
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
##'   (`TRUE`, `FALSE`) or use the string `newer` to
##'   use draft reports where they are newer than archive reports.
##'   For consistency, `always` and `never` are equivalent
##'   to `TRUE` and `FALSE`, respectively.
##'
##' @param parameters Named list of parameters (as would be passed to
##'   [orderly1::orderly_run()]) if your query uses parameters on the
##'   right-hand-side of an expression.
##'
##' @param remote A remote to use, if you want to apply the query
##'   remotely.  If this is used then `draft` cannot be set to
##'   `TRUE` as remotes do not expose draft reports.
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
##' root <- orderly1::orderly_example("demo")
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
##' orderly1::orderly_search("parameter:nmin > 0.15", "other", root = root)
##'
##' # Or use "&&" to find tags within a range
##' orderly1::orderly_search("parameter:nmin > 0.1 && parameter:nmin < 0.3",
##'                         "other", root = root)
##'
##' # If a parameter is not present in some versions of a report you
##' # can use is.null to test for it (this is only ever the case if
##' # you have altered a report definition to add or remove a
##' # parameter)
##' orderly1::orderly_search("is.null(parameter:nmin)", "other", root = root)
##'
##' # We can look for tags
##' orderly1::orderly_search("tag:plot", "other", root = root)
##'
##' # or exclude them
##' orderly1::orderly_search("!tag:plot", "other", root = root)
##'
##' # or combine that with the presence/absence of a tag
##' orderly1::orderly_search("parameter:nmin > 0.15 && !tag:plot",
##'                         "other", root = root)
##'
##' # Use latest() over a query to find the latest report matching the
##' # query expression.
##' orderly1::orderly_search("latest(parameter:nmin > 0.15)",
##'                         "other", root = root)
##'
##' # If no reports are found, then a zero-length character vector is returned
##' orderly1::orderly_search("parameter:nmin > 0.4", "other", root = root)
##'
##' # Or, in the case of latest(), NA
##' orderly1::orderly_search("latest(parameter:nmin > 0.4)",
##'                         "other", root = root)
orderly_search <- function(query, name, parameters = NULL, draft = FALSE,
                           root = NULL, locate = TRUE, remote = NULL) {
  config <- orderly_config(root, locate)
  assert_scalar_character(name)
  parameters <- query_check_parameters(parameters)
  draft <- query_check_draft(draft)
  dat <- parse_query(query, parameters)

  if (is.null(remote)) {
    candidates <- local_report_metadata(name, draft, config)
  } else {
    if (draft != "never") {
      stop("Can't use 'draft' along with 'remote'")
    }
    candidates <- remote_report_metadata(name, remote, config)
  }
  matches <- orderly_search_do_search(dat, name, candidates, config)

  if (dat$latest) {
    latest_id(matches)
  } else {
    matches
  }
}


orderly_search_do_search <- function(dat, name, candidates, config) {
  if (dat$use$parameter) {
    parameters <- lapply(candidates$path, function(p)
      readRDS(p)$meta$parameters)
  } else {
    parameters <- NULL
  }
  if (dat$use$tag) {
    tags <- lapply(candidates$path, function(p) readRDS(p)$meta$tags)
  } else {
    tags <- NULL
  }

  search1 <- function(i, expr, base) {
    env <- new.env(parent = base)
    env$parameter <- parameters[[i]]
    env$tag <- tags[[i]]
    eval(expr, env)
  }

  env <- orderly_search_env()
  i <- vlapply(seq_len(nrow(candidates)), search1, dat$expr, env)
  candidates$id[i]
}


orderly_search_env <- function() {
  env <- new.env(parent = emptyenv())
  ## NOTE: would be nicer to get [[ and %in% in here outside the query
  ## language but this works for now.
  for (op in c("(", "is.null", "[[", "%in%", parse_query_join)) {
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


parse_query_operators <- c("is.null", "==", "!=", ">", ">=", "<", "<=")
parse_query_join <- c("(", "&&", "||", "!")


parse_query <- function(x, parameters) {
  expr <- parse(text = x)
  if (length(expr) != 1L) {
    stop("Expected a single expression")
  }
  expr <- expr[[1L]]

  test <- c("parameter", "tag")

  latest <- is_call(expr, "latest") || identical(expr, as.name("latest"))
  if (latest) {
    if (length(expr) == 1L) {
      ## Special exit to make latest() work; this is done better in
      ## outpack.
      return(list(latest = TRUE,
                  use = set_names(as.list(rep(FALSE, length(test))), test),
                  expr = TRUE))
    } else if (length(expr) == 2L) {
      expr <- expr[[2L]]
    } else {
      stop(sprintf(
        "Unexpected query '%s'; expected at most one argument to latest", x))
    }
  }

  dat <- parse_query_expr(expr, parameters)

  use <- set_names(as.list(test %in% dat$namespace), test)
  expr <- dat$expr
  list(latest = latest, use = use, expr = expr)
}


parse_query_expr <- function(expr, parameters) {
  if (is_call(expr, parse_query_join)) {
    fn <- deparse(expr[[1]])

    a <- parse_query_expr(expr[[2]], parameters)
    expr[[2]] <- a$expr

    if (fn %in% c("&&", "||")) {
      b <- parse_query_expr(expr[[3]], parameters)
      expr[[3]] <- b$expr
    } else {
      b <- NULL
    }

    list(namespace = c(a$namespace, b$namespace), expr = expr)
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
    stop(sprintf(
      "Invalid query '%s'; expected some sort of expression",
      deparse_str(expr)),
      call. = FALSE)
  }

  if (is_call(expr, ":")) {
    res <- parse_query_namespace(expr)
    if (res$namespace != "tag") {
      stop(sprintf("Invalid query expression '%s' requires operator",
                   paste(deparse(expr), collapse = "\n")),
           call. = FALSE)
    }
    tag <- res$key
    return(list(namespace = "tag", expr = bquote(.(tag) %in% tag)))
  }

  if (length(expr) < 2L || length(expr) > 3L) {
    stop(sprintf("Invalid query expression '%s'",
                 paste(deparse(expr), collapse = "\n")),
         call. = FALSE)
  }

  namespace <- "parameter"
  rel <- as.character(expr[[1L]])
  if (rel == "is.null") {
    stopifnot(length(expr) == 2L)
    res <- parse_query_namespace(expr[[2L]])
    if (!identical(res$namespace, "parameter")) {
      stop(sprintf(
        "In '%s', query namespace must be 'parameter' but found '%s'",
        deparse_str(expr), res$namespace), call. = FALSE)
    }
    expr[[2L]] <- bquote(.(as.name(res$namespace))[[.(res$key)]])
  } else if (rel %in% parse_query_operators) {
    rewrite <- function(expr, i) {
      x <- parse_query_filter_element(expr[[i]], parameters)
      if (identical(x$namespace, "parameter")) {
        expr[[i]] <- bquote(.(as.name(x$namespace))[[.(x$key)]])
      } else {
        expr[[i]] <- x$key
      }
      expr
    }
    stopifnot(length(expr) == 3L)
    expr <- rewrite(rewrite(expr, 2L), 3L)
  } else {
    stop(sprintf("Query relationship '%s' not allowed", rel))
  }

  list(namespace = namespace, expr = expr)
}


parse_query_filter_element <- function(x, parameters) {
  if (is.atomic(x)) {
    ret <- list(namespace = NULL, key = x)
  } else if (is.symbol(x)) {
    name <- as.character(x)
    value <- parameters[[name]]
    if (is.null(value)) {
      stop(sprintf(
        paste("Query parameter '%s' not found in supplied parameters",
              "did you mean 'parameter:%s'?"),
        name, name), call. = FALSE)
    }
    ret <- list(namespace = NULL, key = value)
  } else if (is_call(x, ":")) {
    ret <- parse_query_namespace(x)
    if (ret$namespace != "parameter") {
      stop(sprintf(
        "In '%s', query namespace must be 'parameter' but found '%s'",
        deparse_str(x), ret$namespace), call. = FALSE)
    }
  } else {
    stop(sprintf(
      paste("Expected symbol, namespaced query element or literal value but",
            "received '%s'"), deparse_str(x)),
      call. = FALSE)
  }
  ret
}


parse_query_namespace <- function(expr) {
  if (!is_call(expr, ":")) {
    stop(sprintf(
      "Expected namespaced query element but received '%s'",
      deparse_str(expr)), call. = FALSE)
  }

  ns <- expr[[2L]]
  key <- expr[[3L]]
  if (!is.symbol(ns)) {
    stop(sprintf(
      "Invalid namespaced query element '%s'; expected symbol for namespace",
      deparse(expr)), call. = FALSE)
  }
  ns <- as.character(ns)

  if (!is.symbol(key)) {
    stop(sprintf(
      "Invalid namespaced query element '%s'; expected symbol for key",
      deparse(expr)), call. = FALSE)
  }
  key <- as.character(key)

  match_value(ns, c("tag", "parameter"),
              sprintf("Query namespace (used as '%s')", ns))

  list(namespace = ns, key = key)
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


query_check_draft <- function(draft) {
  if (is.logical(draft)) {
    assert_scalar(draft)
    draft <- if (draft) "always" else "never"
  } else {
    match_value(draft, c("always", "newer", "never"))
  }
  draft
}


local_report_metadata <- function(name, draft, config) {
  if (draft == "never") {
    candidates_draft <- NULL
  } else {
    p_draft <- file.path(path_draft(config$root), name)
    ids <- orderly_list_dir(p_draft, TRUE)
    candidates_draft <- data_frame(
      id = ids,
      type = rep("draft", length(ids)),
      path = path_orderly_run_rds(file.path(p_draft, ids)))
  }

  if (draft == "always") {
    candidates_archive <- NULL
  } else {
    p_archive <- file.path(path_archive(config$root), name)
    ids <- orderly_list_dir(p_archive, TRUE)
    candidates_archive <- data_frame(
      id = ids,
      type = rep("archive", length(ids)),
      path = path_orderly_run_rds(file.path(p_archive, ids)))
  }

  rbind(candidates_draft, candidates_archive)
}


remote_report_metadata <- function(name, remote, config) {
  remote <- get_remote(remote, config)
  candidates <- remote_report_update_metadata(name, remote, config)
  candidates$type <- "remote"
  candidates[c("id", "type", "path")]
}
