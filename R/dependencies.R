##' @title Get the dependencies for a given report from the database
##'
##' @param name the name of the report
##' @param id the id of the report, if omitted, use the id of the latest report
##' @param direction A string indicating if we want to move up or down the tree
##'        permitted values are upstream, downstream
##' @param con A connection to a database
##' @noRd
get_dependencies_db <- function(name, id, direction, con, list_all = FALSE) {
  ## now construct the SQL query
  if (direction == "upstream") {
    filter_query <- sprintf("depends.report_version='%s'", id)
  } else {
    filter_query <- sprintf("report_version.id='%s'", id)
  }

  sql_query_file <- read_lines(orderly_file("database/dependency_info.sql"))

  sql_query <- paste(c(sql_query_file, "WHERE", filter_query), collapse = " ")

  query_return <- DBI::dbGetQuery(con, sql_query)

  if (nrow(query_return) == 0) {
    return(NULL)
  }

  query_return$is_latest <- vapply(query_return$report_version,
                             is_latest_in_db, logical(1), con = con)
  if (!list_all) {
    query_return <- query_return[which(query_return$is_latest), ]
  }

  # if we're going uptree
  if (direction == "upstream") {
    unique(query_return$id)
  } else {
    unique(query_return$report_version)
  }
}

##' @title Get the id of the latest version of a report
##'
##' @param name the name of the report
##' @param con A connection to a database
##' @noRd
get_latest_by_name <- function(con, name) {
  sql_query <- paste("SELECT", "id, report, date", 
                     "FROM report_version",
                     "WHERE", sprintf("report='%s'", name),
                     "AND", "date=(SELECT MAX(date)",
                     "FROM", "report_version",
                     sprintf("WHERE report='%s')", name))

  DBI::dbGetQuery(con, sql_query)
}

get_latest_by_id <- function(con, id) {
  get_latest_by_name(con, id_to_name(con, id))$id
}

##' @title Get the name of a report for a given id
##'
##' @param id the id of the report
##' @param con A connection to a database
##' @noRd
id_to_name <- function(con, id) {
  sql_query <- c("SELECT", "report_version.report",
                 "FROM", "report_version",
                 "WHERE", sprintf("report_version.id='%s'", id))
  query_return <- DBI::dbGetQuery(con, paste(sql_query, collapse = " "))

  if (nrow(query_return) == 0) {
    NULL
  } else {
    query_return$report
  }
}

##' @title Is the id the latest version of the report in the database
##'
##' @param id the id of the report
##' @param con A connection to a database
##'
##' @return A boolean TRUE if the report is the latest version
##' @noRd
is_latest_in_db <- function(con, id) {
  latest <- get_latest_by_name(con, id_to_name(con, id))

  latest$id == id
}

##' Logic for working out if a report is out of date; assume B depends on A
##' Then we find the lastest version of A
##' get the artefact hashes from A
##' match them against the depends hashes from B
##' @noRd
is_out_of_date <- function(con, child_id) {
  ## filename - the name of the artefact B used (not the use name)
  ## file_hash - the hash of the artefact B used
  ## report_version - the id of the report where the artefact came from
  ## is_pinned - was this report pinned to a speicif version?
  ## is_latest - if the report was pinned was it to the lastest version
  ## !NOTE! It is unclear what we do when both is_pinned and is_latest are true
  child_query <- sprintf("depends.report_version='%s'", child_id)

  sql_child_file <- read_lines(orderly_file("database/child_info.sql"))

  sql_child_query <- paste(c(sql_child_file, "WHERE", child_query),
                           collapse = " ")

  child_query_return <- DBI::dbGetQuery(con, sql_child_query)

  ## this reports uses no artefacts - so can never be out of date
  if (nrow(child_query_return) == 0) {
    return(FALSE)
  }

  ## we iterate over rows of the data frame and make sure the artefact match
  for (i in 1:nrow(child_query_return)) {
    filename <- child_query_return$filename[i]
    file_hash <- child_query_return$file_hash[i]
    report_id <- child_query_return$report_version[i]
    ## we need to find the id latest version of the report with id = report_id
    if (!child_query_return$is_pinned[i]) { ## if not pinned...
      latest_id <- get_latest_by_id(con, report_id) ## ..use latest
    } else { ## if pinned to..
      if (!child_query_return$is_latest[i]) { ## ..not latest..
        latest_id <- report_id ## ..use the pinned id
      } else { ## pinned to latest
        latest_id <- report_id
      }
    }


    ## we return a dataframe with columns:
    ## filename - the filename of the artefact
    ## file_hash - the hash of the artefact
    parent_query <- sprintf("report_version.id='%s'", latest_id)

    sql_parent_file <- read_lines(orderly_file("database/parent_info.sql"))

    sql_parent_query <- paste(c(sql_parent_file, "WHERE", parent_query),
                             collapse = " ")

    parent_query_return <- DBI::dbGetQuery(con, sql_parent_query)
    i <- which(parent_query_return$filename == filename)
    if (parent_query_return$file_hash[i] != file_hash)
      return(TRUE)
  }

  FALSE
}

##' @title Recursively check that none of the parents share the same name as the
##' current report.
##'
##' @param parent_vertex The vertex whose name we want to check
##' @param name The name we want to match
##'
##' @return A boolean TRUE if there is a match (bad!) FALSE if the is no match
##' (good!)
##' @noRd
check_parents <- function(parent_vertex, name) {
  if (parent_vertex$name == name) {
    return(TRUE)
  } else {
    if (!is.null(parent_vertex$parent)) {
      return(check_parents(parent_vertex$parent, name))
    }
  }

  FALSE
}

##' @title Recursively builds a tree for a given report
##'
##' @param name the name of the report
##' @param id the id of the report, if omitted, use the id of the latest report
##' @param depth [internal] - only used ensure we don't get trapped in an
##'              infinite loop
##' @param parent [internal] - the previous vertex in the tree
##' @param tree [internal] - The tree object that is built up and returned at
##'             the end
##' @param con A connection to a database
##' @param root
##' @param locate
##' @param direction A string indicating if we want to move up or down the tree
##'        permitted values are upstream, downstream
##'
##' @return An R6 tree object
##' @noRd
build_tree <- function(name, id, depth = 100, parent = NULL,
                       tree = NULL, con, direction = "downstream",
                       list_all = FALSE) {
  ## this should never get triggered - it only exists the prevent an infinite
  ## recursion
  if (depth < 0) {
    stop("The tree is very large or degenerate.")
  }

  if (!is.null(parent)) {
    if (check_parents(parent, name)) {
      tree$message <- "There appears to be a circular dependency."
      return(tree)
    }
  }

  # make sure a report with this name exists
  reports_database <- DBI::dbGetQuery(con, "SELECT name FROM report")
  if (!(name %in% reports_database$name)) {
    stop("This report does not exist")
  }

  # do we need to find the latest version of the report?
  if (id == "latest") {
    id <- get_latest_by_name(con, name)$id
  } else {
    id_database <- id_to_name(con, id)
    if (is.null(id_database)) {
      stop(sprintf("No report with id %s in the database", id))
    } else {
      if (name != id_to_name(con, id)) {
        stop(sprintf("id %s does not match report name %s", id, name))
      }
    }
  }

  if (direction == "upstream") {
    ## when going upstream the concept of out-of-date doesn't make sense?
    out_of_date <- is_out_of_date(con, id)
  } else {
    out_of_date <- is_out_of_date(con, id)
  }

  ## if this is no tree, create a tree...
  if (is.null(tree)) {
    v <- Vertex$new(NULL, name, id, out_of_date)
    tree <- Tree$new(v)
  } else { ## ...otherwise add a vertex
    v <- tree$add_child(parent, name, id, out_of_date)
  }

  dependency_ids <- get_dependencies_db(name = name, id = id, con = con,
                                        direction = direction,
                                        list_all = list_all)
  for (dep_id in dependency_ids) {
    dependency_name <- id_to_name(id = dep_id, con = con)
    build_tree(name = dependency_name, id = dep_id, depth = depth - 1,
               parent = v, tree = tree, con = con, direction = direction,
               list_all = list_all)
  }

  tree
}

##' @title Given a tree return a list of reports to be re-run (and the order
##' that they should be re-run)
##'
##' @tree A vertex object
##'
##' @return A list of report names
##' @noRd
out_ot_date_reports <- function(vertex, reports = c()) {
  if (vertex$out_of_date) {
    if (vertex$name %in% reports) {
      reports <- setdiff(reports, vertex$name)
    }
    reports <- c(reports, vertex$name) ## add it to the list
  }

  if (length(vertex$children) > 0) {
    for (vert in vertex$children) {
      reports <- out_ot_date_reports(vert, reports)
    }
  }

  reports
}

##' @title Print the dependency tree for a given report using orderly log
##'
##' @param name the name of the report
##' @param id the id of the report, if omitted, use the id of the latest report
##' @param con A connection to a database
##' @param root
##' @param locate
##' @param direction A string indicating if we want to move up or down the tree
##'        permitted values are upstream, downstream
##' @param propagate A boolean indicating if we want to propogate out of date
##'                  through the tree
##'
##' @export
orderly_build_dep_tree <- function(name, id = "latest", root = NULL,
                                   locate = TRUE, direction = "downstream",
                                   con = NULL, propagate = FALSE,
                                   max_depth = 100, list_all = FALSE) {
  assert_scalar_character(direction)
  direction <- match_value(direction, c("upstream", "downstream"))

  assert_scalar_character(name)
  assert_scalar_character(id)
  assert_scalar_logical(locate)
  assert_scalar_logical(propagate)
  assert_scalar_logical(list_all)
  assert_scalar_numeric(max_depth)

  if (is.null(con)) {
    con <- orderly_db("destination", orderly_config_get(root, locate))
    on.exit(DBI::dbDisconnect(con))
  }

  dep_tree <- build_tree(name = name, id = id, depth = max_depth, con = con,
                         direction = direction, list_all = list_all)

  # propagate out-of-date
  if (propagate) {
    propagate(dep_tree$root, direction)
  }

  dep_tree
}

propagate <- function(vertex, direction) {
  for (child in vertex$children) {
    if (direction == "downstream") {
      if (vertex$out_of_date) {
        child$out_of_date = TRUE
      }
    }

    propagate(child, direction)

    if (direction == "upstream") {
      if (child$out_of_date) {
        vertex$out_of_date = TRUE
      }
    }
  }
}

Vertex <- R6::R6Class("Vertex", list(
  parent = NULL,
  children = list(),
  name = NULL,
  id = NULL,
  out_of_date = NULL,
  initialize = function(parent, name, id, out_of_date) {
    # add some type safety here
    self$parent <- parent
    self$name <- name
    self$id <- id
    self$out_of_date <- out_of_date
  },
  add_child = function(child) {
    self$children <- append(self$children, list(child))
  },
  format = function() {
    sprintf("%s [%s]", self$name, self$id)
  }
  )
)


Tree <- R6::R6Class("Tree", list(
  vertices = NULL,
  root = NULL,
  message = NULL,
  initialize = function(root) {
    self$vertices <- append(self$vertices, list(root))
    self$root <- root
    self$message <- NULL
  },
  add_child = function(parent, name, id, out_of_date) {
    child <- Vertex$new(parent, name, id, out_of_date)
    parent$add_child(child)
    self$vertices <- append(self$vertices, list(child))
    child
  },
  # this is stupidly hacky to get the formatting right
  format_helper = function(vertex = self$root, fvector = c(), str="") {
    ## if the tree has a warning message, append it to the front in the red
    if (!is.null(self$message)) {
      str <- paste(str, crayon::red(self$message), "\n", collapse = "")
      self$message <- NULL
    }

    console_colour <- if (vertex$out_of_date) {crayon::red} else {crayon::blue}

    if (length(fvector) == 0) {
      str <- paste(str, console_colour(sprintf("%s", vertex$format())), "\n", collapse = "")
    } else {
      spacing <- paste(vapply(head(fvector, -1),
                              function(x) { if (x) {"| "} else {"  "}},
                              character(1)),
                       collapse = "")


      str <- paste(str, console_colour(sprintf("%s|___%s", spacing, vertex$format())), "\n", collapse = "")
    }

    number_children <- length(vertex$children)
    if (number_children > 0) {
      i <- 1 ## we need to keep track of when we are at the end of the vector
      for (child in vertex$children) {
        is_last <- (i != number_children)
        str <- self$format_helper(child, c(fvector, is_last), str)
        i <- i + 1
      }
    }
    str
  },
  format = function() {
    self$format_helper()
  }
  )
)
