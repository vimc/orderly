##' orderly allows a report to rely on the artefacts of one or more other
##' orderly reports. This allows users to develop a network of interconnected
##' reports where the output from report becomes the source of data for another.
##' There are two natural questions that can develop around this workflow:
#'
##' 1. We have updated a report and now need to re-run everything that depends
##'    on it.
##' 2. We have a report that we want to re-run to ensure uses the latest
##'    information. Which other reports are used (directly or indirectly) by
##'    this report?
##'
##' This function display this information in an easily readable format.
##' Allowing users to see the dependency tree and which reports are out of date 
##' and need to be re-run.
##'
##' @section Remark:
##' The tree is built using data from the local report database (see
##' \code{\link{orderly_commit}}). This means that it will not find changes from
##' a report that has not be run and commited. _i.e._ if a user changes a
##' report to use or create different artefacts this will not be picked up by
##' the function until the reports are re-run and commited to the archive.
##'
##' @title Print the dependency tree for a given report using orderly log
##'
##' @param name the name of the report
##' @param id the id of the report, if omitted, use the id of the latest report
##' @param direction A string indicating if we want to move up or down the tree
##'        permitted values are upstream, downstream
##' @param propagate A boolean indicating if we want to propogate out of date
##'                  through the tree
##' @param max_depth A numeric, how far back should the tree go, this can be
##'                  useful to truncate a very large tree. (default = 100)
##' @param show_all A boolean, should we show all reports in the tree, not just
##'                 the latest.
##' @inheritParams orderly_list
##'
##' @return An orderly Tree object with the root corresponding to the given
##'         report.
##' @export
##' @examples
##' path <- orderly::orderly_example("demo")
##'
##' id <- orderly::orderly_run("other", root = path, parameters=list(nmin=0))
##' orderly::orderly_commit(id, root = path)
##' id <- orderly::orderly_run("use_dependency", root = path)
##' orderly::orderly_commit(id, root = path)
##' id <- orderly::orderly_run("use_dependency_2", root = path)
##' orderly::orderly_commit(id, root = path)
##' orderly::orderly_graph("other", root = path)
##' orderly::orderly_graph("use_dependency_2", root = path,
##'                                  direction = "upstream")
##'
orderly_graph <- function(name, id = "latest", root = NULL, locate = TRUE,
                          direction = "downstream", propagate = TRUE,
                          max_depth = 100, show_all = FALSE) {
  assert_scalar_character(direction)
  direction <- match_value(direction, c("upstream", "downstream"))

  assert_scalar_character(name)
  assert_scalar_character(id)
  assert_scalar_logical(locate)
  assert_scalar_logical(propagate)
  assert_scalar_logical(show_all)
  assert_scalar_numeric(max_depth)

  con <- orderly_db("destination", orderly_config_get(root, locate))
  on.exit(DBI::dbDisconnect(con))

  # make sure a report with this name exists
  reports_database <- DBI::dbGetQuery(con, "SELECT name FROM report")
  if (!(name %in% reports_database$name)) {
    stop("This report does not exist")
  }

  dep_tree <- build_tree(name = name, id = id, depth = max_depth, con = con,
                         direction = direction, show_all = show_all)

  # propagate out-of-date
  if (propagate) {
    propagate(dep_tree$root, direction)
  }

  dep_tree
}

##' @title Given a tree return a list of reports to be re-run (and the order
##' that they should be re-run)
##'
##' @param tree A dpendency tree object from orderly_graph_out_of_date
##'
##' @return a list of report names to be re-run. First report to rerun first
##' @export
orderly_graph_out_of_date <- function(tree) {
  types <- class(tree)
  assert_is(tree, "Tree")

  reports <- out_of_date_reports(tree$root)

  reports
}

##' @title Get the dependencies for a given report from the database
##'
##' @param name the name of the report
##' @param id the id of the report, if omitted, use the id of the latest report
##' @param direction A string indicating if we want to move up or down the tree
##'        permitted values are upstream, downstream
##' @param con A connection to a database
##' @noRd
get_dependencies_db <- function(name, id, direction, con, show_all = FALSE) {
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

  query_return$is_latest <- vlapply(query_return$report_version,
                                    is_latest_in_db, con = con)
  if (!show_all) {
    query_return <- query_return[which(query_return$is_latest), ]
  }

  # if we're going uptree
  if (direction == "upstream") {
    unique(query_return$id)
  } else {
    unique(query_return$report_version)
  }
}

##' @title All reports with a given name with most recent towards to top
##'
##' @param name the name of the report
##' @param con A connection to a database
##'
##' @return a dataframe with columns:
##'         id - the report id
##'         report - the name of the report (matches the argument name)
##'         date - the date the report was run
##'
##' @noRd
get_ids_by_name <- function(con, name) {
  sql_query <- paste("SELECT", "id, report, date",
                     "FROM report_version",
                     "WHERE", sprintf("report='%s'", name))

  query_return <- DBI::dbGetQuery(con, sql_query)
  query_return <- query_return[rev(order(query_return$date)),]

  query_return
}

##' @title Get the id of the latest version of a report
##'
##' @param name the name of the report
##' @param con A connection to a database
##'
##' @return a one line dataframe with columns:
##'         id - the report id
##'         report - the name of the report (matches the argument name)
##'         date - the date the report was run
##'
##' @noRd
get_latest_by_name <- function(con, name) {
  reports <- get_ids_by_name(con, name)

  reports$id[1]
}

##' @title Get the id of the second most recent version of a report
##'
##' @param name the name of the report
##' @param con A connection to a database
##'
##' @return a one line dataframe with columns:
##'         id - the report id
##'         report - the name of the report (matches the argument name)
##'         date - the date the report was run
##'
##' @noRd
get_previous_by_name <- function(con, name) {
  reports <- get_ids_by_name(con, name)

  if (nrow(reports) < 2) {
    stop(sprintf("There is only one version of %s", name))
  }

  reports$id[2]
}

##' @title Get the id of the latest version of a report
##'
##' @param id the id of the report which we want the latest version of
##' @param con A connection to a database
##'
##' @return the id of the latest version of the report
##'
##' @noRd
get_latest_by_id <- function(con, id) {
  get_latest_by_name(con, id_to_name(con, id))
}

##' @title Get the name of a report for a given id
##'
##' @param id the id of a report
##' @param con A connection to a database
##'
##' @return The name of the report with given id or NULL if there is no report
##'         with this id
##'
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

  latest == id
}

##' Logic for working out if a report is out of date; assume B depends on A
##' We find the lastest version of A
##' Get the artefact hashes from A
##' Match them against the depends hashes from B
##' We return TRUE (this report is out of date) on the first mismatch. We do not
##' keep track of which file / dependency caused the report to be out of date
##' Otherwise we return FALSE (this report is not out of date)
##'
##' @noRd
is_out_of_date <- function(con, child_id) {
  child_query <- sprintf("depends.report_version='%s'", child_id)

  sql_child_file <- read_lines(orderly_file("database/child_info.sql"))

  sql_child_query <- paste(c(sql_child_file, "WHERE", child_query),
                           collapse = " ")

  ## we return a dataframe with columns:
  ## filename - the name of the artefact B used (not the use name)
  ## file_hash - the hash of the artefact B used
  ## report_version - the id of the report where the artefact came from
  ## is_pinned - was this report pinned to a specific version?
  ## is_latest - if the report was pinned was it to the lastest version
  ## !NOTE! It is unclear what we do when both is_pinned and is_latest are true
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
    if (!child_query_return$is_pinned[i]) {         ## if not pinned...
      latest_id <- get_latest_by_id(con, report_id) ## ..use latest
    } else {                                  ## if pinned to a report..
      if (!child_query_return$is_latest[i]) { ## and that report is not latest..
        latest_id <- report_id                ## then use the pinned id
      } else {                 ## pinned to a report that was the lastest
        latest_id <- report_id ## when the report was run
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

    ## now we match the hashes of the artefacts used by the child report with
    ## the hashes of the latest/pinned versions of the artefact created by the
    ## parents. If there is a mismatch we return TRUE, the report is out-of-date
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
##' @param direction A string indicating if we want to move up or down the tree
##'        permitted values are upstream, downstream
##'
##' @return An R6 tree object
##' @noRd
build_tree <- function(name, id, depth = 100, parent = NULL,
                       tree = NULL, con, direction = "downstream",
                       show_all = FALSE) {
  ## this should never get triggered - it only exists the prevent an infinite
  ## recursion
  if (depth < 0) {
    stop("The tree is very large or degenerate.")
  }

  if (!is.null(parent)) {
    if (check_parents(parent, name)) {
      tree$set_message("There appears to be a circular dependency.")
      return(tree)
    }
  }

  # do we need to find the latest version of the report?
  if (id == "latest") {
    id <- get_latest_by_name(con, name)
  } else if (id == "previous") {
    id <- get_previous_by_name(con, name)
  } else {
    id_database <- id_to_name(con, id)
    if (is.null(id_database)) {
      stop(sprintf("No report with id %s in the database", id))
    } else if (name != id_to_name(con, id)) {
        stop(sprintf("id %s does not match report name %s", id, name))
    }
  }

  ## A remark on `out-of-date-ness`
  ## We only flag a report as out-of-date when it depends on artefacts from
  ## another report and the artefacts it used differ from the artefacts in the
  ## latest version of the other reprot.
  ## In particular the following reports will never be flagged as out-of-date
  ## * A report that uses no artefacts
  ## * A report that only uses deterministic artefacts (i.e. artefacts that
  ##   never change from version to version).
  out_of_date <- is_out_of_date(con, id)

  ## if this is no tree, create a tree...
  if (is.null(tree)) {
    v <- Vertex$new(NULL, name, id, out_of_date)
    tree <- Tree$new(v, direction)
  } else { ## ...otherwise add a vertex
    v <- tree$add_child(parent, name, id, out_of_date)
  }

  dependency_ids <- get_dependencies_db(name = name, id = id, con = con,
                                        direction = direction,
                                        show_all = show_all)
  for (dep_id in dependency_ids) {
    dependency_name <- id_to_name(id = dep_id, con = con)

    build_tree(name = dependency_name, id = dep_id, depth = depth - 1,
               parent = v, tree = tree, con, direction = direction,
               show_all = show_all)
  }

  tree
}

##' @title Given a vertex correpsonding to a report return a list of reports to
##' be re-run by the out of date flag (and the order that they should be re-run)
##'
##' @param vertex The vertex to be checked
##' @param reports A vector of report names to be re-run, this is a built up
##'                recursively as we go down the tree.
##'
##' @return A list of report names
##' @noRd
out_of_date_reports <- function(vertex, reports = c()) {
  if (vertex$out_of_date) {
    if (vertex$name %in% reports) {
      ## we always add reports to the end to get the re-run order right
      reports <- setdiff(reports, vertex$name)
    }
    reports <- c(reports, vertex$name) ## add it to the list
  }

  if (length(vertex$children) > 0) {
    for (vert in vertex$children) {
      reports <- out_of_date_reports(vert, reports)
    }
  }
  reports
}

##' @title Propagate out-of-date statuses down the tree. _i.e._ If a report is
##'        out-of-date make everything that depends on it out-of-date too
##'
##' @param vertex The R6 vertex to be checked
##' @param direction Upstream or downstream produce slightly different behaviour
##'
##' @return Nothing Updates the R6 vertex and child vertices
##' @noRd
propagate <- function(vertex, direction) {
  for (child in vertex$children) {

    if ((direction == "downstream") && (vertex$out_of_date)) {
      child$out_of_date = TRUE
    }

    propagate(child, direction)

    if ((direction == "upstream") && (child$out_of_date)) {
      vertex$out_of_date = TRUE
    }

  }
}
