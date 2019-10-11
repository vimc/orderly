##' @title Get the dependencies for a given report from the database
##'
##' @param name the name of the report
##' @param id the id of the report, if omitted, use the id of the latest report
##' @param upstream A boolean indicating if we want to move up or down the tree
##' @param con A connection to a database
get_dependencies_db <- function(name, id, upstream, con,
                                use_latest = FALSE) {
  ## now construct the SQL query
  if (upstream) {
    filt_qry <- sprintf("depends.report_version='%s'", id)
  } else {
    filt_qry <- sprintf("report_version.id='%s'", id)
  }

  sql_qry <- paste(c("SELECT",
               "depends.report_version, ",
               "report_version.report, report_version.id, ",
               "file_artefact.filename, ", "file_artefact.file_hash",
               "FROM", "(depends",
               "INNER JOIN", "file_artefact", "ON",
               "depends.use=file_artefact.id",
               "INNER JOIN", "report_version_artefact", "ON",
               "file_artefact.artefact=report_version_artefact.id",
               "INNER JOIN", "report_version", "ON",
               "report_version_artefact.report_version=report_version.id)",
               "WHERE", filt_qry), collapse = " ")

  db_ret <- DBI::dbGetQuery(con, sql_qry)

  ## This is only relevant when going upstream
  sql_art_qry <- paste(c("SELECT" , "filename, ", "file_hash",
                         "FROM", "(depends",
                         "INNER JOIN", "report_version", "ON",
                         "depends.report_version=report_version.id",
                         "INNER JOIN", "file_artefact", "ON",
                         "depends.use = file_artefact.id)",
                         "WHERE", filt_qry), collapse = " ")

##  print("                                                                     ")

  db_art_ret <- DBI::dbGetQuery(con, sql_art_qry)

##  print(db_ret)
##  print(db_art_ret)

  if (nrow(db_ret) == 0) {
    return(NULL)
  }

  db_ret$out_of_date <- sapply(db_ret$report_version,
                               is_latest_in_db, con = con)
  if (use_latest) {
    db_ret <- db_ret[which(db_ret$out_of_date), ]
  }

  # if we're going uptree
  if (upstream) {
    return(unique(db_ret$id))
  } else {
    return(unique(db_ret$report_version))
  }
}

##' @title Get the id of the latest version of a report
##'
##' @param name the name of the report
##' @param con A connection to a database
get_latest_in_db <- function(con, name) {
  sql_qry <- paste("SELECT",
                   "id, report, date FROM report_version",
                   "WHERE",
                   sprintf("report='%s'", name),
                   "AND",
                   "date=(SELECT MAX(date)",
                   "FROM",
                   "report_version",
                   sprintf("WHERE report='%s')", name)
  )
  db_ret <- DBI::dbGetQuery(con, sql_qry)
  return(db_ret)
}

##' @title Get the name of a report for a given id
##'
##' @param id the id of the report
##' @param con A connection to a database
id_to_name <- function(con, id) {
  sql_qry <- c("SELECT", "report_version.report",
               "FROM", "report_version",
               "WHERE", sprintf("report_version.id='%s'", id)
  )
  db_ret <- DBI::dbGetQuery(con, paste(sql_qry, collapse = " "))

  return(db_ret$report)
}

##' @title Is the id the latest version of the report in the database
##'
##' @param id the id of the report
##' @param con A connection to a database
is_latest_in_db <- function(con, id) {
  latest <- get_latest_in_db(con, id_to_name(con, id))

  return(latest$id == id)
}

is_out_of_date <- function(con, parent_id, child_id) {
  par_qry <- sprintf("depends.report_version='%s'", parent_id)
  sql_par_qry <- paste(c("SELECT" , "report_version.id, ",
                                    "filename, ",
                                    "file_hash",
                         "FROM", "(depends",
                         "INNER JOIN", "report_version", "ON",
                                       "depends.report_version=report_version.id",
                         "INNER JOIN", "file_artefact", "ON",
                                       "depends.use = file_artefact.id)",
                         "WHERE", par_qry), collapse = " ")

  db_par <- DBI::dbGetQuery(con, sql_par_qry)

  chd_qry <- sprintf("report_version.id='%s'", child_id)
  sql_chd_qry <- paste(c("SELECT", "report_version.id, ",
                                   "file_artefact.filename, ",
                                   "file_artefact.file_hash",
                         "FROM", "(report_version",
                         "INNER JOIN", "report_version_artefact", "ON",
                                       "report_version.id = report_version_artefact.report_version",
                         "INNER JOIN", "file_artefact", "ON",
                                       "file_artefact.artefact = report_version_artefact.id)",
                         "WHERE", chd_qry), collapse = " ")

  db_chd <- DBI::dbGetQuery(con, sql_chd_qry)
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
##' @param upstream A boolean indicating if we want to move up or down the tree
build_tree <- function(name, id, depth = 0, parent = NULL,
                       tree = NULL, con, upstream = FALSE) {
  if (depth > 10) {
    print("*WARNING* There appears to be a circular dependency")
    return(tree)
  }

  # make sure a report with this name exists
  db_reports <- DBI::dbGetQuery(con, "SELECT name FROM report")
  if (!(name %in% db_reports$name)) {
    stop("This report does not exist")
  }

    # we need to go find the latest version of the report
  latest_id <- get_latest_in_db(con, name)
  if (id == "latest") {
    id <- latest_id$id
  } else {
    if (name != id_to_name(con, id)) {
      stop("id does not match report name")
    }
  }

  ## if this is no tree, create a tree
  if (is.null(tree)) {
    v <- Vertex$new(NULL, name, id, (id != latest_id$id))
    tree <- Tree$new(v)
  } else {
    v <- tree$add_child(parent, name, id, (id != latest_id$id))
  }

  dep_ids <- get_dependencies_db(name = name, id = id,
                                 con = con,
                                 upstream = upstream)
  for (dep_id in dep_ids) {
    dep_name <- id_to_name(id = dep_id, con = con)
    build_tree(name = dep_name,
               id = dep_id,
               depth = depth + 1,
               parent = v,
               tree = tree,
               con = con,
               upstream = upstream)
  }

  return(tree)
}

##' @title Print the dependency tree for a given report using orderly log
##'
##' @param name the name of the report
##' @param id the id of the report, if omitted, use the id of the latest report
##' @param con A connection to a database
##' @param root
##' @param locate
##' @param upstream A boolean indicating if we want to move up or down the tree
##' @param propagate A boolean indicating if we want to propogate out of date
##'                  through the tree
##'
##' @export
orderly_build_dep_tree <- function(name, id = "latest", root = NULL,
                                   locate = TRUE, upstream = FALSE, con = NULL,
                                   propagate = FALSE) {
  if (is.null(con)) {
    con <- orderly_db("destination", orderly_config_get(root, locate))
    on.exit(DBI::dbDisconnect(con))
  }

  dep_tree <- build_tree(name = name, id = id, con = con, upstream = upstream)
  if (upstream) {
    message(crayon::yellow("++++++UPSTREAM++++++"))
  } else {
    message(crayon::green("+++++DOWNSTREAM+++++"))
  }

  # propagate out-of-date
  if (propagate) {
    propagate(dep_tree$root, upstream)
  }

  if (length(dep_tree) == 0) {
    orderly_log("dep tree", "Nothing to update.")
  }

  dep_tree
}


propagate <- function(vertex, upstream) {
  for (child in vertex$children) {
    if (!upstream) {
      if (!vertex$out_of_date) {
        child$out_of_date = FALSE
      }
    }

    propagate(child, upstream)

    if (upstream) {
      if (!child$out_of_date) {
        vertex$out_of_date = FALSE
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
    return(sprintf("%s [%s]", self$name, self$id))
  }
  )
)


Tree <- R6::R6Class("Tree", list(
  vertices = NULL,
  root = NULL,
  initialize = function(root) {
    self$vertices <- append(self$vertices, list(root))
    self$root <- root
  },
  add_child = function(parent, name, id, out_of_date) {
    child <- Vertex$new(parent, name, id, out_of_date)
    parent$add_child(child)
    self$vertices <- append(self$vertices, list(child))
    return(child)
  },
  get_vertex = function(name, id) {
    for (vertex in self$vertices) {
      if ((name == vertex$name) && (id == vertex$id)) {
        return(vertex)
      }
    }
  },
  # this is stupidly hacky to get the formatting right
  format_helper = function(vertex = self$root, fvector = c(), str="") {
    console_colour <- if (vertex$out_of_date) {crayon::red} else {crayon::blue}

    if (length(fvector) == 0) {
      str <- paste(str, console_colour(sprintf("%s", vertex$format())), "\n", collapse = "")
    } else {
      spacing <- paste(ifelse(head(fvector, -1), "| ", "  "), collapse = "")
      str <- paste(str, console_colour(sprintf("%s|___%s", spacing, vertex$format())), "\n", collapse = "")
    }

    if (length(vertex$children) > 0) {
      for (i in 1:length(vertex$children)) {
        child <- vertex$children[[i]]

        if (i != length(vertex$children)) {
          str <- self$format_helper(child, c(fvector, TRUE), str)
        } else {
          str <- self$format_helper(child, c(fvector, FALSE), str)
        }
      }
    }
    str
  },
  format = function() {
    self$format_helper()
  }
  )
)
