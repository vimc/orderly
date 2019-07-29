##' @title Get the dependencies for a given report from the database
##'
##' @param name the name of the report
##' @param id the id of the report, if omitted, use the id of the latest report
##' @param root
##' @param upstream A boolean indicating if we want to move up or down the tree
##' @param locate
##' @param con A connection to a database
get_dependencies_db <- function(name, id = NULL, root = NULL, upstream = FALSE,
                                locate = TRUE, con = NULL, use_latest = FALSE) {
  ## get a connection to the database
  if (is.null(con)) {
    con <- orderly_db("destination", orderly_config_get(root, locate))
    on.exit(DBI::dbDisconnect(con))
  }

  ## find the latest version of this report in the database
  if (is.null(id)) {
    id <- get_latest_in_db(name = name, con = con)$id
  }
  
  ## now construct the SQL query
  if (upstream) {
    filt_qry <- sprintf("depends.report_version='%s'", id)
  } else {
    filt_qry <- sprintf("report_version.id='%s'", id)
  }
  
  sql_qry <- paste(c("SELECT",
               "depends.report_version, ",
               "report_version.report, report_version.id, ",
               "file_artefact.filename",
               "FROM", "(depends",
               "INNER JOIN", "file_artefact", "ON",
               "depends.use=file_artefact.id",
               "INNER JOIN", "report_version_artefact", "ON",
               "file_artefact.artefact=report_version_artefact.id",
               "INNER JOIN", "report_version", "ON",
               "report_version_artefact.report_version=report_version.id)",
               "WHERE", filt_qry), collapse = " ")

  db_ret <- DBI::dbGetQuery(con, sql_qry)

  if (nrow(db_ret) == 0) {
    return(NULL)
  }

  db_ret$latest <- sapply(db_ret$report_version, is_latest_in_db, con = con)
  if (use_latest) {
    db_ret <- db_ret[which(db_ret$latest), ]
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
  dtst <- DBI::dbGetQuery(con, sql_qry)
  return(dtst)
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
  dtst <- DBI::dbGetQuery(con, paste(sql_qry, collapse = " "))
  
  return(dtst$report)
}

##' @title Is the id the latest version of the report in the database
##'
##' @param id the id of the report
##' @param con A connection to a database
is_latest_in_db <- function(con, id) {
  latest <- get_latest_in_db(con, id_to_name(con, id))
  
  return(latest$id == id)
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
build_tree <- function(name, id = "latest", depth = 0, parent = NULL,
                       tree = NULL, con = NULL, root = NULL, locate = TRUE,
                       upstream = FALSE) {
  if (depth > 10) {
    print("*WARNING* There appears to be a circular dependency")
    return(tree)
  }

  if (is.null(con)) {
    con <- orderly_db("destination", orderly_config_get(root, locate))
    on.exit(DBI::dbDisconnect(con))
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
    v <- Vertex$new(NULL, name, id, (id == latest_id$id))
    tree <- Tree$new(v)
  } else {
    v <- tree$add_child(parent, name, id, (id == latest_id$id))
  }
  
  dep_ids <- get_dependencies_db(name = name, id = id,
                                 root = root, locate = TRUE, con = con,
                                 upstream = upstream)
  for (dep_id in dep_ids) {
    dep_name <- id_to_name(id = dep_id, con = con)
    build_tree(name = dep_name,
               id = dep_id,
               depth = depth + 1,
               parent = v,
               tree = tree,
               con = con,
               root = root,
               locate = locate,
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
print_dep_tree <- function(name, id = "latest", root = NULL, locate = TRUE,
                           upstream = FALSE, con = NULL, propagate = FALSE) {
  if (is.null(con)) {
    con <- orderly_db("destination", orderly_config_get(root, locate))
    on.exit(DBI::dbDisconnect(con))
  }

  dep_tree <- build_tree(name = name, id = id,
                          root = root, locate = TRUE, con = con,
                          upstream = upstream)
  if (upstream) {
    cat(yellow("++++++UPSTREAM++++++\n"))
  } else {
    cat(green("+++++DOWNSTREAM+++++\n"))
  }

  # propagate out-of-date
  if (propagate) {
    propagate(dep_tree$root, upstream)
  }

  if (length(dep_tree) == 0) {
    orderly_log("dep tree", "Nothing to update.")
  }

  dep_tree$print_tree()
}


propagate <- function(vertex, upstream) {
  for (child in vertex$children) {
    if (!upstream) {
      if (!vertex$latest) {
        child$latest = FALSE
      }
    }

    propagate(child, upstream)

    if (upstream) {
      if (!child$latest) {
        vertex$latest = FALSE
      }
    }
  }
}


Vertex <- R6::R6Class("Vertex", list(
  parent = NULL,
  children = list(),
  name = NULL,
  id = NULL,
  latest = NULL,
  initialize = function(parent, name, id, latest) {
    # add some type safety here
    self$parent <- parent
    self$name <- name
    self$id <- id
    self$latest <- latest
  },
  add_child = function(child) {
    self$children <- append(self$children, list(child))
  },
  to_string = function() {
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
  add_child = function(parent, name, id, latest) {
    child <- Vertex$new(parent, name, id, latest)
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
  print_tree = function(vertex = self$root, fvector = c()) {
    console_colour <- if (vertex$latest) {crayon::blue} else {crayon::red}

    if (length(fvector) == 0) {
      cat(console_colour(sprintf("%s\n", vertex$to_string())))
    } else {
      spacing <- paste(ifelse(head(fvector, -1), "| ", "  "), collapse = "")
      cat(console_colour(sprintf("%s|___%s\n", spacing, vertex$to_string())))
    }

    if (length(vertex$children) > 0) {
      for (i in 1:length(vertex$children)) {
        child <- vertex$children[[i]]

        if (i != length(vertex$children)) {
          self$print_tree(child, c(fvector, TRUE))
        } else {
          self$print_tree(child, c(fvector, FALSE))
        }
      }
    }
  }
  )
)