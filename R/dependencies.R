##' @title Get the dependencies for a given report from the orderly_run.yml
##' 
##' @param report the name of the report
##' @param id the id of the report, if omitted, use the id of the latest report
##' @param draft If true look in the draft directory otehrwise look in archive
##' @inheritParams orderly_list 
##'
##' @export
get_dependencies_db <- function(name, id = NULL, root = NULL, upstream = FALSE,
                                locate = TRUE, con = NULL) {
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

  dtst <- DBI::dbGetQuery(con, sql_qry)

  if (nrow(dtst) == 0) {
    return(NULL)
  }

  dtst$latest <- sapply(dtst$report_version, is_latest_in_db, con = con)
  if (TRUE) {
    dtst <- dtst[which(dtst$latest), ]
  }
  
  # if we're going uptree
  if (upstream) {
    return(unique(dtst$id))
  } else {
    return(unique(dtst$report_version))
  }
}

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

id_to_name <- function(con, id) {
  sql_qry <- c("SELECT", "report_version.report",
               "FROM", "report_version",
               "WHERE", sprintf("report_version.id='%s'", id)
  )
  dtst <- DBI::dbGetQuery(con, paste(sql_qry, collapse = " "))
  
  return(dtst$report)
}

is_latest_in_db <- function(con, id) {
  name <- id_to_name(con, id)
  latest <- get_latest_in_db(con, name)
  
  return(latest$id == id)
}

build_tree <- function(report, id = "latest", depth = 0, parent = NULL,
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
  latest_id <- get_latest_in_db(con, report)
  if (id == "latest") {
    id <- latest_id$id
  }
  
  ## if this is no tree, create a tree
  if (is.null(tree)) {
    v <- Vertex$new(NULL, report, id, (id == latest_id$id))
    tree <- Tree$new(v)
  } else {
    v <- tree$add_child(parent, report, id, (id == latest_id$id))
  }
  
  ##children <- getchildren(report)

  dep_ids <- get_dependencies_db(name = report, id = id,
                                 root = root, locate = TRUE, con = con,
                                 upstream = upstream)
  for (dep_id in dep_ids) {
    dep_name <- id_to_name(id = dep_id, con = con)
    build_tree(report = dep_name,
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
##' @param report the name of the report
##' @param id the id of the report, if omitted, use the id of the latest report
##' @param draft If true look in the draft directory otehrwise look in archive
##' @inheritParams orderly_list 
##'
##' @export
print_dep_tree <- function(report, id = "latest", draft = FALSE,
                           root = NULL, locate = TRUE, remote = NULL,
                           upstream = FALSE, con = NULL) {
  if (is.null(con)) {
    con <- orderly_db("destination", orderly_config_get(root, locate))
    on.exit(DBI::dbDisconnect(con))

    #remote <- get_remote(remote, orderly_config_get(root, locate))
  }

  dep_tree <- build_tree(report = report, id = id,
                          root = root, locate = TRUE, con = con,
                          upstream = upstream)

  library(crayon)
  if (upstream) {
    cat(yellow("++++++UPSTREAM++++++\n"))
  } else {
    cat(green("+++++DOWNSTREAM+++++\n"))
  }

  # propegate out-of-date
  #dep_tree <- propegate(dep_tree, )

  if (length(dep_tree) == 0) {
    orderly_log("dep tree", "Nothing to update.")
  }

  dep_tree$print_tree()
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
    # add root to tree
    self$vertices <- append(self$vertices, list(root))
    # remember which is the root
    self$root <- root
  },
  add_child = function(parent, name, id, latest) {
    # create a new vertex
    c <- Vertex$new(parent, name, id, latest)
    # assign the child to the parent
    parent$add_child(c)
    # and add it to the tree
    self$vertices <- append(self$vertices, list(c))
    return(c)
  },
  get_vertex = function(name, id) {
    for (v in self$vertices) {
      if ((name == v$name) && (id == v$id))
        return(v)
    }
  },
  print_tree = function(v = self$root, depth = 0) {
    console_colour <- if (v$latest) {crayon::blue} else {crayon::red}

    if (depth == 0) {
      cat(console_colour(sprintf("%s\n",v$to_string())))  
    } else {
      cat(console_colour(sprintf("%s|___%s\n",
                         strrep(x="| ", times = depth - 1),
                         v$to_string())))
    }
    for (c in v$children) {
      self$print_tree(c, depth + 1)
    }
  }
  )
)