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

##' @title Generate a list of dependencies for a given report
##'
##' @param report the name of the report
##' @param id the id of the report, if omitted, use the id of the latest report
##' @param draft If true look in the draft directory otehrwise look in archive
##' @param depth only used internally, to prevent infinite recursion and make 
##'              the output log look nice
##' @param dep_reports the list of reports, needs to passed as an argument
##'                          (and returned)
##' @inheritParams orderly_list                          
##'
##' @export
build_graph <- function(report, id = "latest", root = NULL,
                        depth = 0, dep_reports = list(), locate = TRUE,
                        con = NULL, upstream = FALSE) {
  # this is potentially needed to trap circular dependencies
  # TODO - We could be clever here and check if this report already appears in
  # dep_reports
  if (depth > 10) {
    orderly_log("depends",
                "*WARNING* There appears to be a circular dependency")
    return(dep_reports)
  }

  ##print(paste(rep("*", depth), collapse = ""))

  if (is.null(con)) {
    con <- orderly_db("destination", orderly_config_get(root, locate))
    on.exit(DBI::dbDisconnect(con))
  }

  # we need to go find the latest version of the report
  latest_id <- get_latest_in_db(con, report)
  if (id == "latest") {
    id <- latest_id$id
  }
  # here we should also check if the artifect -> resources have the same hash
  # if they match then we shouldn't flag as out of date

  dep_reports[["name"]] = report
  dep_reports[["id"]] = id
  dep_reports[["latest"]] <- (id == latest_id$id)

  dep_ids <- get_dependencies_db(name = report, id = id,
                                 root = root, locate = TRUE, con = con,
                                 upstream = upstream)

  for (dep_id in dep_ids) {
    dep_name <- id_to_name(id = dep_id, con = con)
    dep_reports[[dep_name]] <- build_graph(report = dep_name,
                                           id = dep_id,
                                           root = root,
                                           depth = depth + 1,
                                           locate = locate,
                                           upstream = upstream)    
  }

  return(dep_reports)
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
  library(crayon)
  if (upstream) {
    cat(yellow("++++++UPSTREAM++++++\n"))
  } else {
    cat(green("+++++DOWNSTREAM+++++\n"))
  }

  if (is.null(con)) {
    con <- orderly_db("destination", orderly_config_get(root, locate))
    on.exit(DBI::dbDisconnect(con))

    remote <- get_remote(remote, orderly_config_get(root, locate))
  }

  dep_tree <- build_graph(report = report, id = id,
                          root = root, locate = TRUE, con = con,
                          upstream = upstream)
  if (length(dep_tree) == 0) {
    orderly_log("dep tree", "Nothing to update.")
  }

  print_tree_r(dep_tree)
} 

print_tree_r <- function(dep_tree, depth = 0) {
  console_colour <- if (dep_tree$latest) {crayon::blue} else {crayon::red}

  if (depth == 0) {
    cat(console_colour(sprintf("%s [%s]\n",
                       dep_tree$name,
                       dep_tree$id)))    
  } else {
    cat(console_colour(sprintf("%s|___%s [%s]\n",
                       strrep(x="| ", times = depth - 1),
                       dep_tree$name,
                       dep_tree$id)))  
  }

  children <- setdiff(names(dep_tree), c("name", "id", "latest"))
  for (child in children) {
    print_tree_r(dep_tree[[child]], depth + 1)
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

