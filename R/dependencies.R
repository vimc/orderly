##' @title Get the dependencies for a given report from the orderly_run.yml
##' 
##' @param report the name of the report
##' @param id the id of the report, if omitted, use the id of the latest report
##' @param config_path Path to the orderly configuration
##' @param draft If true look in the draft directory otehrwise look in archive
##'
##' @export
get_dependencies <- function(name, id = NULL, config_path = NULL, draft = FALSE) {
  if (is.null(id))
    id <- orderly:::orderly_latest(name, config = config_path, draft = draft)
  
  if (is.null(config_path))
    config_path <- "."

  if (draft)
    rds_path <- file.path(config_path, "draft", name, id, "orderly_run.rds")
  else
    rds_path <- file.path(config_path, "archive", name, id, "orderly_run.rds")

  if (!file.exists(rds_path)) {
    warning(sprintf(
      "The report %s:%s does not exist or does not have orderly_run.rds",
      name, id))
    return(NULL)
  }
  run_data <- readRDS(rds_path)
  return(run_data$meta$depends)
}

##' @title Generate a list of dependencies for a given report
##'
##' @param report the name of the report
##' @param id the id of the report, if omitted, use the id of the latest report
##' @param config_path Path to the orderly configuration
##' @param draft If true look in the draft directory otehrwise look in archive
##' @param depth only used internally, to prevent infinite recursion and make 
##'              the output log look nice
##' @param dep_reports the list of reports, needs to passed as an argument
##'                          (and returned)
##'
##' @export
what_depends_on <- function(report, id = "latest", config_path = NULL,
                            draft = FALSE, depth = 0,
                            dep_reports = list()) {
  # this is potentially needed to trap circular dependencies
  # TODO - We could be clever here and check if this report already appears in
  # dep_reports
  if (depth > 10) {
    orderly::orderly_log("depends",
                         "*WARNING* There appears to be a circular dependency")
    return(dep_reports)
  }
  
  # we need to go find the latest version of the report
  if (id == "latest")
    id <- orderly_latest(report, config = config_path, draft = draft)
  
  # get a list of all local reports - doing this every time is tedious, but
  # it doesn't take long and looks nicer than passing around a vector of names
  if (draft)
    rep_names <- unique(orderly_list_drafts(config = config_path)$name)
  else
    rep_names <- unique(orderly_list_archive(config = config_path)$name)
  
  # here's the plan - We have a report, A.
  # - We iterate through the list of local reports reading the dependencies from
  # the orderly_run.yml
  # - If for some report (B), A appears in the list of dependencies of B, then
  # B depends on A so this gets added to the list
  # - Then we need to check which reports depend on B
  for (name in rep_names) {
    rep_id <- orderly_latest(name, config = config_path, draft = draft)
    deps <- get_dependencies(name = name, id = rep_id, draft = draft,
                             config_path = config_path)
    if (report %in% deps$name) {
      i <- which(deps$name == report)
      dep_reports[[length(dep_reports) + 1]] <-  list(name = name,
                                                      id = rep_id,
                                                      depth = depth)
      dep_reports <- what_depends_on(report = name, id = rep_id, 
                                     config_path = config_path,
                                     draft = draft, depth = depth + 1,
                                     dep_reports = dep_reports)
    }
  }
  return(dep_reports)
}

##' @title Print the dependency tree for a given report using orderly log
##' 
##' @param report the name of the report
##' @param id the id of the report, if omitted, use the id of the latest report
##' @param config_path Path to the orderly configuration
##' @param draft If true look in the draft directory otehrwise look in archive
##'
##' @export
print_dep_tree <- function(report, id = "latest", draft = FALSE,
                           config_path = NULL) {
  dep_tree <- what_depends_on(report = report, id = id, draft = draft, 
                              config_path = config_path)
  if (length(dep_tree) == 0)
    orderly::orderly_log("dep tree", "Nothing to update.")
  
  for (dep in dep_tree) {
    dep_str <- sprintf("%s- %s [%s]",
                       paste(rep("  ", dep$depth), collapse = ""), 
                       dep$name,
                       dep$id)
    orderly::orderly_log("dep tree", dep_str)
  }
} 