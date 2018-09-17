get_dependencies <- function(name, id = NULL, config = NULL, draft = FALSE) {
#  print(sprintf("%s, %s", "get_dependencies", config))
  if (is.null(id))
    id <- orderly:::orderly_latest(name, config = config, draft = draft)
  
  if (is.null(config))
    config <- "."

  if (draft)
    rds_path <- file.path(config, "draft", name, id, "orderly_run.rds")
  else
    rds_path <- file.path(config, "archive", name, id, "orderly_run.rds")

  if (!file.exists(rds_path)) {
    print(rds_path)
    warning(sprintf("The report %s:%s does not exist or does not have orderly_run.rds", name, id))
    return(NULL)
  }
  run_data <- readRDS(rds_path)
  return(run_data$meta$depends)
}
##' @title gneerate the dependency tree for a given report
##'
##' @param report the name of the report
##' @param id the id of the report, if omitted, use the id of the latest report
##' @param draft If true look in the draft directory otehrwise look in archive
##' @param depth only used internally, to prevent infinite recursion and make 
##'              the output log look nice
##' @param dependent_reports the list of reports, needs to passed as an argument
##'                          (and returned)
##'
##' @export
what_depends_on <- function(report, id = "latest", config = NULL,
                            draft = FALSE, depth = 0,
                            dependent_reports = list()) {
#  print(sprintf("%s, %s", "what_depends_on", config))
  if (depth > 10) {
    orderly::orderly_log("depends",
                         "*WARNING* There appears to be a circular dependency")
    return(dependent_reports)
  }
  
  if (id == "latest")
    id <- orderly:::orderly_latest(report, config = config, draft = draft)
  
  if (draft)
    rep_names <- unique(orderly::orderly_list_drafts(config = config)$name)
  else
    rep_names <- unique(orderly::orderly_list_archive(config = config)$name)
  
  for (name in rep_names) {
    rep_id <- orderly:::orderly_latest(name, config = config, draft = draft)
    deps <- get_dependencies(name = name, id = rep_id, draft = draft, config = config)
    if (report %in% deps$name) { ## need to check
      i <- which(deps$name == report)
      dependent_reports[[length(dependent_reports) + 1]] <- 
        list(name = name, depth = depth)
      dependent_reports <- what_depends_on(report = name, id = deps$id[i], config = config,
                                           draft = draft, depth = depth + 1,
                                           dependent_reports = dependent_reports)
    }
  }
  return(dependent_reports)
}

##' @title Print the dependency tree with orderly log
##' @param dep_tree a list of dependencies and their depth in the dependcy tree
##'
##' @export
print_dep_tree <- function(report, id = "latest", draft = FALSE,
                           config = NULL) {
#  print(sprintf("%s, %s", "print_dep_tree", config))
  dep_tree <- what_depends_on(report = report, id = id, draft = draft, 
                              config = config)
  if (length(dep_tree) == 0)
    orderly::orderly_log("dep tree", "Nothing to update.")
  
  for (dep in dep_tree) {
    dep_str <- sprintf("%s- %s", paste(rep("  ", dep$depth), collapse = ""), dep$name)
    orderly::orderly_log("depends", dep_str)
  }
} 