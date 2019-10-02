migrate <- function(data, path, config) {
  globals <- data$meta$file_info_inputs$filename[
    data$meta$file_info_inputs$file_purpose == "global"]
  if (length(globals) > 0 && is.null(data$meta$global_resources)) {
    ## Before 0.7.15, global resources could not be renamed
    data$meta$global_resources <- set_names(globals, globals)
    changed <- TRUE
  } else {
    changed <- FALSE
  }
  migration_result(changed, data)
}
