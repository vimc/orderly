migrate <- function(data, path, config) {
  if (is.null(data$meta$elapsed)) {
    ## Very old versions of orderly (pre 0.5.0) do not include elapsed
    ## time, but that does include old reports in montagu.  So we need
    ## to make sure that they report something sensible here.
    data$meta$elapsed <- 0
    changed <- TRUE
  } else {
    changed <- FALSE
  }
  migration_result(changed, data)
}
