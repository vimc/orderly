migrate <- function(data, path, config) {
  info <- data$meta$file_info_artefacts
  prev <- info$file_hash
  curr <- hash_files(file.path(path, info$filename), FALSE)
  err <- prev != curr
  if (!any(err)) {
    return(migration_result(FALSE, data))
  }

  msg <- c(sprintf("Modified %s in %s/%s",
                   ngettext(sum(err), "artefact", "artefacts"),
                   data$meta$name,
                   data$meta$id),
           paste0("  - ", info$filename[err]))
  orderly_log("WARNING", msg)

  data$meta$file_info_artefacts$file_hash <- curr

  migration_result(TRUE, data)
}
