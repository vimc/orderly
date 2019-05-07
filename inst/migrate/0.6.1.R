migrate <- function(data, path, config) {
  if (!is.null(data$archive_version) && data$archive_version >= "0.6.1") {
    migration_result(FALSE, data)
  }

  updated <- FALSE
  if (file_exists("README.md", workdir = path, check_case = FALSE) &&
      is.null(data$meta$hash_readme)) {
    updated <- TRUE
    data$meta$hash_readme <- withr::with_dir(path, {
      hash_files(dir(pattern = "readme.md", ignore.case = TRUE))
    })
  } else {
    updated <- FALSE
  }

  migration_result(updated, data)
}
