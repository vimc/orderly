##' Initialise an orderly store
##' @title Initialise an orderly store
##'
##' @param root The root of the store; this must be an empty directory
##'   or the path of a directory to create
##'
##' @param doc Logical, indicating if documentation should be added to
##'   the directories.  This also has the (potentially useful) effect
##'   of making these directories noticeable by git.
##'
##' @param quiet Logical, indicating if informational messages should
##'   be suppressed.
##'
##' @export
orderly_init <- function(root, doc = TRUE, quiet = FALSE) {
  if (file.exists(root)) {
    if (!file.info(root)$isdir || length(dir(root)) > 0) {
      stop("'root', if it already exists, must be an empty directory")
    }
  } else {
    dir.create(root, FALSE, TRUE)
  }
  dir_create(path_data(root))
  dir_create(path_src(root))
  dir_create(path_archive(root))
  dir_create(path_draft(root))
  if (doc) {
    readme <- function(path) {
      file.path(root, path, "README.md")
    }
    file_copy(orderly_file("readme_src.md"), readme("src"))
    file_copy(orderly_file("readme_data.md"), readme("data"))
    file_copy(orderly_file("readme_draft.md"), readme("draft"))
    file_copy(orderly_file("readme_archive.md"), readme("archive"))
    file_copy(orderly_file("readme_root.md"),
              file.path(root, "README.md"))
  }
  file_copy(orderly_file("orderly_config_example.yml"),
            path_orderly_config_yml(root))
  writeLines(as.character(utils::packageVersion("orderly")),
             file.path(path_orderly_archive_version(root)))
  if (!quiet) {
    message(sprintf("Now, edit the file 'orderly_config.yml' within '%s'",
                    root))
  }

  root
}
