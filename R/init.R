##' Initialise an orderly store.  This is a helper function that
##' automates getting started with using orderly for a new project.
##' It is not required to use - you can create the orderly structure
##' yourself (all that is compulsory is the \code{orderly_config.yml}
##' file).
##'
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
##'
##' @seealso \code{\link{orderly_new}} for creating new reports within
##'   a configured orderly repository.
##'
##' @examples
##' # Initialise a new orderly repository in an temporary directory:
##' path <- orderly::orderly_init(tempfile())
##'
##' # This has created the directory skeleton that you need to get
##' # started using orderly:
##' dir(path)
##'
##' # As instructed, the next thing to do is to edit the
##' # orderly_config.yml file to match your needs:
##' readLines(file.path(path, "orderly_config.yml"))
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
    file_copy(orderly_file("init/readme_src.md"), readme("src"))
    file_copy(orderly_file("init/readme_data.md"), readme("data"))
    file_copy(orderly_file("init/readme_draft.md"), readme("draft"))
    file_copy(orderly_file("init/readme_archive.md"), readme("archive"))
    file_copy(orderly_file("init/readme_root.md"),
              file.path(root, "README.md"))
  }
  file_copy(orderly_file("init/orderly_config.yml"),
            path_orderly_config_yml(root))
  write_orderly_archive_version(cache$current_archive_version, root)
  if (!quiet) {
    message(sprintf("Now, edit the file 'orderly_config.yml' within '%s'",
                    root))
  }

  root
}
