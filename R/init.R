##' Initialise an orderly store.  This is a helper function that
##' automates getting started with using orderly for a new project.
##' It is not required to use - you can create the orderly structure
##' yourself (all that is compulsory is the \code{orderly_config.yml}
##' file).
##'
##' This function creates a minimal orderly structure, containing:
##' \describe{
##'
##' \item{\code{orderly_config.yml}}{The orderly
##' configuration. Minimally, this can be empty, but it must exist.}
##'
##' \item{\code{src}}{The path where report sources live. This should
##' be placed under version control, and contain a number of reports,
##' each in their own directory with an \code{orderly.yml} describing
##' their inputs and outputs (artefacts).  The
##' \code{\link{orderly_new}} function can be used to accelerate
##' creation of new reports.}
##'
##' \item{\code{draft}}{A directory where reports will be run using
##' \code{\link{orderly_run}}.  This directory should be excluded from
##' version control. \code{orderly} will create it as needed if it
##' does not exist when a report is run.}
##'
##' \item{\code{archive}}{A directory where successfully run reports
##' will be moved to after being committed with
##' \code{\link{orderly_commit}}.  This directory should be excluded
##' from version control. \code{orderly} will create it as needed if
##' it does not exist when a report is committed.}
##'
##' \item{\code{data}}{A directory where data extracted from the
##' database (if used) will be stored.  This directory should be
##' excluded from version control. \code{orderly} will create it as
##' needed if it does not exist when a report is run.}
##'
##' }
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
##' fs::dir_tree(path)
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
