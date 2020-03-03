##' Add one or more resources to an \code{orderly.yml} file.
##'
##' @title Add a resource to orderly.yml
##'
##' @param filenames Character vector of resources or sources to add.
##'   These must be filenames relative to the report directory, must
##'   exist, and must not already be present in the orderly.yml
##'
##' @param name Name of the report to modify.  Like
##'   \code{\link{orderly_develop_start}} this can be \code{NULL} if
##'   you have already set the working directory to be the source
##'   directory.
##'
##' @inheritParams orderly_run
##'
##' @param show Logical, indicating if we should print the proposed
##'   changes to screen
##'
##' @param edit Logical, indicating if we should actually edit the
##'   \code{orderly.yml} file.
##'
##' @param prompt Logical, indicating if we should prompt before
##'   editing the orderly.yml file.  Only has an effect if \code{edit}
##'   is \code{TRUE}.
##'
##' @return Invisibly, this function returns information about the
##'   file it would edit.  This information is primarily for debugging
##'   purposes and the format is subject to change.
##'
##' @export
##' @examples
##' path <- orderly::orderly_example("minimal")
##'
##' # Suppose we wanted to use the mtcars data within our report.
##' # First, the file must exist:
##' write.csv(mtcars, file.path(path, "src", "example", "mtcars.csv"),
##'           row.names = FALSE)
##'
##' # Preview expected changes
##' orderly::orderly_use_resource("mtcars.csv", "example", path, edit = FALSE)
##'
##' # Modify the orderly.yml file within src/example:
##' orderly::orderly_use_resource("mtcars.csv", "example", path, prompt = FALSE)
##'
##' # The result is a file that now has a 'resources' section
##' # containing our new file
##' writeLines(readLines(file.path(path, "src", "example", "orderly.yml")))
##'
##' # (of course, we'd still need to modify the script to use it).
orderly_use_resource <- function(filenames, name = NULL, root = NULL,
                                 locate = FALSE, show = TRUE, edit = TRUE,
                                 prompt = TRUE) {
  orderly_use_edit_array(filenames, "Resource", "resources",
                         name, root, locate, show, edit, prompt)
}


##' @export
##' @rdname orderly_use_resource
orderly_use_source <- function(filenames, name = NULL, root = NULL,
                               locate = FALSE, show = TRUE, edit = TRUE,
                               prompt = TRUE) {
  orderly_use_edit_array(filenames, "Source", "sources",
                         name, root, locate, show, edit, prompt)
}


## Basically the same thing for packages, source, resources
orderly_use_edit_array <- function(files, name_friendly, name_block,
                                   name = NULL, root = NULL,
                                   locate = FALSE, show = TRUE, edit = TRUE,
                                   prompt = TRUE) {
  loc <- orderly_develop_location(name, root, locate)
  config <- loc$config
  name <- loc$name
  path <- loc$path

  info <- recipe_read(path, config, FALSE)

  assert_character(files, name = name_friendly)
  assert_file_exists(files, workdir = path, name = name_friendly)

  err <- intersect(info[[name_block]], files)
  if (length(err) > 0L) {
    stop(sprintf(
      "%s already declared: %s", name_friendly,
      paste(squote(err), collapse = ", ")),
      call. = FALSE)
  }

  err <- unique(files[duplicated(files)])
  if (length(err) > 0L) {
    stop(sprintf(
      "%s duplicated: %s", name_friendly,
      paste(squote(err), collapse = ", ")),
      call. = FALSE)
  }

  path_yml <- file.path("src", name, "orderly.yml")
  yml <- readLines(file.path(root, path_yml))

  dat <- yaml_block_info(name_block, yml)

  if (!dat$exists) {
    ## TODO: could add a more extensive help here - ideally syncing up
    ## with the help that exists in the orderly template.
    to_add <-
      c("",
        sprintf("%s:", name_block),
        sprintf("  - %s", files))
    where <- length(yml)
  } else if (dat$block) {
    to_add <- sprintf("%s- %s", dat$indent, files)
    where <- dat$end
  } else {
    where <- dat$start
    prev <- yaml_load(yml[[where]])[[name_block]]
    yml[[where]] <- sprintf("%s:", name_block)
    to_add <- sprintf("  - %s", c(prev, files))
  }

  withr::with_dir(
    config$root,
    insert_into_file(yml, where, to_add, path_yml, show, edit, prompt))
}
