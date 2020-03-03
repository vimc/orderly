##' Add one or more resources to an \code{orderly.yml} file.
##'
##' @title Add a resource to orderly.yml
##'
##' @param resources,sources Character vector of resources or sources
##'   to add.  These must be filenames relative to the report
##'   directory, must exist, and must not already be present in the
##'   orderly.yml
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
##' @rdname orderly_use
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
orderly_use_resource <- function(resources, name = NULL, root = NULL,
                                 locate = FALSE, show = TRUE, edit = TRUE,
                                 prompt = TRUE) {
  loc <- orderly_develop_location(name, root, locate)
  assert_character(resources, name = "Resource")
  assert_file_exists(resources, workdir = loc$path, name = "Resource")
  orderly_use_edit_array(resources, "Resource", "resources",
                         loc, show, edit, prompt)
}


##' @export
##' @rdname orderly_use
orderly_use_source <- function(sources, name = NULL, root = NULL,
                               locate = FALSE, show = TRUE, edit = TRUE,
                               prompt = TRUE) {
  loc <- orderly_develop_location(name, root, locate)
  assert_character(sources, name = "Source")
  assert_file_exists(sources, workdir = loc$path, name = "Source")
  orderly_use_edit_array(sources, "Source", "sources",
                         loc, show, edit, prompt)
}

##' @export
##' @rdname orderly_use
orderly_use_package <- function(packages, name = NULL, root= NULL,
                                locate = FALSE, show = TRUE, edit = TRUE,
                                prompt = TRUE) {
  loc <- orderly_develop_location(name, root, locate)
  orderly_use_edit_array(packages, "Package", "packages",
                         loc, show, edit, prompt)
}


## Basically the same thing for packages, source, resources
orderly_use_edit_array <- function(entries, name_friendly, name_block, loc,
                                   show = TRUE, edit = TRUE, prompt = TRUE) {
  config <- loc$config
  name <- loc$name
  path <- loc$path

  info <- recipe_read(path, config, FALSE)

  err <- intersect(info[[name_block]], entries)
  if (length(err) > 0L) {
    stop(sprintf(
      "%s already declared: %s", name_friendly,
      paste(squote(err), collapse = ", ")),
      call. = FALSE)
  }

  err <- unique(entries[duplicated(entries)])
  if (length(err) > 0L) {
    stop(sprintf(
      "%s duplicated: %s", name_friendly,
      paste(squote(err), collapse = ", ")),
      call. = FALSE)
  }

  path_yml <- file.path("src", name, "orderly.yml")
  yml <- readLines(file.path(config$root, path_yml))

  dat <- yaml_block_info(name_block, yml)

  if (!dat$exists) {
    ## TODO: could add a more extensive help here - ideally syncing up
    ## with the help that exists in the orderly template.
    to_add <-
      c("",
        sprintf("%s:", name_block),
        sprintf("  - %s", entries))
    where <- length(yml)
  } else if (dat$block) {
    to_add <- sprintf("%s- %s", dat$indent, entries)
    where <- dat$end
  } else {
    where <- dat$start
    prev <- yaml_load(yml[[where]])[[name_block]]
    yml[[where]] <- sprintf("%s:", name_block)
    to_add <- sprintf("  - %s", c(prev, entries))
  }

  withr::with_dir(
    config$root,
    insert_into_file(yml, where, to_add, path_yml, show, edit, prompt))
}
