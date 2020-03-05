##' Add one or more resources to an \code{orderly.yml} file.
##'
##' The \code{orderly_use_gitignore} configures a basic
##' \code{.gitignore} file at the root of your orderly project that
##' will prevent files from being added to git.  This is only really
##' useful if you are using (or will use) git, but it is harmless at
##' worst.
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
                                 locate = TRUE, show = TRUE, edit = TRUE,
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
                               locate = TRUE, show = TRUE, edit = TRUE,
                               prompt = TRUE) {
  loc <- orderly_develop_location(name, root, locate)
  assert_character(sources, name = "Source")
  assert_file_exists(sources, workdir = loc$path, name = "Source")
  orderly_use_edit_array(sources, "Source", "sources",
                         loc, show, edit, prompt)
}

##' @param packages Character vector of package names to add.  These
##'   must not already exist in the orderly.yml
##'
##' @export
##' @rdname orderly_use
orderly_use_package <- function(packages, name = NULL, root= NULL,
                                locate = TRUE, show = TRUE, edit = TRUE,
                                prompt = TRUE) {
  loc <- orderly_develop_location(name, root, locate)
  orderly_use_edit_array(packages, "Package", "packages",
                         loc, show, edit, prompt)
}


##' @export
##' @rdname orderly_use
orderly_use_gitignore <- function(root = NULL, locate = TRUE,
                                  show = TRUE, edit = TRUE, prompt = TRUE) {
  config <- orderly_config_get(root, locate)
  root <- config$root

  ## Do we check that the project here already has git?  We can do
  ## that with runner_has_git(), which looks for both the git
  ## executable and the .git directory in the root.  But of course the
  ## .git directory could be above this point.
  included <- readLines(orderly_file("init/gitignore"))
  dest <- file.path(root, ".gitignore")

  ## There are two options for adding the lines here: we could look
  ## to see what is _already_ included and filter on that, or we can
  ## ask git what it is going to ignore and add.  We can use
  ##
  ## git check-ignore --non-matching --no-index --verbose
  ##
  ## to test that, which could be nicer than using a basic match
  ##
  ## But for now, let's do the simplest thing (which has the advantage
  ## of not requiring any system calls)
  if (file.exists(dest)) {
    prev <- readLines(dest)
    to_add <- setdiff(included[grepl("^[^#[:space:]]", included)], prev)
    where <- length(prev)
  } else {
    prev <- NULL
    where <- NA
    to_add <- included
  }

  withr::with_dir(
    config$root,
    insert_into_file(prev, where, to_add, dest, show, edit, prompt))
}


## Basically the same thing for packages, source, resources
orderly_use_edit_array <- function(entries, name_friendly, name_block, loc,
                                   show = TRUE, edit = TRUE, prompt = TRUE) {
  config <- loc$config
  name <- loc$name
  path <- loc$path

  info <- recipe_read(path, config, FALSE, develop = TRUE)

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


orderly_use_dependency <- function(parent, filename, as = NULL,
                                   version = "latest", name = NULL,
                                   root = NULL, locate = TRUE, show = TRUE,
                                   edit = TRUE, prompt = TRUE) {
  loc <- orderly_develop_location(name, root, locate)

  assert_report_id(version)

  assert_character(filename)
  if (is.null(as)) {
    as <- filename
  } else {
    assert_character(as)
    assert_length(as, length(filename))
  }
  assert_unique(as)

  info_parent <- recipe_read(file.path(loc$config$root, "src", parent),
                             loc$config, validate = FALSE)
  pos <- unlist(info_parent$artefacts[, "filenames"], FALSE, FALSE)
  msg <- setdiff(filename, pos)
  if (length(msg) > 0L) {
    stop(sprintf(
      "Requested file not found in %s: %s\nValid options: %s",
      parent,
      paste(squote(msg, collapse = ", ")),
      paste(squote(pos, collapse = ", "))))
  }

  ## TODO: check to see if we have already imported this dependency,
  ## in that case we *really should* add it to that list, but that's
  ## another issue for later.

  ## TODO: deal with marking deletions better, don't just remove them
  ## like below.  This requires a bit of a tweak to the filediff
  ## object.

  path_yml <- file.path("src", name, "orderly.yml")
  yml <- readLines(file.path(loc$config$root, path_yml))
  dat <- yaml_block_info("depends", yml)

  to_add <- c(
    "depends:",
    sprintf("  %s:", parent),
    sprintf("    id: %s", version),
    sprintf("    use:"),
    sprintf("      %s: %s", as, filename))

  if (!dat$exists) {
    where <- length(yml)
    ## TODO: find this line elsewhere in the file?  Or search for it
    ## as an alternative place.
    ##
    ## This is added by the orderly template, so let's get rid of it.
    if (where > 0 && yml[[where]] == "# depends:") {
      yml <- yml[-where]
      where <- length(yml)
    }
  } else if (dat$block) {
    to_add <- to_add[-1L]
    if (dat$indent != "  ") {
      ## Reformat our additions to match the existing indent:
      n <- nchar(sub("[^ ].*$", "", to_add)) / 2
      to_add <- paste0(strrep(dat$indent, n), sub("^ *", "", to_add))
    }
    where <- dat$end
  } else {
    prev <- yaml_load(yml[[where]])[[name_block]]
    ## TODO: confirm that this is really empty
    browser()
    where <- dat$start
    yml[[where]] <- to_add[[1L]]
    to_add <- to_add[-1L]
  }

  withr::with_dir(
    loc$config$root,
    insert_into_file(yml, where, to_add, path_yml, show, edit, prompt))
}
