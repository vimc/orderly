##' For interactive testing of orderly code.  This runs through and
##' sets everything up as orderly would (creates a new working
##' directory and copies files into it, pulls data from the database,
##' copies over any dependent reports) but then rather than running
##' the report hands back to the user.
##'
##' Previous versions of orderly changed into the created directory
##' when using `orderly1::orderly_test_start`, which allowed
##' interactive testing of a report, including ensuring that it has
##' created all expected outputs.  However, CRAN rules do not allow
##' changing the working directory, which significantly reduces the
##' usefulness of this function - as such we may remove it entirely in
##' a future version of orderly if it does not prove useful in this
##' more limited form.
##'
##' The new suggested workflow is:
##'
##' 1. run `orderly_test_start(...)` to prepare a report directory
##' 2. manually change into that directory following the printed
##'   instructions
##' 3. use `orderly_test_check` to check that your report has created
##'       the expected artefacts
##' 4. manually change back to your original directory
##'
##' @title Prepare a directory for orderly to use
##'
##' @param name Name of the report to run (see
##'   [orderly1::orderly_list()]).  A leading `src/` will be
##'   removed if provided, allowing easier use of autocomplete.
##'
##' @inheritParams orderly_run
##' @return The path to the report directory
##' @export
##' @examples
##'
##' path <- orderly1::orderly_example("minimal")
##' p <- orderly1::orderly_test_start("example", root = path)
##'
##' # The data in the orderly example is now available to use
##' dat
##'
##' # Check to see which artefacts have been created so far:
##' orderly1::orderly_test_check(p)
##'
##' # Manually the code that this report has in its script
##' png(file.path(p, "mygraph.png"))
##' barplot(setNames(dat$number, dat$name), las = 2)
##' dev.off()
##'
##' # We now confirm that the artefact has been created:
##' orderly1::orderly_test_check(p)
orderly_test_start <- function(name, parameters = NULL, envir = parent.frame(),
                               root = NULL, locate = TRUE, instance = NULL,
                               use_draft = FALSE, remote = NULL) {
  ## TODO: deprecate
  version <- orderly_version$new(name, root, locate)
  workdir <- version$test_start(parameters, instance, envir, use_draft, remote)

  msg <- c("orderly has prepared your files at the path",
           "",
           sprintf("  %s", workdir),
           "",
           "but unfortunately due to CRAN policies we cannot change the",
           "directory to that path.  In order to continue testing your",
           "report interactively, please run",
           "",
           sprintf('    setwd("%s")', clean_path(workdir)),
           "",
           "you will be responsible for getting back to your previous working",
           "directory after this, which you can do with",
           "",
           sprintf('    setwd("%s")', clean_path(getwd())),
           "",
           "Please see the documentation ?orderly1::orderly_test_start for",
           "more details")
  message(paste(msg, collapse = "\n"))

  workdir
}


##' @export
##' @rdname orderly_test_start
##' @param path Path to the report that is currently being run
orderly_test_check <- function(path = NULL) {
  path <- path %||% getwd()
  assert_is_directory(path, FALSE)
  info <- cache$test[[normalizePath(path)]]
  if (is.null(info)) {
    stop(sprintf("Not running in test mode (for path %s)", path))
  }
  config <- orderly_config(info$root, FALSE)
  recipe <- orderly_recipe$new(info$name, config, TRUE, path)

  found <- withr::with_dir(path, recipe_exists_artefacts(recipe))
  msg <- sprintf("%7s: %s", ifelse(found, "found", "missing"), names(found))
  artefacts <- names(found)
  h <- withr::with_dir(path, hash_artefacts(artefacts))
  h[is.na(h)] <- "<missing>"
  orderly_log("artefact", sprintf("%s: %s", artefacts, h))
  invisible(all(found))
}
