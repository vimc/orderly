##' For interactive testing of orderly code.  This runs through and
##' sets everything up as orderly would (creates a new working
##' directory and copies files into it, pulls data from the database,
##' copies over any dependent reports) but then rather than running
##' the report hands back to the user. The \code{orderly_data}
##' function returns an environment with the extracted data.
##'
##' Previous versions of orderly changed into the created directory
##' when using \code{orderly::orderly_test_start}, which allowed
##' interactive testing of a report, including ensuring that it has
##' created all expected outputs.  However, CRAN rules do not allow
##' changing the working directory, which significantly reduces the
##' usefulness of this function - as such we may remove it entirely in
##' a future version of orderly if it does not prove useful in this
##' more limited form.
##'
##' The new suggested workflow is:
##'
##' \enumerate{
##' \item{run \code{orderly_test_start(...)} to prepare a report directory}
##' \item{manually change into that directory following the printed
##'   instructions}
##' \item{use \code{orderly_test_check} to check that your report has created
##'       the expected artefacts}
##' \item{manually change back to your original directory}
##' }
##'
##' @title Prepare a directory for orderly to use
##'
##' @param name Name of the report to run (see
##'   \code{\link{orderly_list}}).  A leading \code{src/} will be
##'   removed if provided, allowing easier use of autocomplete.
##'
##' @inheritParams orderly_run
##' @return The path to the report directory
##' @export
##' @examples
##'
##' path <- orderly::orderly_example("minimal")
##' p <- orderly::orderly_test_start("example", root = path)
##'
##' # The data in the orderly example is now available to use
##' dat
##'
##' # Check to see which artefacts have been created so far:
##' orderly::orderly_test_check(p)
##'
##' # Manually the code that this report has in its script
##' png(file.path(p, "mygraph.png"))
##' barplot(setNames(dat$number, dat$name), las = 2)
##' dev.off()
##'
##' # We now confirm that the artefact has been created:
##' orderly::orderly_test_check(p)
orderly_test_start <- function(name, parameters = NULL, envir = parent.frame(),
                               root = NULL, locate = TRUE, instance = NULL,
                               use_draft = FALSE, remote = NULL) {
  config <- orderly_config_get(root, locate)
  info <- recipe_prepare(config, name, id_file = NULL, ref = NULL,
                         fetch = FALSE, message = NULL, use_draft = use_draft,
                         parameters = parameters, remote = remote)
  prep <- withr::with_dir(
    info$workdir,
    orderly_prepare_data(config, info, parameters, envir, instance))

  ## We take the opportunity here to filter out any no-longer-existing
  ## test reports.
  if (length(cache$test) > 0) {
    cache$test <- cache$test[file.exists(names(cache$test))]
  }

  cache$test[[normalizePath(info$workdir)]] <- info

  msg <- c("orderly has prepared your files at the path",
           "",
           sprintf("  %s", info$workdir),
           "",
           "but unfortunately due to CRAN policies we cannot change the",
           "directory to that path.  In order to continue testing your",
           "report interactively, please run",
           "",
           sprintf('    setwd("%s")', clean_path(info$workdir)),
           "",
           "you will be responsible for getting back to your previous working",
           "directory after this, which you can do with",
           "",
           sprintf('    setwd("%s")', clean_path(info$owd)),
           "",
           "Please see the documentation ?orderly::orderly_test_start for",
           "more details")
  message(paste(msg, collapse = "\n"))

  info$workdir
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
  found <- withr::with_dir(path, recipe_exists_artefacts(info))
  msg <- sprintf("%7s: %s", ifelse(found, "found", "missing"), names(found))
  artefacts <- names(found)
  h <- withr::with_dir(path, hash_artefacts(artefacts))
  h[is.na(h)] <- "<missing>"
  orderly_log("artefact", sprintf("%s: %s", artefacts, h))
  invisible(all(found))
}


##' @export
##' @rdname orderly_test_start
##' @examples
##' # The function orderly_data does all the preparation work that
##' # orderly_run does, but does not run the report; instead it
##' # returns the created environment with all the data and parameters
##' # set.
##' path <- orderly::orderly_example("demo")
##' env <- orderly::orderly_data("other", list(nmin = 0.2), root = path)
##' ls(env)
##' env$nmin
##' env$extract
orderly_data <- function(name, parameters = NULL, envir = NULL,
                         root = NULL, locate = TRUE, instance = NULL,
                         use_draft = FALSE) {
  config <- orderly_config_get(root, locate)
  info <- recipe_read(file.path(path_src(config$root), name),
                      config, use_draft = use_draft, parameters = parameters)
  envir <- orderly_environment(envir)
  recipe_data(config, info, parameters, envir, instance)$dest
}
