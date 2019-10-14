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
##' @inheritParams orderly_run
##' @export
##' @examples
##'
##' path <- orderly::orderly_example("minimal")
##' info <- orderly::orderly_test_start("example", root = path)
##'
##' # The data in the orderly example is now available to use
##' dat
##'
##' # Check to see which artefacts have been created so far:
##' orderly::orderly_test_check(info$path)
##'
##' # Manually the code that this report has in its script
##' png("mygraph.png")
##' barplot(setNames(dat$number, dat$name), las = 2)
##' dev.off()
##'
##' orderly::orderly_test_check()
orderly_test_start <- function(name, parameters = NULL, envir = parent.frame(),
                               root = NULL, locate = TRUE) {
  config <- orderly_config_get(root, locate)
  info <- recipe_prepare(config, name, id_file = NULL, ref = NULL,
                         fetch = FALSE, message = NULL)
  prep <- withr::with_dir(
    info$workdir,
    orderly_prepare_data(config, info, parameters, envir))

  ret <- list(
    owd = getwd(),
    path = info$workdir,
    name = name,
    id = info$id,
    parameters = parameters,
    config = config,
    info = info)

  ## We take the opportunity here to filter out any no-longer-existing
  ## test reports.
  if (length(cache$test) > 0) {
    cache$test <- cache$test[file.exists(names(cache$test))]
  }
  cache$test[[normalizePath(info$workdir)]] <- ret

  msg <- c("orderly has prepared your files at the path",
           "",
           sprintf("  %s", info$workdir),
           "",
           "but unfortunately due to CRAN policies we cannot change the",
           "directory to that path.  In order to continue testing your",
           "report interactively, please run",
           "",
           sprintf('    setwd("%s")', info$workdir),
           "",
           "you will be responsible for getting back to your previous working",
           "directory after this, which you can do with",
           "",
           sprintf('    setwd("%s")', info$owd),
           "",
           "Please see the documentation ?orderly::orderly_test_start for",
           "more details")
  message(paste(msg, collapse = "\n"))

  invisible(ret)
}


##' @export
##' @rdname orderly_test_start
orderly_test_check <- function(path = NULL) {
  path <- path %||% getwd()
  assert_is_directory(path)
  info <- cache$test[[normalizePath(path)]]
  if (is.null(info)) {
    stop(sprintf("Not running in test mode (for path %s)", path))
  }
  found <- withr::with_dir(path, recipe_exists_artefacts(info$info))
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
                         root = NULL, locate = TRUE) {
  config <- orderly_config_get(root, locate)
  info <- recipe_read(file.path(path_src(config$root), name), config)
  envir <- orderly_environment(envir)
  recipe_data(config, info, parameters, envir)$dest
}
