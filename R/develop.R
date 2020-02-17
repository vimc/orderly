##' The functions \code{orderly_develop_start},
##' \code{orderly_develop_status} and \code{orderly_develop_clean}
##' provide a workflow for developing a report in much the same way as
##' one might write code outside of orderly.
##' \code{orderly_develop_start} will copy all files required (global
##' resources and dependencies) into the report source directory, as
##' well as collect all data and parameters - at this point the
##' directory can be developed in directly.
##' \code{orderly_develop_status} provides information about the
##' status of files in the directory, while
##' \code{orderly_develop_clean} deletes all copied files.
##'
##' These functions are designed to work within a report's \code{src}
##' directory.  For example, for a report \code{analysis} they will
##' alter or report on the directory \code{src/analysis}.  It is
##' intended that \code{orderly_develop_start} can be run repeatedly;
##' doing this will \emph{refresh} the contents of the directory if
##' upstream files have been updated.
##'
##' Some degree of care should be used while using these functions.
##'
##' Because \code{orderly_develop_start} copies files into your source
##' tree you should be careful to add these files to your
##' \code{.gitignore} files so that they are not included if using
##' git.  Rerunning \code{orderly_develop_start} will copy a fresh
##' copy dependencies into your tree, overwriting files that are there
##' without warning.
##'
##' The \code{orderly_develop_clean} function will delete dependencies
##' without warning.
##'
##' @title Develop an orderly report
##'
##' @param name Name of the report to develop (see
##'   \code{\link{orderly_list}}).  A leading \code{src/} will be
##'   removed if provided, allowing easier use of autocomplete.
##'   Alternatively, the default of \code{NULL} is useful if you have
##'   already set the working directory to be the source directory.
##'
##' @inheritParams orderly_run
##'
##' @return A character vector with the full path to the directory,
##'   invisibly.
##'
##' @export
##' @examples
##' path <- orderly::orderly_example("demo")
##'
##' # This report uses a depenency - it requires that the file
##' # incoming.csv exists.  This file is created from the report 'other'
##' orderly::orderly_develop_status("use_dependency", root = path)
##'
##' # Copy the required dependencies over, in this case from a draft report
##' orderly::orderly_run("other", list(nmin = 0), root = path, echo = FALSE)
##' orderly::orderly_develop_start("use_dependency", root = path,
##'                                use_draft = TRUE)
##'
##' # Files have been copied across into the source directory
##' orderly::orderly_develop_status("use_dependency", root = path)
##'
##' # The report can then be developed as needed, interactively.  After
##' # we're happy things can be cleaned up with
##' orderly::orderly_develop_clean("use_dependency", root = path)
##'
orderly_develop_start <- function(name = NULL, parameters = NULL,
                                  envir = parent.frame(),
                                  root = NULL, locate = TRUE, instance = NULL,
                                  use_draft = FALSE) {
  loc <- orderly_develop_location(name, root, locate)
  envir <- orderly_environment(envir)

  orderly_log("name", loc$name)

  info <- recipe_read(loc$path, loc$config, use_draft = use_draft)

  info$workdir <- loc$path
  withr::with_dir(info$workdir, {
    info <- recipe_copy_global(info, loc$config)
    info <- recipe_copy_depends(info)
    orderly_prepare_data(loc$config, info, parameters, envir, instance)
  })

  invisible(loc$path)
}


##' @export
##' @rdname orderly_develop_start
orderly_develop_status <- function(name = NULL, root = NULL, locate = TRUE) {
  loc <- orderly_develop_location(name, root, locate)
  orderly_status(loc$path)
}


##' @export
##' @rdname orderly_develop_start
orderly_develop_clean <- function(name = NULL, root = NULL, locate = TRUE) {
  loc <- orderly_develop_location(name, root, locate)
  status <- orderly_status(loc$path)
  drop <- status$filename[status$derived & status$present]
  if (length(drop) > 0L) {
    orderly_log("remove", drop)
    file.remove(file.path(loc$path, drop))
  }
  invisible(NULL)
}


orderly_develop_location <- function(name, root, locate) {
  config <- orderly_config_get(root, locate)
  config <- check_orderly_archive_version(config)

  if (is.null(name)) {
    if (!file.exists("orderly.yml")) {
      stop("Did not find orderly.yml within working directory")
    }
    if (!fs::path_has_parent(getwd(), config$root)) {
      stop("Working directory is not within the orderly root")
    }
    rel <- fs::path_split(fs::path_rel(getwd(), config$root))[[1]]
    if (length(rel) != 2 || rel[[1L]] != "src") {
      stop("Unexpected working directory - expected src/<name>")
    }
    name <- rel[[2L]]
  } else if (grepl("^src/.+", name)) {
    name <- sub("^src/", "", name)
  }

  path <- file.path(path_src(config$root), name)
  inplace <- same_path(path, getwd())

  list(config = config, name = name, path = path, inplace = inplace)
}


## This might be more general, but for now assume not
orderly_status <- function(path) {
  assert_is_directory(path, FALSE)
  assert_file_exists(file.path(path, "orderly.yml"))
  ## TODO: this needs making more robust
  config <- orderly_config_get(file.path(path, "..", ".."), FALSE)
  info <- recipe_read(path, config, FALSE)

  ## There are the file that orderly cares about
  internal <- list(orderly = "orderly.yml",
                   script = info$script,
                   source = info$sources,
                   resource = info$resources,
                   global = names(info$global_resources),
                   dependency = info$depends$as,
                   artefact = unlist(info$artefacts[, "filenames"],
                                     FALSE, FALSE))
  status <- data_frame(
    filename = unlist(internal, FALSE, FALSE),
    type = rep(names(internal), lengths(internal)))
  extra <- setdiff(dir(path, recursive = TRUE), status$filename)
  if (length(extra) > 0L) {
    status <- rbind(status, data_frame(filename = extra, type = "unknown"))
  }
  status$present <- file.exists(file.path(path, status$filename))
  ## TODO: better name?
  status$derived <- status$type %in% c("global", "dependency", "artefact")
  ## status$origin <- NA_character_
  ## This is going to be fairly hard to get right.  Better might be to
  ## check if it is *consistent*?
  ## if (any(status$type == "dependency" & status$present)) {
  ##   con <- orderly_db("destination", config)
  ##   browser()
  ## }

  class(status) <- c("orderly_status", "data.frame")
  status
}
