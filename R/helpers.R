##' Add one or more resources to an \code{orderly.yml} file.
##'
##' @title Add a resource to orderly.yml
##'
##' @param resources Character vector of resources to add.  These must
##'   be filenames relative to the report directory, must exist, and
##'   must not already be present in the orderly.yml
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
orderly_use_resource <- function(resources, name = NULL, root = NULL,
                                 locate = FALSE, show = TRUE, edit = TRUE,
                                 prompt = TRUE) {
  loc <- orderly_develop_location(name, root, locate)
  config <- loc$config
  name <- loc$name
  path <- loc$path

  info <- recipe_read(path, config, FALSE)

  assert_character(resources)
  assert_file_exists(resources, workdir = path, name = "Resource")

  err <- intersect(info$resources, resources)
  if (length(err) > 0L) {
    stop("Resource already declared: ", paste(squote(err), collapse = ", "))
  }

  err <- unique(resources[duplicated(resources)])
  if (length(err) > 0L) {
    stop("Resource duplicated: ", paste(squote(err), collapse = ", "))
  }

  path_yml <- file.path("src", name, "orderly.yml")
  yml <- readLines(file.path(root, path_yml))

  dat <- yaml_block_info("resources", yml)

  if (!dat$exists) {
    ## TODO: could add a more extensive help here - ideally syncing up
    ## with the help that exists in the orderly template.
    to_add <-
      c("",
        "# Resources that the script needs to run",
        "resources:",
        sprintf("  - %s", resources))
    where <- length(yml)
  } else if (dat$block) {
    to_add <- sprintf("%s- %s", dat$indent, resources)
    where <- dat$end
  } else {
    where <- dat$start
    prev <- yaml_load(yml[[where]])$resources
    yml[[where]] <- "resources:"
    to_add <- sprintf("  - %s", c(prev, resources))
  }

  withr::with_dir(
    config$root,
    insert_into_file(yml, where, to_add, path_yml, show, edit, prompt))
}


insert_into_file <- function(text, where, value, path, show, edit, prompt) {
  x <- filediff(text, where, value)

  if (show) {
    message(sprintf("Changes to '%s'", path))
    cat(format_filediff(x))
  }

  if (edit && prompt && !prompt_ask_yes_no("Write changes to file? ")) {
    edit <- FALSE
    message("Not modifying file")
  }

  if (edit) {
    message(sprintf("Writing to '%s'", path))
    writeLines(x$result, path)
  }

  invisible(x)
}


filediff <- function(text, where, value) {
  i <- seq_len(where)
  ret <- list(text = text,
              where = where,
              value = value,
              result = c(text[i], value, text[-i]),
              changed = seq_along(value) + where)
  class(ret) <- "filediff"
  ret
}


format_filediff <- function(x, ..., context = 2L) {
  i <- seq_along(x$result)
  focus <- range(x$changed)
  i <- i[i > focus[[1L]] - context & i < focus[[2L]] + context]
  line <- format(i)
  text <- x$result[i]
  changed <- i %in% x$changed
  grey <- crayon::make_style("grey")
  line[changed] <- crayon::bold(grey(line[changed]))
  text[changed] <- crayon::bold(text[changed])
  paste(sprintf("%s | %s\n", line, text), collapse = "")
}
