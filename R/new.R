##' Create new report, starting from a template.  Orderly comes with a
##' set of templates, but projects can bring their own templates; see
##' Details below for how these are configured and discovered by
##' orderly.
##'
##' To create a custom template, create a directory \code{templates}
##' within your orderly root.  Within that directory create
##' directories containing all the files that you would like a report
##' to contain.  This \emph{must} contain a file
##' \code{orderly.yml} but may contain further files (for example, you
##' might want a default script and Rmd file).
##'
##' If \code{template} is not given (i.e., is \code{NULL}) then we
##' look for a template called \code{default} (i.e., stored at
##' \code{template/default}), then fall back on the system orderly
##' template.
##'
##' We first look for a file \code{orderly/template.yml} within the
##' orderly root.  If that is not found, then a copy from the orderly
##' package is used.  This can always be used by using \code{template
##' = "system"}.
##'
##' @title Create new report
##'
##' @param name Name of the new report (will be a directory name).
##'
##' @param quiet Logical, indicating if informational messages should
##'   be suppressed.
##'
##' @param template The name of a template.  If \code{NULL} orderly
##'   will search for a template (see Details).  If given it must be
##'   the name of a directory within a directory \code{templates} in
##'   your project root.  The special label "orderly" will use
##'   orderly's builtin template.
##'
##' @inheritParams orderly_list
##'
##' @seealso \code{\link{orderly_init}} for initialising a new orderly
##'   repository.
##'
##' @export
##' @examples
##' path <- orderly::orderly_example("minimal")
##'
##' # Create a new report with the name "myreport" in this orderly
##' # repository:
##' orderly::orderly_new("myreport", root = path)
##'
##' # The directory will be initialised with a orderly.yml file
##' # containing documentation
##' dir(file.path(path, "src", "myreport"))
##' readLines(file.path(path, "src", "myreport", "orderly.yml"))
orderly_new <- function(name, root = NULL, locate = TRUE, quiet = FALSE,
                        template = NULL) {
  config <- orderly_config_get(root, locate)
  assert_scalar_character(name)
  if (grepl("[[:space:]]", name)) {
    stop("'name' cannot contain spaces")
  }
  dest <- file.path(path_src(config$root), name)
  if (file.exists(dest)) {
    stop(sprintf("A report already exists called '%s'", name))
  }

  if (is.null(template)) {
    template <-
      if (has_template(config$root, "default")) "default" else "system"
  }

  if (template == "system") {
    orderly_new_system(dest, config)
  } else {
    orderly_new_user(dest, config, template)
  }

  if (!quiet) {
    message(sprintf("Created report at '%s'", dest))
    message("Edit the file 'orderly.yml' within this directory")
  }
  invisible(dest)
}


orderly_new_system <- function(dest, config) {
  dir.create(dest)
  template <- orderly_file("orderly_example.yml")
  dest_yml <- file.path(dest, "orderly.yml")

  if (nrow(config$fields) == 0L) {
    file_copy(template, dest_yml)
  } else {
    txt <- readLines(template)
    fields <- config$fields
    desc <- ifelse(is.na(fields$description), fields$name, fields$description)
    req <- ifelse(fields$required, "required", "optional")
    str <- sprintf("%s -- character (%s)", desc, req)
    str <- vcapply(strwrap(str, prefix = "# ", simplify = FALSE),
                   paste, collapse = "\n")
    ex <- sprintf(ifelse(fields$required, "%s: ~", "# %s:"), fields$name)
    writeLines(c(txt, paste("", str, "#", ex, sep = "\n")), dest_yml)
  }
}


orderly_new_user <- function(dest, config, template) {
  if (!has_template(config$root, template)) {
    stop(sprintf("Did not find file '%s' within orderly root",
                 file.path("template", template, "orderly.yml")))
  }
  dir.create(dest)
  path_template <- file.path(config$root, "template", template)
  files <- dir(path_template, all.files = TRUE, no.. = TRUE, full.names = TRUE)
  file_copy(files, dest, recursive = TRUE)
}


has_template <- function(root, name) {
  file.exists(file.path(root, "template", name, "orderly.yml"))
}
