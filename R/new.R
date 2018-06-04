##' Create new report, starting from a template.  We first look for a
##' file \code{orderly/template.yml} within the orderly root.  If that
##' is not found, then a copy from the orderly package is used.
##'
##' @title Create new report
##'
##' @param name Name of the new report (will be a directory name).
##'
##' @param quiet Logical, indicating if informational messages should
##'   be suppressed.
##'
##' @inheritParams orderly_list
##' @export
orderly_new <- function(name, config = NULL, locate = TRUE, quiet = FALSE) {
  config <- orderly_config_get(config, locate)
  assert_scalar_character(name)
  if (grepl("[[:space:]]", name)) {
    stop("'name' cannot contain spaces")
  }
  dest <- file.path(path_src(config$path), name)
  if (file.exists(dest)) {
    stop(sprintf("A report already exists called '%s'", name))
  }
  dir.create(dest)

  ### Sort out the extra fields when present.  For a custom template
  ### we don't get involved with this because it's too hard to work
  ### out if a custom optional field has been included.
  template <- file.path(config$path, "orderly", "template.yml")
  is_custom_template <- file.exists(template)
  if (!is_custom_template) {
    template <- orderly_file("orderly_example.yml")
  }
  yml <- file.path(dest, "orderly.yml")
  file.copy(template, yml)

  if (!is_custom_template && nrow(config$fields) > 0L) {
    txt <- readLines(yml)
    fields <- config$fields
    desc <- ifelse(is.na(fields$description), fields$name, fields$description)
    req <- ifelse(fields$required, "required", "optional")
    str <- sprintf("%s -- %s (%s)", desc, fields$type, req)
    str <- vcapply(strwrap(str, prefix = "# ", simplify = FALSE),
                   paste, collapse = "\n")
    ex <- sprintf(ifelse(fields$required, "%s: ~", "# %s:"), fields$name)
    orig <- readLines(yml)
    writeLines(c(orig, paste("", str, "#", ex, sep = "\n")), yml)
  }

  if (!quiet) {
    message(sprintf("Created report at '%s'", dest))
    message("Edit the file 'orderly.yml' within this directory")
  }
  invisible(dest)
}
