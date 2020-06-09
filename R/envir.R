orderly_envir_read <- function(path) {
  filename <- path_orderly_envir_yml(path)
  if (is.null(path) || !file.exists(filename)) {
    return(NULL)
  }

  ## TODO: check case VIMC-889
  dat <- yaml_read(filename)
  assert_named(dat, TRUE, basename(filename))
  n <- lengths(dat)
  nok <- n > 1L
  if (any(nok)) {
    stop(sprintf("Expected all elements of %s to be scalar (check %s)",
                 basename(filename),
                 paste(squote(names(dat)[nok]), collapse = ", ")))
  }
  vcapply(dat[n == 1], as.character)
}
