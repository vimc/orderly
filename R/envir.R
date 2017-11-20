orderly_envir_read <- function(path) {
  filename <- path_orderly_envir_yml(path)
  if (file.exists(filename)) {
    ## TODO: check case VIMC-889
    dat <- yaml_read(filename)
    assert_named(dat, TRUE, basename(filename))
    nok <- lengths(dat) != 1L
    if (any(nok)) {
      stop(sprintf("Expected all elements of %s to be scalar (check %s)",
                   basename(filename),
                   paste(squote(names(dat)[nok]), collapse = ", ")))
    }
    vcapply(dat, as.character)
  } else {
    NULL
  }
}
