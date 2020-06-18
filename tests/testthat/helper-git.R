git_ref_to_sha <- function(ref, root = NULL, check = FALSE) {
  assert_scalar_character(ref)
  res <- git_run(c("rev-parse", ref), root = root, check = FALSE)
  if (res$success) {
    res$output
  } else if (check) {
    stop(sprintf("Git reference '%s' not found", ref), call. = FALSE)
  } else {
    NA_character_
  }
}

git_ref_exists <- function(ref, root = NULL) {
  assert_scalar_character(ref)
  git_run(c("merge-base", ref, "HEAD"), root = root, check = FALSE)$success
}
