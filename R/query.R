## This gives a list of the source report names known to the system.
## This will not include things that have been deleted in source but
## are present in the database, because I want this to be useful for
## getting targets that one can run.
orderly_list <- function(config = NULL, locate = TRUE) {
  config <- orderly_config_get(config, locate)
  basename(list_dirs(path_src(config$path)))
}

## Not sure about the name here: should it be
##
##   orderly_list_drafts
##   orderly_drafts
##
## or something else?  Using list_drafts is not ideal because it
## returns a different type to orderly_list
orderly_drafts <- function(config = NULL, locate = TRUE) {
  orderly_list2(TRUE, config, locate)
}

orderly_archive <- function(config = NULL, locate = TRUE) {
  orderly_list2(FALSE, config, locate)
}

orderly_list2 <- function(draft, config = NULL, locate = TRUE) {
  config <- orderly_config_get(config, locate)
  path <- if (draft) path_draft else path_archive
  check <- list_dirs(path(config$path))
  res <- lapply(check, dir)
  data.frame(name = rep(basename(check), lengths(res)),
             id = as.character(unlist(res)),
             stringsAsFactors = FALSE)
}
