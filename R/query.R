## This gives a list of the source report names known to the system.
## This will not include things that have been deleted in source but
## are present in the database, because I want this to be useful for
## getting targets that one can run.
orderly_list <- function(config = NULL, locate = TRUE) {
  config <- orderly_config_get(config, locate)
  basename(list_dirs(path_src(config$path)))
}
