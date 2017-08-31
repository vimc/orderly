patch_orderly <- function(root) {
  nms <- orderly::orderly_list(root)
  for (p in file.path(root, "src", nms)) {
    patch_orderly_yml(p)
  }

  drafts <- orderly::orderly_list_drafts(root)
  archive <- orderly::orderly_list_archive(root)
  path <- c(file.path(root, "draft", drafts$name, drafts$id),
            file.path(root, "archive", archive$name, archive$id))

  config <- orderly:::orderly_config(root)
  for (p in path) {
    patch_orderly_yml(p)
    patch_orderly_run_yml(p, config)
  }

  orderly::orderly_rebuild(".")
}

patch_orderly_yml <- function(path) {
  filename <- file.path(path, "orderly.yml")
  dat <- readLines(filename)
  dat2 <- sub("filename:", "filenames:", dat)
  changed <- !identical(dat, dat2)
  if (changed) {
    message("patching ", filename)
    writeLines(dat2, filename)
  }
  changed
}

patch_orderly_run_yml <- function(path, config) {
  hash_orderly <- orderly:::recipe_read(path, config = config)$hash
  hash_input <- orderly:::hash_files(file.path(path, "orderly.yml"), FALSE)

  filename <- orderly:::path_orderly_run_yml(path)
  if (!file.exists(filename)) {
    message("orderly_run.yml not present at ", path)
    return()
  }
  cmp <- orderly:::yaml_read(filename)
  changed <- hash_orderly != cmp$hash_orderly || hash_input != cmp$hash_input
  if (changed) {
    message("updating ", filename)
    cmp$hash_orderly <- hash_orderly
    cmp$hash_input <- hash_input
    writeLines(yaml::as.yaml(cmp, column.major = FALSE), filename)
  }
}

if (!interactive()) {
  patch_orderly(".")
}
