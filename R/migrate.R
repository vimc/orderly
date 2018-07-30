## The assumption at this point is that we'll update the *rds* only
## and not the yaml, and no other files in the directory.  The
## rationale here is that the yaml is too hard to edit (roundtripping
## through R is lossy without careful serialisation/deserialisation
## and so unlikely to work well in all cases).  The new database
## addition code only reads from the rds so this is the bit that we
## really want in anycase.  Perhaps a future version will regenerate
## the yml from the rds?
##
## However, the path will be allowed to come through and in some cases
## we might add new files.
orderly_migrate <- function(config = NULL, locate = TRUE, to = NULL,
                            dry_run = FALSE) {
  config <- orderly_config_get(config, locate)
  current <- read_orderly_version(config$path)

  avail <- sort(dir(orderly_file("migrate"), pattern = "^([0-9]+\\.){3}R$",
                    full.names = TRUE))
  names(avail) <- sub("\\.R$", "", basename(avail))

  if (is.null(to)) {
    to <- names(avail)[[length(avail)]]
  }

  apply <- numeric_version(current) < numeric_version(names(avail)) &
    numeric_version(to) >= numeric_version(names(avail))
  avail <- avail[apply]

  for (v in names(avail)) {
    f <- source_to_function(avail[[v]], "migrate", topenv())
    migrate_apply(config$path, v, f, dry_run)
  }
}


migrate_apply <- function(root, version, fun, dry_run) {
  reports <- unlist(lapply(list_dirs(path_archive(root)), list_dirs))
  previous <- read_orderly_version(root)
  orderly_log("migrate", sprintf("'%s' => '%s'", previous, version))
  withCallingHandlers({
    for (p in reports) {
      changed <- migrate_apply1(p, version, fun, dry_run)
      if (changed) {
        orderly_log("updated",
                    sub(paste0(path_archive(root), "/"), "", p, fixed = TRUE))
      }
    }
    if (!dry_run) {
      writeLines(version, path_orderly_version(root))
    }
  },
  error = function(e) {
    migrate_rollback(root, version, previous)
  })
}


migrate_apply1 <- function(path, version, fun, dry_run) {
  file <- path_orderly_run_rds_backup(path, version)
  if (file.exists(file)) {
    ## I don't know about this one; we should always roll back a
    ## failed migration so this should never happen...
    message(sprintf("Already migrated '%s'", path))
    return(invisible(FALSE))
  }

  file_orig <- path_orderly_run_rds(path)
  res <- fun(readRDS(file_orig), path)
  if (res$changed && !dry_run) {
    ## Start by making the backup
    file_copy(file_orig, file)
    saveRDS(res$data, file_orig)
  }
  res$changed
}


migrate_rollback <- function(root, version, previous) {
  reports <- unlist(lapply(list_dirs(path_archive(root)), list_dirs))
  orderly_log("rollback", sprintf("'%s' => '%s'", version, previous))
  for (p in reports) {
    migrate_rollback1(p, version, root)
  }
  writeLines(as.character(previous), path_orderly_version(root))
}


migrate_rollback1 <- function(path, version, root) {
  file <- path_orderly_run_rds_backup(path, version)
  if (file.exists(file)) {
    orderly_log("revert",
                sub(paste0(path_archive(root), "/"), "", path, fixed = TRUE))
    file.rename(file, path_orderly_run_rds(path))
  }
}


read_orderly_version <- function(root) {
  readlines_if_exists(path_orderly_version(root)) %||% "0.0.0"
}
