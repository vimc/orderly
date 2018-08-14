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
                            verbose = FALSE, dry_run = FALSE) {
  config <- orderly_config_get(config, locate)
  root <- config$path

  migrations <- migrate_plan(root, to)

  for (v in names(migrations)) {
    f <- source_to_function(migrations[[v]], "migrate", topenv())
    migrate_apply(root, v, f, config, verbose, dry_run)
  }
}


migrate_plan <- function(root, to = NULL) {
  current <- read_orderly_archive_version(root)
  avail <- available_migrations()

  if (is.null(to)) {
    to <- names(avail)[[length(avail)]]
  }

  apply <- numeric_version(current) < numeric_version(names(avail)) &
    numeric_version(to) >= numeric_version(names(avail))
  avail[apply]
}


migrate_apply <- function(root, version, fun, config, verbose, dry_run) {
  reports <- unlist(lapply(list_dirs(path_archive(root)), list_dirs))
  previous <- read_orderly_archive_version(root)
  orderly_log("migrate", sprintf("'%s' => '%s'", previous, version))
  withCallingHandlers({
    for (p in reports) {
      changed <- migrate_apply1(p, version, fun, config, dry_run)
      nm <- sub(paste0(path_archive(root), "/"), "", p, fixed = TRUE)
      if (changed) {
        orderly_log("updated", nm)
      } else if (verbose) {
        orderly_log("ok", nm)
      }
    }
    if (!dry_run) {
      writeLines(version, path_orderly_archive_version(root))
    }
  },
  error = function(e) {
    if (!dry_run) {
      migrate_rollback(root, version, previous)
    }
  })
}


migrate_apply1 <- function(path, version, fun, config, dry_run) {
  file <- path_orderly_run_rds_backup(path, version)
  if (file.exists(file)) {
    ## I don't know about this one; we should always roll back a
    ## failed migration so this should never happen...
    message(sprintf("Already migrated '%s'", path))
    return(invisible(FALSE))
  }

  file_orig <- path_orderly_run_rds(path)
  res <- fun(readRDS(file_orig), path, config)
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
  writeLines(as.character(previous), path_orderly_archive_version(root))
}


migrate_rollback1 <- function(path, version, root) {
  file <- path_orderly_run_rds_backup(path, version)
  if (file.exists(file)) {
    orderly_log("revert",
                sub(paste0(path_archive(root), "/"), "", path, fixed = TRUE))
    file.rename(file, path_orderly_run_rds(path))
  }
}


migration_result <- function(changed, data) {
  list(changed = changed, data = data)
}


available_migrations <- function() {
  avail <- dir(orderly_file("migrate"), pattern = "^([0-9]+\\.){3}R$",
               full.names = TRUE)
  names(avail) <- sub("\\.R$", "", basename(avail))
  avail[order(numeric_version(names(avail)))]
}


read_orderly_archive_version <- function(root) {
  readlines_if_exists(path_orderly_archive_version(root)) %||% "0.0.0"
}


check_orderly_archive_version <- function(config) {
  used <- numeric_version(config$archive_version)
  curr <- cache$current_archive_version
  if (used < curr) {
    stop(sprintf("orderly archive needs migrating from %s => %s",
                 as.character(used), as.character(curr)),
         call. = FALSE)
  }
}
