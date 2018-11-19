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


##' Migrate an orderly archive.  This is needed periodically when the
##' orderly archive version changes.  If you get a message like
##' \code{orderly archive needs migrating from a.b.c => x.y.z} then
##' you need to run this function.  The archive version is at most
##' equal to the package version.
##'
##' Sometimes we add change information saved out in the orderly run.
##' This requires patching previously run versions of the orderly
##' metadata and that's not something we want to do lightly.  This
##' function uses a relatively safe, and reversible, way of migrating
##' metadata.  We only modify the \code{orderly_run.rds} files and
##' leave the human-readable \code{orderly_run.yml} ones alone (at
##' least for now).
##'
##' @title Migrate an orderly archive
##' @inheritParams orderly_list
##'
##' @param to The version to migrate to.  The default is the current
##'   archive version; this is almost always what is wanted.
##'
##' @param verbose Logical, indicating if extra noisy output from the
##'   migration should be given.
##'
##' @param dry_run Logical, indicating if we should try running the
##'   migration but not actually applying it.  This is intended
##'   primarily for developing new migrations and will probably not
##'   work if you are multiple archive versions behind.
##'
##' @export
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
  ## This ensures we work through all reports in order of creation
  ## (based on id).
  archive <- orderly_list_archive(root, FALSE)
  archive <- archive[order(archive$id), ]
  reports <- file.path(path_archive(root), archive$name, archive$id)

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
      write_orderly_archive_version(version, root)
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
  dat <- readRDS(file_orig)
  version_previous <- get_version(dat$archive_version)
  if (version_previous < version) {
    res <- fun(dat, path, config)
    res$data$archive_version <- numeric_version(version)
    if (!dry_run) {
      file_copy(file_orig, file)
      saveRDS(res$data, file_orig)
    }
    res$changed
  } else {
    FALSE
  }
}


migrate_rollback <- function(root, version, previous) {
  reports <- unlist(lapply(list_dirs(path_archive(root)), list_dirs))
  orderly_log("rollback", sprintf("'%s' => '%s'", version, previous))
  for (p in reports) {
    migrate_rollback1(p, version, root)
  }
  write_orderly_archive_version(previous, root)
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
  get_version(readlines_if_exists(path_orderly_archive_version(root)), FALSE)
}


write_orderly_archive_version <- function(version, root) {
  writeLines(as.character(version), path_orderly_archive_version(root))
}


check_orderly_archive_version <- function(config) {
  used <- numeric_version(config$archive_version)
  curr <- cache$current_archive_version
  if (used == "0.0.0" && nrow(orderly_list_archive(config)) == 0L) {
    write_orderly_archive_version(curr, config$path)
    used <- curr
  }
  if (used < curr) {
    stop(sprintf("orderly archive needs migrating from %s => %s\n",
                 as.character(used), as.character(curr)),
         "Run orderly::orderly_migrate() to fix",
         call. = FALSE)
  }
}
