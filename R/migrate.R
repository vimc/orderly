##' Migrate an orderly archive.  This is needed periodically when the
##' orderly archive version changes.  If you get a message like
##' `orderly archive needs migrating from a.b.c => x.y.z` then
##' you need to run this function.  The archive version is at most
##' equal to the package version.
##'
##' Sometimes we add change information saved out in the orderly run.
##' This requires patching previously run versions of the orderly
##' metadata and that's not something we want to do lightly.  This
##' function uses a relatively safe, and reversible, way of migrating
##' metadata.  We modify the `orderly_run.rds` files, but will
##' create versioned backups as files are changed.
##'
##' @title Migrate an orderly archive
##' @inheritParams orderly_list
##'
##' @param to The version to migrate to.  The default is the current
##'   archive version; this is almost always what is wanted.
##'
##' @param dry_run Logical, indicating if we should try running the
##'   migration but not actually applying it.  This is intended
##'   primarily for developing new migrations and will probably not
##'   work if you are multiple archive versions behind.
##'
##' @param skip_failed Logical, where `TRUE` we will skip over
##'   entries that failed to be migrated.  This is expected to be
##'   useful on local archives only because it violates the
##'   append-only nature of orderly.  However, if a local archive
##'   contains unusual copies of orderly archives that can't be
##'   migrated this might come in helpful.
##'
##' @param clean Logical, where `TRUE` (and where the migration
##'   was successful and `dry_run` is `FALSE`) orderly will
##'   clean up all migration backup files.  Use this periodically to
##'   clean up the archive.
##'
##' @export
##' @return No return value, this function is called only for its side effects
##' @examples
##' # Without an orderly repository created by a previous version of
##' # orderly, this function does nothing interesting:
##' path <- orderly1::orderly_example("minimal")
##' orderly1::orderly_migrate(path)
orderly_migrate <- function(root = NULL, locate = TRUE, to = NULL,
                            dry_run = FALSE, skip_failed = FALSE,
                            clean = FALSE) {
  ## We'll skip warnings here - they'll come out as messages rather
  ## than warnings.
  oo <- options(orderly.nowarnings = TRUE)
  on.exit(options(oo))

  config <- orderly_config(root, locate)
  root <- config$root

  migrations <- migrate_plan(config$archive_version, to)

  for (v in names(migrations)) {
    f <- source_to_function(migrations[[v]], "migrate", topenv())
    migrate_apply(root, v, f, config, dry_run, skip_failed)
  }

  if (clean) {
    migrate_clean(config, dry_run)
  }
}


migrate_plan <- function(from, to = NULL) {
  avail <- available_migrations()

  if (is.null(to)) {
    to <- names(avail)[[length(avail)]]
  }

  apply <- numeric_version(from) < numeric_version(names(avail)) &
    numeric_version(to) >= numeric_version(names(avail))
  avail[apply]
}


migrate_apply <- function(root, version, fun, config, dry_run, skip_failed) {
  ## This ensures we work through all reports in order of creation
  ## (based on id).
  archive <- orderly_list_archive(root, FALSE)
  archive <- archive[order(archive$id), ]
  reports <- file.path(path_archive(root), archive$name, archive$id)

  previous <- read_orderly_archive_version(root)
  orderly_log("migrate", sprintf("'%s' => '%s'", previous, version))
  withCallingHandlers({
    for (p in reports) {
      migrate_apply1(p, version, fun, config, dry_run, skip_failed)
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


migrate_apply1 <- function(root, version, fun, config, dry_run, skip_failed) {
  if (skip_failed) {
    return(tryCatch(
      migrate_apply1(root, version, fun, config, dry_run, FALSE),
      error = function(e) migrate_skip(e, root, version, config, dry_run)))
  }
  file <- path_orderly_run_rds_backup(root, version)

  ## This should never happen, and the behaviour if it does is not
  ## well defined.  So let's assert here and if it turns out to matter
  ## we'll find out soon enough.
  stopifnot(!file.exists(file))

  file_orig <- path_orderly_run_rds(root)
  dat <- readRDS(file_orig)
  name <- dat$meta$name
  id <- dat$meta$id

  version_previous <- get_version(dat$archive_version)
  if (version_previous < version) {
    withCallingHandlers({
      res <- fun(dat, root, config)
      res$data$archive_version <- numeric_version(version)
      if (!dry_run) {
        file_copy(file_orig, file)
        saveRDS(res$data, file_orig)
      }
      changed <- res$changed
    }, error = function(e) {
      migrate_fail_message(name, id, e)
    })
  } else {
    changed <- FALSE
  }
  status <- if (changed) "updated" else "ok"
  orderly_log(status, sprintf("%s/%s", name, id))
}


migrate_fail_message <- function(name, id, error) {
  orderly_log("ERROR", c(sprintf("%s/%s", name, id),
                         "migration failed with error:",
                         error$message))
}


migrate_skip <- function(error, root, version, config, dry_run) {
  name <- basename(dirname(root))
  id <- basename(root)
  migrate_fail_message(name, id, error)
  dest_rel <- file.path(path_archive_broken(), name, id)
  dest <- file.path(config$root, dest_rel)
  if (dry_run) {
    action <- "would be"
  } else {
    dir.create(dirname(dest), FALSE, TRUE)
    file_move(root, dest)
    action <- "has been"
  }
  message(sprintf("...this report %s moved to\n\t'%s'",
                  action, dest_rel))
  "failed"
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
  avail <- dir(orderly_file("migrate"), pattern = "^([0-9]+\\.){3}R$", # nolint
               full.names = TRUE)
  names(avail) <- sub("\\.R$", "", basename(avail))
  avail[order(numeric_version(names(avail)))]
}


read_orderly_archive_version <- function(root) {
  get_version(readlines_if_exists(path_orderly_archive_version(root)), FALSE)
}


write_orderly_archive_version <- function(version, root) {
  root <- path_orderly_archive_version(root)
  dir.create(dirname(root), FALSE, TRUE)
  writeLines(as.character(version), root)
}


check_orderly_archive_version <- function(config) {
  used <- numeric_version(config$archive_version)
  curr <- cache$current_archive_version
  if (used == "0.0.0" && nrow(orderly_list_archive(config)) == 0L) {
    orderly_log("info",
                sprintf("Writing initial orderly archive version as %s", curr))
    write_orderly_archive_version(curr, config$root)
    used <- curr
    config$archive_version <- curr
  } else if (used < curr) {
    stop(sprintf("orderly archive needs migrating from %s => %s\n",
                 as.character(used), as.character(curr)),
         "Run orderly1::orderly_migrate() to fix",
         call. = FALSE)
  }
  config
}


migrate_single <- function(path, config) {
  assert_is(config, "orderly_config")
  dat_rds <- readRDS(path_orderly_run_rds(path))

  report_archive_version <- dat_rds$archive_version
  archive_version <- config$archive_version

  if (report_archive_version > archive_version) {
    ## TODO: better message here
    stop("Report was created with orderly more recent than this, upgrade!")
  } else if (report_archive_version == archive_version) {
    return()
  }

  migrations <- migrate_plan(report_archive_version, archive_version)

  for (v in names(migrations)) {
    fun <- source_to_function(migrations[[v]], "migrate", topenv())
    orderly_log("migrate", sprintf("'%s' => '%s'", report_archive_version, v))
    migrate_apply1(path, v, fun, config,
                   dry_run = FALSE, skip_failed = FALSE)
    report_archive_version <- v
  }
}


migrate_metadata <- function(dat_rds, config) {
  assert_is(config, "orderly_config")

  report_archive_version <- dat_rds$archive_version
  archive_version <- config$archive_version

  if (report_archive_version > archive_version) {
    stop("Report was created with orderly more recent than this, upgrade!")
  }

  if (report_archive_version < archive_version) {
    stop(sprintf(
      "Can't migrate metadata for '%s:%s', migrate remote or pull archive",
      dat_rds$meta$name, dat_rds$meta$id))
  }
}


migrate_clean <- function(config, dry_run) {
  files <- list.files(file.path(config$root, "archive"),
                      "^orderly_run_([0-9]+\\.){3}rds", # nolint
                      recursive = TRUE, full.names = TRUE)
  size <- pretty_bytes(sum(file_size(files)))
  orderly_log("clean",
              sprintf("%d backup files to delete (%s)", length(files), size))
  if (!dry_run) {
    file.remove(files)
    orderly_log("clean", "...files deleted")
  }
  invisible()
}
