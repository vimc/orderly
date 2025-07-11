##' Download dependent reports from an orderly remote.  This can only
##' be used if the `orderly_config.yml` lists a remote.  This
##' allows for a centralised workflow where a central orderly store
##' exists and holds the canonical copies of reports, from which
##' versions can be downloaded into local stores.
##'
##' The `orderly_pull_archive` function pulls report directly
##' (without it being a dependent report).
##'
##' After setting your username up you can run
##' `orderly_pull_dependencies("reportname")` to pull the
##' *dependencies* of `"reportname"` down so that
##' `"reportname"` can be run, or you can run
##' `orderly_pull_archive("reportname")` to pull a copy of
##' `"reportname"` that has been run on the remote server.
##'
##' Pulling an archive report from a remote also pulls its
##' dependencies (recursively), and adds all of these to the local
##' database.  This may require migrating old orderly archives
##' ([orderly1::orderly_migrate()]).  Note that this migration will
##' likely fail for remote orderly versions older than 0.6.8 because
##' the migration needs to read data files on disk that are not
##' included in the downloaded archive in order to collect all the
##' information required for the database.  In this case, ask the
##' administrator of the remote orderly archive to migrate their
##' archive, and then re-pull.
##'
##' Pushing an archive is possible only if the remote supports it.
##' Currently this is supported by [orderly1::orderly_remote_path()]
##' remotes, though not by orderlyweb remotes.  There is no control
##' over what will *accept* a push at this point, nor any check
##' that what you've pushed is "good" except that it exists in your
##' archive.  As with pulling an archive, pushes are recursive with
##' respect to dependencies.  The configuration interface here will
##' likely change a little over time.
##'
##' @title Download dependent reports
##'
##' @param name Name of the report to download dependencies for.
##'   Alternatively, the default of `NULL` is useful if you have
##'   already set the working directory to be the source directory.
##'
##' @param remote Description of the location.  Typically this is a
##'   character string indicating a remote specified in the
##'   `remotes` block of your `orderly_config.yml`.  It is
##'   also possible to pass in a directly created remote object (e.g.,
##'   using [orderly1::orderly_remote_path()], or one provided by
##'   another package).  If left `NULL`, then the default remote
##'   for this orderly repository is used - by default that is the
##'   first listed remote.
##'
##' @param parameters Parameters to pass through when doing dependency
##'   resolution.  If you are using a query for `id` that
##'   involves a parameter (e.g., `latest(parameter:x == p)`) you
##'   will need to pass in the parameters here.  Similarly, if you are
##'   pulling a report that uses query dependencies that reference
##'   parameters you need to pass them here (the same parameter set
##'   will be passed through to all dependencies).
##'
##' @param recursive Logical, indicating if all dependencies of a
##'   report should also be pulled. Setting this to `FALSE` only
##'   the direct reports, along with metadata for the dependencies;
##'   this will be potentially much faster, but leaves your archive in
##'   a more fragile state.
##'
##' @inheritParams orderly_list
##' @export
##'
##' @return No return value, these functions are called only for their
##'   side effects
##'
##' @seealso [orderly1::orderly_remote_path()], which implements the
##'   remote interface for orderly repositories at a local path.  See
##'   also [OrderlyWeb](https://github.com/vimc/orderly-web) for a
##'   system for hosting orderly repositories over an HTTP API.
##'   `vignette("remote", package = "orderly1")` describes the
##'   remote system in more detail.
##'
##' @example man-roxygen/example-remote.R
orderly_pull_dependencies <- function(name = NULL, root = NULL, locate = TRUE,
                                      remote = NULL, parameters = NULL,
                                      recursive = TRUE) {
  loc <- orderly_develop_location(name, root, locate)
  name <- loc$name
  config <- loc$config
  remote <- get_remote(remote, config)
  depends <- orderly_recipe$new(name, config)$depends

  msg <- sprintf("%s has %d %s", name, NROW(depends),
                 ngettext(NROW(depends), "dependency", "dependencies"))
  orderly_log("depends", msg)

  if (!is.null(depends)) {
    for (i in seq_len(nrow(depends))) {
      if (!isTRUE(depends$draft[[i]])) {
        orderly_pull_archive(depends$name[[i]], depends$id[[i]], config,
                             FALSE, remote, parameters, recursive)
      }
    }
  }
}


##' @export
##' @rdname orderly_pull_dependencies
##'
##' @param id The identifier (for `orderly_pull_archive`).  The default is
##'   to use the latest report.
orderly_pull_archive <- function(name, id = "latest", root = NULL,
                                 locate = TRUE, remote = NULL,
                                 parameters = NULL, recursive = TRUE) {
  info <- pull_info(name, id, root, locate, remote, parameters)
  name <- info$name
  id <- info$id
  config <- info$config
  remote <- info$remote

  dest <- file.path(path_archive(config$root), name, id)
  label <- sprintf("%s:%s", name, id)
  if (file.exists(dest)) {
    orderly_log("pull", sprintf("%s already exists, skipping", label))
  } else {
    orderly_log("pull", label)
    path <- remote$pull(name, id)

    ## There's an assumption here that the dependency resolution here
    ## will not be badly affected by migrations.  If a migration
    ## changes how d$meta$depends is structured (if d$meta$depends
    ## stops being a data.frame that includes name and id as
    ## columns) then we'll need to deal with that when checking
    ## dependencies too.
    d <- readRDS(path_orderly_run_rds(path))
    orderly_pull_resolve_dependencies(d, remote, config, recursive)

    ## Only migrate after dependencies have been resolved because
    ## some migrations do check dependencies.
    migrate_single(path, config)

    withCallingHandlers({
      copy_directory(path, dest)
      report_db_import(name, id, config)
    }, error = function(e) unlink(dest, recursive = TRUE))
  }
}


orderly_pull_metadata <- function(name, id, root = NULL, locate = TRUE,
                                  remote = NULL) {
  parameters <- NULL
  info <- pull_info(name, id, root, locate, remote, parameters)
  name <- info$name
  id <- info$id
  config <- info$config
  remote <- info$remote

  dest_metadata <- file.path(path_metadata(config$root), name, id)
  dest_archive <- file.path(path_archive(config$root), name, id)

  label <- sprintf("%s:%s", name, id)
  if (file.exists(dest_archive) || file.exists(dest_metadata)) {
    orderly_log("metadata", sprintf("%s already exists, skipping", label))
  } else {
    orderly_log("metadata", sprintf("fetching %s", label))
    ## We'll need to make sure that the OW remote fully implements this!
    path <- remote$metadata(name, id)

    meta <- readRDS(path)

    ## Error if the metadata is unusable
    migrate_metadata(meta, config)

    dir_create(dirname(dest_metadata))
    file_copy(path, dest_metadata)
    orderly_pull_resolve_dependencies(meta, remote, config, FALSE)

    report_db_import(name, id, config, metadata = TRUE)
  }
}


##' @rdname orderly_pull_dependencies
##' @export
orderly_push_archive <- function(name, id = "latest", root = NULL,
                                 locate = TRUE, remote = NULL) {
  config <- orderly_config(root, locate)
  config <- check_orderly_archive_version(config)
  remote <- get_remote(remote, config)
  if (!is.function(remote$push)) {
    stop("'push' is not supported by this remote")
  }

  if (id == "latest") {
    id <- orderly_latest(name, config, FALSE)
  }

  v <- remote_report_versions(name, config, FALSE, remote)
  if (id %in% v) {
    orderly_log("push", sprintf("%s:%s already exists, skipping", name, id))
  } else {
    orderly_log("push", sprintf("%s:%s", name, id))
    path <- file.path(config$root, "archive", name, id)
    orderly_push_resolve_dependencies(path, remote, config)
    remote$push(path)
  }
}


##' Run a report on a remote server.  Note that this is only supported
##' for remotes using OrderlyWeb at present.
##'
##' @title Run a report on a remote server
##'
##' @param name Name of the report
##'
##' @param parameters Parameters for the report
##'
##' @param timeout Time to tell the server to wait before killing the
##'   report.
##'
##' @param wait Time to wait for the report to be run; if the report
##'   takes longer than this time to run but `timeout` is longer
##'   it will remain running on the server but we will stop waiting
##'   for it and instead throw an error.
##'
##' @param poll Period to poll the server for results (in seconds)
##'
##' @param open Logical, indicating if the report should be opened in
##'   a browser on completion (if supported by the remote)
##'
##' @param stop_on_error Logical, indicating if we should throw an
##'   error if the report fails.  If you set this to `FALSE` it
##'   will be much easier to debug, but more annoying in scripts.  If
##'   the report times out on the server (i.e., takes longer than
##'   `timeout`) that counts as an error.
##'
##' @param stop_on_timeout Logical, indicating if we should throw an
##'   error if the report takes longer than `wait` seconds to
##'   complete.
##'
##' @param progress Logical, indicating if a progress spinner should
##'   be included.
##'
##' @param ref Optional reference, indicating which branch should be
##'   used.  This cannot be used if the remote has `default_branch_only`
##'   set.
##'
##' @param instance Select instance of the source database to be used,
##'   where multiple instances are configured. Use a single unnamed
##'   character string to indicate an instance to match. Will use
##'   default if NULL.
##'
##' @inheritParams orderly_pull_dependencies
##'
##' @export
##' @return No return value, this function is called only for its side effects
##' @examples
##' path_remote <- orderly1::orderly_example("demo")
##' path_local <- orderly1::orderly_example("demo")
##' remote <- orderly1::orderly_remote_path(path_remote)
##' # Currently, path remotes don't support run
##' try(orderly1::orderly_run_remote(
##'   "minimal", remote = remote, root = path_local))
orderly_run_remote <- function(name, parameters = NULL, ref = NULL,
                               timeout = NULL, wait = 3600, poll = 1,
                               open = TRUE, stop_on_error = TRUE,
                               stop_on_timeout = TRUE,
                               progress = TRUE,
                               root = NULL, locate = TRUE, instance = NULL,
                               remote = NULL) {
  cfg <- orderly_config(root, locate)
  if (!is.null(ref)) {
    ref <- gert::git_commit_id(ref, cfg$root)
  }
  remote <- get_remote(remote, cfg)
  invisible(remote$run(
    name, parameters = parameters, ref = ref,
    timeout = timeout, wait = wait, poll = poll,
    progress = progress, open = open, instance = instance,
    stop_on_error = stop_on_error,
    stop_on_timeout = stop_on_timeout))
}


##' Set and get default remote locations.  Default locations are
##' specific to an orderly repository (based on the path of the
##' repository) so there is no interaction between different orderly
##' projects.
##'
##' @title Set default remote location
##'
##' @param value A string describing a remote, a remote object, or
##'   `NULL` to clear
##'
##' @inheritParams orderly_list
##' @export
##'
##' @return The default remote (for
##'   `orderly_default_remote_get`). The function
##'   `orderly_default_remote_set` is called for its side effects
##'   only.
##'
##' @rdname orderly_default_remote
##' @examples
##' # Same setup as in orderly_remote_path, with a remote orderly:
##' path_remote <- orderly1::orderly_example("demo")
##' id <- orderly1::orderly_run("other", list(nmin = 0),
##'                            root = path_remote, echo = FALSE)
##' orderly1::orderly_commit(id, root = path_remote)
##' id <- orderly1::orderly_run("use_dependency",
##'                            root = path_remote, echo = FALSE)
##' orderly1::orderly_commit(id, root = path_remote)
##'
##' # And a local orderly
##' path_local <- orderly1::orderly_example("demo")
##'
##' # We'll create an object to interact with this remote using
##' # orderly_remote_path.
##' remote <- orderly1::orderly_remote_path(path_remote)
##'
##' # There is no remote set by default:
##' try(orderly1::orderly_default_remote_get(root = path_local))
##'
##' # We can set one:
##' orderly1::orderly_default_remote_set(remote, root = path_local)
##'
##' # and now we can retrieve it:
##' orderly1::orderly_default_remote_get(root = path_local)
##'
##' # Note that this has not affected the other orderly:
##' try(orderly1::orderly_default_remote_get(root = path_remote))
orderly_default_remote_set <- function(value, root = NULL, locate = TRUE) {
  config <- orderly_config(root, locate)

  if (is.null(value)) {
    remote <- NULL
  } else {
    remote <- get_remote(value, config)
  }

  cache$default_remote[[config$root]] <- remote
  invisible(remote)
}


##' @rdname orderly_default_remote
##' @export
orderly_default_remote_get <- function(root = NULL, locate = TRUE) {
  config <- orderly_config(root, locate)
  if (!is.null(cache$default_remote[[config$root]])) {
    return(cache$default_remote[[config$root]])
  }
  if (length(config$remote) > 0L) {
    return(get_remote(names(config$remote)[[1L]], config))
  }
  msg <- paste("default remote has not been set yet:",
               "use 'orderly1::orderly_default_remote_set'")
  stop(msg)
}


##' Get a remote, based on the configuration in
##' `orderly_config.yml` - different remote drivers have
##' different methods, and this function gives you access to these
##' lower-level objects.
##'
##' @title Get a remote
##'
##' @inheritParams orderly_pull_dependencies
##'
##' @return The orderly remote, as described in
##'   `orderly_config.yml` - if no remotes are configured, or if
##'   the requested remote does not exist, an error will be thrown.
##'
##' @seealso [orderly1::orderly_pull_dependencies()] which provides a
##'   higher-level interface to pulling from a remote (including
##'   adding the downloaded archive into your orderly repository), and
##'   see the documentation underlying the orderly remote driver that
##'   your `orderly_config.yml` declares for information about
##'   using that remote.
##'
##' @export
##' @example man-roxygen/example-orderly-remote.R
orderly_remote <- function(remote = NULL, root = NULL, locate = TRUE) {
  config <- orderly_config(root, locate)
  get_remote(remote, config)
}


get_remote <- function(remote, config) {
  if (is.null(remote)) {
    return(orderly_default_remote_get(config))
  }
  if (implements_remote(remote)) {
    return(remote)
  }
  if (!is.character(remote)) {
    stop("Unknown remote type ",
         paste(squote(class(remote)), collapse = " / "),
         call. = FALSE)
  }

  assert_scalar(remote)
  if (remote %in% names(config$remote)) {
    load_remote(remote, config)
  } else if (file.exists(remote)) {
    orderly_remote_path(remote)
  } else {
    stop(sprintf("Unknown remote '%s'", remote), call. = FALSE)
  }
}


load_remote <- function(name, config) {
  remote <- config$remote[[name]]
  hash <- hash_object(remote)

  cached <- cache$remotes[[hash]]
  if (!is.null(cached)) {
    return(cached)
  }

  driver <- getExportedValue(remote$driver[[1L]], remote$driver[[2L]])
  base <- "orderly_config.yml:remote"

  where <- sprintf("%s:%s:args", base, name)
  args <- resolve_secrets(resolve_env(remote$args, where), config)
  value <- do.call(driver, args)

  ## TODO(VIMC-3544): put this in an 'data' or equivalent argument
  ## to the constructor, but that required fixing both drivers.
  attr(value, "slack_url") <-
    resolve_env(remote["slack_url"], error = FALSE)$slack_url
  attr(value, "teams_url") <-
    resolve_env(remote["teams_url"], error = FALSE)$teams_url
  attr(value, "primary") <- isTRUE(remote$primary)

  cache$remotes[[hash]] <- value

  value
}


## For debugging
clear_remote_cache <- function() {
  cache$remotes <- list()
}


## Most of these functions can really shrink now?
remote_report_names <- function(root = NULL, locate = TRUE, remote = NULL) {
  remote <- get_remote(remote, orderly_config(root, locate))
  remote$list_reports()
}


remote_report_versions <- function(name, config = NULL, locate = TRUE,
                                   remote = NULL) {
  remote <- get_remote(remote, orderly_config(config, locate))
  remote$list_versions(name)
}


remote_name <- function(remote) {
  remote$name %||% "(unknown)"
}


## Test to see if something (might) implement our remote interface
implements_remote <- function(x) {
  is.recursive(x) &&
    is.function(x$list_reports) &&
    is.function(x$list_versions) &&
    is.function(x$pull) &&
    is.function(x$run) &&
    is.function(x$url_report) &&
    is.function(x$kill)
}


orderly_pull_resolve_dependencies <- function(d, remote, config, recursive) {
  depends <- d$meta$depends
  if (NROW(depends) > 0L) { # NROW(x) is 0 for x = NULL
    depends <- depends[!duplicated(depends$id), c("name", "id"), drop = FALSE]
    orderly_log("depends",
                paste(sprintf("%s/%s", depends$name, depends$id),
                      collapse = ", "))
    if (recursive) {
      for (i in seq_len(nrow(depends))) {
        orderly_pull_archive(depends$name[[i]], depends$id[[i]], root = config,
                             locate = FALSE, remote = remote)
      }
    } else {
      for (i in seq_len(nrow(depends))) {
        orderly_pull_metadata(depends$name[[i]], depends$id[[i]],
                              root = config, locate = FALSE, remote = remote)
      }
    }
  }
}


orderly_push_resolve_dependencies <- function(path, remote, config) {
  d <- readRDS(path_orderly_run_rds(path))
  depends <- d$meta$depends
  if (NROW(depends) > 0L) { # NROW(x) is 0 for x = NULL
    depends <- depends[!duplicated(depends$id), c("name", "id"), drop = FALSE]
    orderly_log("depends",
                paste(sprintf("%s/%s", depends$name, depends$id),
                      collapse = ", "))
    for (i in seq_len(nrow(depends))) {
      orderly_push_archive(depends$name[[i]], depends$id[[i]], root = config,
                           locate = FALSE, remote = remote)
    }
  }
}


## Where will this really be used?  I have it just in query_search at
## the moment, and it is written just to support that, but are there
## other times where we want this?  Probably working with dependency
## graphs too.
remote_report_update_metadata <- function(name, remote, config) {
  ids <- remote$list_versions(name)
  path_cache <- file.path(path_remote_cache(config$root), name)
  dir.create(path_cache, FALSE, TRUE)
  msg <- setdiff(ids, dir(path_cache))
  for (i in msg) {
    file_copy(remote$metadata(name, i), file.path(path_cache, i))
  }
  data_frame(id = ids, path = file.path(path_cache, ids))
}


##' Pack a bundle on a remote. This is like calling
##' [orderly1::orderly_bundle_pack()] on the remote and can be used to
##' extract a long-running report from a server to run (say) on an HPC
##' system.
##'
##' The workflow here will typically be:
##'
##' 1. Use `orderly_bundle_pack_remote()` to create a local
##'    copy of a bundle, extracted from a remote. Typically this will
##'    be run from the system where the bundle will be run (an HPC
##'    head-node or another powerful computer).
##'
##' 2. Run the bundle using [orderly1::orderly_bundle_run()]
##'
##' 3. Re-import the completed bundle using
##'   `orderly_bundle_import_remote` which sends the zip
##'   file to the remote and adds it to the archive.
##'
##' Typically these commands will *not* be run from the orderly
##' root. However, the `root` argument may still be used to find
##' your remote configuration. Alternatively, if your `remote`
##' argument is an orderly remote (e.g.,
##' [orderly1::orderly_remote_path()], or `orderlyweb`'s
##' `orderlyweb::orderlyweb_remote`) then the `root` and
##' `locate` arguments will be ignored and this command can be
##' run from anywhere. This is the recommended configuration for
##' running on a HPC system.
##'
##' @title Pack and import bundles with remotes
##'
##' @inheritParams orderly_bundle_pack
##'
##' @param remote The remote to pack the bundle from, or import into
##'
##' @param dest Optional path to write bundle to (a directory
##'   name). By default we use the temporary directory and return the
##'   full path to the created file.
##'
##' @export
orderly_bundle_pack_remote <- function(name, parameters = NULL,
                                       instance = NULL,
                                       root = NULL, locate = TRUE,
                                       remote = NULL, dest = tempdir()) {
  remote <- get_remote(remote, orderly_config(root, locate))
  dir_create(dest)
  path <- remote$bundle_pack(name, parameters = parameters,
                             instance = instance)
  if (dest != tempdir()) {
    fs::file_move(path, dest)
    path <- file.path(dest, basename(path))
  }

  path
}


##' @rdname orderly_bundle_pack_remote
##'
##' @param path The path to unpack and import
##'   (a zip file created by `orderly_bundle_run`)
##'
##' @export
orderly_bundle_import_remote <- function(path, root = NULL, locate = TRUE,
                                         remote = NULL) {
  remote <- get_remote(remote, orderly_config(root, locate))
  remote$bundle_import(path)
}

##' Get status of remote queue.
##'
##' Get the status of the remote queue as a list.
##'
##' @inheritParams orderly_pull_dependencies
##'
##' @return List containing details of running and queued reports on the
##' remote queue. Including report name, status and version (where known)
##'
##' @export
orderly_remote_status <- function(root = NULL, locate = TRUE, remote = NULL) {
  remote <- get_remote(remote, orderly_config(root, locate))
  remote$queue_status()
}


pull_info <- function(name, id, root, locate, remote, parameters) {
  loc <- orderly_develop_location(name, root, locate)
  name <- loc$name
  config <- loc$config
  config <- check_orderly_archive_version(config)

  remote <- get_remote(remote, config)

  v <- remote_report_versions(name, config, FALSE, remote)
  if (length(v) == 0L) {
    stop(sprintf("No versions of '%s' were found at '%s'",
                 name, remote_name(remote)), call. = FALSE)
  }

  if (id == "latest") {
    id <- latest_id(v)
  } else if (id_is_query(id)) {
    query <- id
    id <-
      orderly_search(id, name, parameters, root = root, remote = remote)
    if (is.na(id)) {
      msg <- c(
        sprintf("Failed to find suitable version of '%s' with query:", name),
        sprintf("  '%s'", query),
        "and parameters",
        sprintf("  - %s: %s", names(parameters),
                vcapply(parameters, as.character)))
      stop(paste(msg, collapse = "\n"), call. = FALSE)
    }
  } else {
    ## We've been given a direct id so we just need to check that it
    ## exists
    if (!(id %in% v)) {
      ## Confirm that the report does actually exist, working around
      ## VIMC-1281:
      stop(sprintf(
        "Version '%s' of '%s' not found at '%s': valid versions are:\n%s",
        id, name, remote_name(remote),
        paste(sprintf("  - %s", v), collapse = "\n")),
        call. = FALSE)
    }
  }

  list(name = name,
       id = id,
       config = config,
       remote = remote)
}


##' Cancel a report
##'
##' The action will depend on the status of the report:
##'   * queued - report run will be deleted
##'   * running - report run will be cancelled
##'   * complete/errored - no effect
##'
##' @param keys The key or keys for the reports to cancel
##'
##' @inheritParams orderly_pull_dependencies
##'
##' @return List with names as report keys and values are lists containing
##'   * `killed` - boolean TRUE if report successfully cancelled, FALSE
##'   otherwise
##'   * `message` - string detailing reason why cancellation failed
##'
##' @export
orderly_cancel_remote <- function(keys, root = NULL, locate = TRUE,
                                  remote = NULL) {
  remote <- get_remote(remote, orderly_config(root, locate))
  out <- lapply(keys, remote$kill)
  names(out) <- keys
  out
}
