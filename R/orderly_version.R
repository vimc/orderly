orderly_version <- R6::R6Class(
  "orderly_version",

  private = list(
    config = NULL,
    name = NULL,
    recipe = NULL,

    id = NULL,
    batch_id = NULL,
    workflow_info = NULL,
    workdir = NULL,

    envvar = NULL,
    git = NULL,

    envir = NULL,
    data = NULL,
    secrets = NULL,
    environment = NULL,
    depends = NULL,
    changelog = NULL,
    tags = NULL,
    parameters = NULL,
    instance = NULL,
    inputs = NULL,

    preflight_info = NULL,
    postflight_info = NULL,
    time = NULL,

    git_checkout = function(ref, fetch) {
      if (is.null(ref)) {
        return(function() NULL)
      }
      if (fetch) {
        git_fetch(private$config$root)
      }
      prev <- git_detach_head_at_ref(ref, private$config$root)
      function() git_checkout_branch(prev, TRUE, private$config$root)
    },

    create = function(id_file) {
      private$id <- new_report_id()
      orderly_log("id", private$id)
      if (!is.null(id_file)) {
        orderly_log("id_file", id_file)
        writelines_atomic(private$id, id_file)
      }
    },

    copy_files = function() {
      src <- file.path(path_src(private$config$root), private$recipe$name)
      recipe <- private$recipe

      file_copy(file.path(src, "orderly.yml"), "orderly.yml")
      recipe_copy_script(recipe, src)
      recipe_copy_readme(recipe, src)
      recipe_copy_sources(recipe, src)
      recipe_copy_global(recipe, private$config)
      recipe_copy_resources(recipe, src)
      recipe_copy_depends(private$depends)

      private$inputs <- recipe$inputs()

      recipe
    },

    create_workdir = function() {
      workdir <- file.path(path_draft(private$config$root),
                           private$recipe$name, private$id)
      dir_create(workdir)
      private$workdir <- workdir
      withr::with_dir(private$workdir, private$copy_files())
    },

    ## Fetch external resources
    fetch = function() {
      withr::with_envvar(private$envvar, {
        private$fetch_environment()
        private$fetch_secrets()
        private$fetch_data()
      })
    },

    fetch_environment = function() {
      if (!is.null(private$recipe$environment)) {
        env_vars <- lapply(names(private$recipe$environment), function(name) {
          sys_getenv(private$recipe$environment[[name]],
                     sprintf("orderly.yml:environment:%s", name))
        })
        names(env_vars) <- names(private$recipe$environment)
        private$environment <- env_vars
      }
    },

    fetch_secrets = function() {
      private$secrets <-
        resolve_secrets(private$recipe$secrets, private$config)
    },

    fetch_data = function() {
      uses_db <- length(private$recipe$data) > 0 ||
        !is.null(private$recipe$connection)
      if (!uses_db) {
        return(NULL)
      }

      ret <- list()

      con <- orderly_db("source", private$config,
                        instance = private$instance)
      on.exit(lapply(con, DBI::dbDisconnect))

      ret$instance <- lapply(con, attr, "instance", exact = TRUE)

      views <- private$recipe$views
      for (v in names(views)) {
        orderly_log("view", sprintf("%s : %s", views[[v]]$database, v))
        sql <- temporary_view(v, views[[v]]$query)
        DBI::dbExecute(con[[views[[v]]$database]], sql)
      }

      data <- list()
      for (v in names(private$recipe$data)) {
        database <- private$recipe$data[[v]]$database
        query <- private$recipe$data[[v]]$query
        withCallingHandlers(
          data[[v]] <- DBI::dbGetQuery(con[[database]], query),
          error = function(e)
            orderly_log("data", sprintf("%s => %s: <error>", database, v)))
        orderly_log("data",
                    sprintf("%s => %s: %s x %s",
                            database, v, nrow(data[[v]]), ncol(data[[v]])))
      }

      if (length(data) > 0L) {
        ret$data <- data

        ## All the information about data - it's a little more complicated
        ## than the other types of inputs because there are *two* sizes at
        ## present.  We should probably drop the csv one tbh and render to
        ## csv as required?
        con_rds <- orderly_db("rds", private$config, FALSE)
        con_csv <- orderly_db("csv", private$config, FALSE)
        hash_data_csv <- con_csv$mset(data)
        hash_data_rds <- con_rds$mset(data)
        ret$info <- data_frame(
          name = names(data),
          database = vcapply(private$recipe$data, "[[", "database",
                             USE.NAMES = FALSE),
          query = vcapply(private$recipe$data, "[[", "query",
                          USE.NAMES = FALSE),
          hash = unname(hash_data_rds),
          size_csv = file_size(con_csv$filename(hash_data_rds)),
          size_rds = file_size(con_rds$filename(hash_data_csv)))
      }

      if (!is.null(private$recipe$connection)) {
        ret$con <- list()
        for (i in names(private$recipe$connection)) {
          ret$con[[i]] <- con[[private$recipe$connection[[i]]]]
        }
        ## Ensure that only unexported connections are closed when
        ## we exit this method (happens above in the on.exit()!)
        keep <- setdiff(
          list_to_character(private$recipe$connection, FALSE),
          names(private$config$database))
        con <- con[keep]
      }

      private$data <- ret
    },

    prepare_environment_parameters = function() {
      if (!is.null(private$parameters)) {
        list2env(private$parameters, private$envir)
        recipe_substitute(private$recipe, private$parameters)
      }
    },

    prepare_environment_secrets = function() {
      if (!is.null(private$secrets)) {
        list2env(private$secrets, private$envir)
      }
    },

    prepare_environment_environment = function() {
      if (!is.null(private$environment)) {
        list2env(private$environment, private$envir)
      }
    },

    prepare_environment_data = function() {
      if (!is.null(private$data)) {
        if (length(private$data$data) > 0L) {
          list2env(private$data$data, private$envir)
        }
        if (length(private$data$con) > 0L) {
          list2env(private$data$con, private$envir)
        }
      }
    },

    prepare_environment = function() {
      withr::with_envvar(private$envvar, {
        withr::with_dir(private$workdir, {
          check_missing_packages(private$recipe$packages)

          private$prepare_environment_parameters()
          private$prepare_environment_secrets()
          private$prepare_environment_environment()
          private$prepare_environment_data()

          for (p in private$recipe$packages) {
            library(p, character.only = TRUE) # nolint
          }
          for (s in private$recipe$sources) {
            source(s, private$envir) # nolint
          }
        })
      })
    },

    preflight = function() {
      private$preflight_info <- list(
        n_dev = length(grDevices::dev.list()),
        n_sink = sink.number(),
        git = git_info(private$config$root),
        random_seed = random_seed(),
        time = Sys.time())
      orderly_log("start", as.character(private$preflight_info$time))
    },

    postflight = function() {
      time <- Sys.time()
      elapsed <- time - private$preflight_info$time
      private$time <- list(start = private$preflight_info$time,
                           end = time,
                           elapsed = time - private$preflight_info$time)
      orderly_log("end", as.character(time))
      orderly_log("elapsed", sprintf("Ran report in %s", format(elapsed)))
      withr::with_dir(private$workdir, {
        recipe_check_device_stack(private$preflight_info$n_dev)
        recipe_check_sink_stack(private$preflight_info$n_sink)
        recipe_check_connections(private$recipe)
      })

      hash_artefacts <-
        withr::with_dir(private$workdir,
                        recipe_check_artefacts(private$recipe))

      artefacts <- private$recipe$artefacts
      artefacts <- data_frame(
        format = list_to_character(artefacts[, "format"], FALSE),
        description = list_to_character(artefacts[, "description"], FALSE),
        order = seq_len(nrow(artefacts)))
      n <- lengths(private$recipe$artefacts[, "filenames"])
      file_info_artefacts <- data_frame(
        order = rep(seq_along(n), n),
        filename = names(hash_artefacts),
        file_hash = unname(hash_artefacts),
        file_size = file_size(file.path(private$workdir,
                                        names(hash_artefacts))))

      recipe_check_hashes(
        private$inputs,
        withr::with_dir(private$workdir, private$recipe$inputs()),
        "input", "inputs")

      depends <- private$depends
      if (!is.null(depends)) {
        pre <- data_frame(filename = depends$as, file_hash = depends$hash)
        post <- withr::with_dir(
          private$workdir,
          file_info(pre$filename)[c("filename", "file_hash")])
        recipe_check_hashes(pre, post, "dependency", "dependencies")
        depends <- depends[c("name", "id", "filename", "as", "hash",
                             "id_requested", "is_latest", "is_pinned")]
      }

      data_info <- private$data$info

      if (is.null(private$recipe$views)) {
        view_info <- view_info <- NULL
      } else {
        view_info <- data_frame(
          name = names(private$recipe$views),
          database = vcapply(private$recipe$views, "[[", "database",
                             USE.NAMES = FALSE),
          query = vcapply(private$recipe$views, "[[", "query",
                          USE.NAMES = FALSE))
      }

      private$postflight_info <- list(
        artefacts = artefacts,
        file_info_artefacts = file_info_artefacts,
        data_info = data_info,
        view_info = view_info)
    },

    postflight_failed = function() {
      time <- Sys.time()
      if (is.null(private$time)) {
        elapsed <- time - private$preflight_info$time
        private$time <- list(start = private$preflight_info$time,
                             end = time,
                             elapsed = elapsed)
      }
      orderly_log("failed", as.character(time))
      orderly_log("elapsed", sprintf("Failed report in %s",
                                     format(private$time$elapsed)))
    },

    metadata = function() {
      recipe <- private$recipe

      if (length(recipe$fields) == 0L) {
        extra_fields <- NULL
      } else {
        extra_fields <- as_data_frame(recipe$fields)
      }

      if (is.null(recipe$views)) {
        views <- NULL
      } else {
        views <- data_frame(
          name = names(recipe$views),
          database = vcapply(recipe$views, "[[", "database",
                             USE.NAMES = FALSE),
          query = vcapply(recipe$views, "[[", "query", USE.NAMES = FALSE))
      }

      list(id = private$id,
           name = recipe$name,
           parameters = private$parameters,
           date = as.character(Sys.time()),
           displayname = recipe$displayname %||% NA_character_,
           description = recipe$description %||% NA_character_,
           extra_fields = extra_fields,
           connection = !is.null(recipe$connection),
           packages = recipe$packages,
           random_seed = private$preflight_info$random_seed,
           instance = private$data$instance,
           file_info_inputs = private$inputs,
           ## TODO (VIMC-3843): migration to fix this double handling
           ## of artefacts
           file_info_artefacts = private$postflight_info$file_info_artefacts,
           global_resources = recipe$global_resources,
           artefacts = private$postflight_info$artefacts,
           depends = private$depends,
           elapsed = as.numeric(private$time$elapsed, "secs"),
           changelog = private$changelog,
           tags = private$tags,
           git = private$preflight_info$git,
           batch_id = private$batch_id,
           workflow = private$workflow_info,
           data = private$postflight_info$data_info,
           view = private$postflight_info$view_info)
    },

    write_orderly_run_rds = function() {
      session <- withr::with_envvar(private$envvar, session_info())
      session$meta <- private$metadata()
      ## NOTE: git is here twice for some reason
      session$git <- session$meta$git
      session$archive_version <- cache$current_archive_version
      saveRDS(session, path_orderly_run_rds(private$workdir))
    },

    write_orderly_fail_rds = function(error) {
      if (is.null(private$workdir)) {
        message("Can't save fail RDS, workdir not set")
        return(invisible(FALSE))
      }
      session <- withr::with_envvar(private$envvar, session_info())
      trace <- utils::limitedLabels(sys.calls())
      if (length(trace) > 4) {
        ## Remove the last 4 entries, these will always be calls to:
        ## > .handleSimpleError(...
        ## > h(simpleError(msg, call))
        ## > self$run_failed_cleanup(e)
        ## > private$write_orderly_fail_rds(error)
        ## which aren't really interesting
        trace <- trace[seq(1, length(trace) - 4)]
      }
      error$trace <- trace
      session$error <- error
      session$meta <- private$metadata()
      ## NOTE: git is here twice for some reason
      session$git <- session$meta$git
      session$archive_version <- cache$current_archive_version
      saveRDS(session, path_orderly_fail_rds(private$workdir))
    }
  ),

  public = list(
    initialize = function(name, root, locate) {
      private$config <- orderly_config(root, locate)
      private$envvar <- orderly_envir_read(private$config$root)
      private$name <- name
    },

    ## Run a report, as for orderly_run (user-facing)
    run = function(parameters = NULL, instance = NULL, envir = NULL,
                   message = NULL, tags = NULL, echo = TRUE,
                   use_draft = FALSE, remote = NULL) {
      self$run_read(parameters, instance, envir, tags, use_draft,
                    remote)
      withCallingHandlers({
        self$run_prepare(message)
        private$fetch()
        self$run_execute(echo)
        self$run_cleanup()
        private$id
      }, error = function(e) {
        self$run_failed_cleanup(e)
      })
    },

    ## Run a report as for orderly_run_internal (orderly-internal facing)
    run_internal = function(parameters = NULL, instance = NULL, envir = NULL,
                            message = NULL, tags = NULL, echo = TRUE,
                            use_draft = FALSE, remote = NULL,
                            ## These might move around a bit
                            id_file = NULL, batch_id = NULL,
                            workflow_info = NULL, ref = NULL, fetch = FALSE,
                            capture_log = FALSE) {
      logfile <- tempfile()
      capture_log <- capture_log %||%
        private$config$get_run_option("capture_log") %||% FALSE
      withCallingHandlers(
        conditional_capture_log(capture_log, logfile, {
          git_restore <- private$git_checkout(ref, fetch)
          tryCatch({
            self$run_read(parameters, instance, envir, tags, use_draft,
                          remote)
            self$run_prepare(message, id_file)
          }, finally = git_restore())
          if (capture_log) {
            on.exit(file_copy(logfile,
                              file.path(private$workdir, "orderly.log")))
          }
          private$batch_id <- batch_id
          private$workflow_info <- workflow_info
          private$fetch()
          self$run_execute(echo)
          self$run_cleanup()
        }), error = function(e) {
          self$run_failed_cleanup(e)
        })
      private$id
    },

    ## Start an in-src development of a report (orderly_develop_start)
    develop_start = function(parameters = NULL, instance = NULL, envir = NULL,
                             use_draft = FALSE, remote = NULL) {
      self$run_read(parameters, instance, envir, NULL,
                    use_draft, remote, TRUE)
      private$workdir <- private$recipe$path
      withr::with_dir(private$workdir, {
        recipe_copy_global(private$recipe, private$config)
        recipe_copy_depends(private$depends)
      })
      private$fetch()
      private$prepare_environment()
      sys_setenv(private$envvar)
      private$workdir
    },

    ## Start an in-draft development of a report (orderly_test_start,
    ## possibly to be deprecated)
    test_start = function(parameters = NULL, instance = NULL, envir = NULL,
                          use_draft = FALSE, remote = NULL) {
      self$run_read(parameters, instance, envir, NULL, use_draft, remote)
      self$run_prepare()
      private$fetch()
      private$prepare_environment()
      self$set_current(test = TRUE)
      private$workdir
    },

    bundle_pack = function(dest, parameters = NULL, instance = NULL,
                         remote = NULL, tags = NULL) {
      envir <- NULL
      use_draft <- FALSE
      self$run_read(parameters, instance, envir, tags, use_draft, remote)
      if (!is.null(private$recipe$connection)) {
        stop("Cannot use 'connection:' with a bundle")
      }

      self$run_prepare()

      check_missing_packages(private$recipe$packages)
      private$fetch()

      if (file.exists(dest)) {
        assert_is_directory(dest, FALSE)
      }

      path <- private$workdir
      dest_id <- file.path(dest, private$id)
      path_meta <- file.path(dest_id, "meta")
      path_pack <- file.path(dest_id, "pack")
      dir_create(path_meta)

      files <- list.files(private$workdir, recursive = TRUE,
                          all.files = TRUE, no.. = TRUE)
      manifest <- file_info(files, private$workdir)

      ## Collect every 'reasonable' data member of the class (we don't
      ## want methods or any reference objects, nor the workdir which
      ## will be invalid)
      info <- as.list(private)
      info <- info[!vlapply(info, function(x) is.function(x) || R6::is.R6(x))]
      info$workdir <- NULL

      ## TODO(VIMC-3975): Sign the manifest
      saveRDS(private$config, file.path(path_meta, "config.rds"))
      saveRDS(manifest, file.path(path_meta, "manifest.rds"))
      saveRDS(info, file.path(path_meta, "info.rds"))
      saveRDS(session_info(), file.path(path_meta, "session.rds"))
      ## If we're on the same filesystem we could move the file with
      ## fs::file_move or file.rename, but the temporary directory may
      ## easily be in a different filesystem to the work tree.
      fs::dir_copy(private$workdir, path_pack)
      fs::dir_delete(private$workdir)
      zip <- zip_dir(dest_id)

      unlink(dest_id, recursive = TRUE)

      orderly_log("bundle pack", private$id)
      list(id = private$id, path = zip)
    },

    bundle_run = function(recipe, info, echo = TRUE, envir = NULL) {
      private$recipe <- recipe
      private$envir <- orderly_environment(envir)
      private$workdir <- recipe$path
      for (v in names(info)) {
        private[[v]] <- info[[v]]
      }

      withCallingHandlers({
        ## Refetch the preflight info here: we want to keep git but
        ## replace everything else I think.  We might save the random
        ## seed but that is not actually supposed to work across R
        ## versions.
        private$preflight()
        private$preflight_info["git"] <- info$preflight_info["git"]

        self$run_execute(echo)
        self$run_cleanup()
      }, error = function(e) {
        self$run_failed_cleanup(e)
      })
    },

    ## The next bit are the basic "phases" - we'll probably tweak
    ## these over time to find the right gaps

    ## Read phase of running a report - read orderly.yml, check
    ## parameters, tags and dependencies
    run_read = function(parameters, instance, envir, tags,
                        use_draft, remote, develop = FALSE) {
      loc <- orderly_develop_location(private$name, private$config, FALSE)
      private$recipe <- orderly_recipe$new(loc$name, loc$config,
                                           develop = develop)
      orderly_log("name", private$recipe$name)
      private$instance <- instance
      private$envir <- orderly_environment(envir)
      private$parameters <- recipe_parameters(private$recipe, parameters)
      private$tags <- union(private$recipe$tags,
                            recipe_validate_tags(tags, private$config, NULL))
      private$depends <-
        private$recipe$resolve_dependencies(use_draft, private$parameters,
                                            remote)
    },

    ## Prepare phase of a report - create id, load changelog and
    ## prepare files in in the draft directory
    run_prepare = function(message = NULL, id_file = NULL) {
      private$create(id_file)
      private$create_workdir()
      ## This feels more like something to do in the read section, but
      ## we need the id for this to work properly.
      private$changelog <- changelog_load(
        private$recipe$name, private$id, private$recipe$changelog$contents,
        message, private$config)
      recipe_substitute(private$recipe, private$parameters)
      private$preflight()
    },

    ## Execute phase of running a report (load packages, sources and
    ## run the script)
    run_execute = function(echo = TRUE) {
      self$set_current()
      on.exit(recipe_current_run_clear(), add = TRUE)

      private$prepare_environment()
      withr::with_envvar(private$envvar, {
        withr::with_dir(private$workdir, {
          source(private$recipe$script, local = private$envir, # nolint
                 echo = echo, max.deparse.length = Inf)
        })
      })
    },

    ## Cleanup phase of running (verify artefacts and write out metadata)
    run_cleanup = function() {
      private$postflight()
      private$write_orderly_run_rds()
    },

    run_failed_cleanup = function(error) {
      private$postflight_failed()
      private$write_orderly_fail_rds(error)
    },

    ## Commit a report, including reporting back via slack/teams
    commit = function(capture_log, ...) {
      logfile <- file.path(path_draft(private$config$root),
                           private$name, private$id, "orderly.log")
      conditional_capture_log(
        capture_log, logfile,
        orderly_commit(private$id, root = private$config, locate = FALSE, ...))
      path_rds <- path_orderly_run_rds(
        file.path(private$config$root, "archive", private$name, private$id))
      post_success(readRDS(path_rds), private$config)
    },

    ## Register data associated with the currently running (or tested)
    ## report.  This can be retrieved by orderly_run_info() - not yet
    ## implemented for orderly_develop_start see #223/VIMC-3848)
    set_current = function(test = FALSE) {
      d <- list(id = private$id,
                name = private$name,
                root = private$config$root,
                depends = private$depends)
      recipe_current_run_set(d, private$workdir, test)
    }
  ))
