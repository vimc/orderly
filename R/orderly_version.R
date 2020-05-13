orderly_version <- R6::R6Class(
  "orderly_version",

  ## Start public for now
  public = list(
    config = NULL,
    name = NULL,
    recipe = NULL,

    id = NULL,
    batch_id = NULL,
    workdir = NULL,

    envvar = NULL,
    git = NULL,

    envir = NULL,
    data = NULL,
    changelog = NULL,
    tags = NULL,
    parameters = NULL,
    instance = NULL,
    inputs = NULL,

    preflight_info = NULL,
    postflight_info = NULL,
    time = NULL,

    initialize = function(name, root, locate) {
      self$config <- orderly_config_get(root, locate)
      self$envvar <- orderly_envir_read(self$config$root)
      self$name <- name
    },

    ## Here's the all-in-one
    run = function(parameters = NULL, instance = NULL, envir = NULL,
                   message = NULL, tags = NULL, echo = TRUE,
                   use_draft = FALSE, remote = NULL) {
      self$run_read(parameters, instance, envir, tags, use_draft,
                    remote)
      self$run_prepare(message)
      self$run_execute(echo)
      self$run_cleanup()
    },

    ## And the one for use non-interactively
    run_internal = function(parameters = NULL, instance = NULL, envir = NULL,
                            message = NULL, tags = NULL, echo = TRUE,
                            use_draft = FALSE, remote = NULL,
                            ## These might move around a bit
                            id_file = NULL, batch_id = NULL,
                            ref = NULL, fetch = FALSE, capture_log = FALSE) {
      logfile <- tempfile()
      ## TODO: this does not properly capture errors.
      conditional_capture_log(capture_log, logfile, {
        git_restore <- self$git_checkout(ref, fetch)
        tryCatch({
          self$run_read(parameters, instance, envir, tags, use_draft,
                        remote)
          self$run_prepare(message, id_file)
        }, finally = git_restore())
        if (capture_log) {
          on.exit(file_copy(logfile, file.path(self$workdir, "orderly.log")))
        }
        self$batch_id <- batch_id
        self$run_execute(echo)
        self$run_cleanup()
      })
    },

    ## Implementation for orderly_develop_start
    develop_start = function(parameters = NULL, instance = NULL, envir = NULL,
                             use_draft = FALSE, remote = NULL) {
      self$run_read(parameters, instance, envir, NULL,
                    use_draft, remote, TRUE)
      self$workdir <- self$recipe$path
      withr::with_envvar(self$envvar, {
        withr::with_dir(self$workdir, {
          recipe_copy_global(self$recipe, self$config)
          recipe_copy_depends(self$recipe)
          self$prepare_environment()
        })
      })
      sys_setenv(self$envvar)
    },

    ## The next bit are the basic "phases" - we'll probably tweak
    ## these over time to find the right gaps
    run_read = function(parameters, instance, envir, tags,
                        use_draft, remote, develop = FALSE) {
      loc <- orderly_develop_location(self$name, self$config, FALSE)
      self$recipe <- orderly_recipe$new(loc$name, loc$config,
                                        develop = develop)
      orderly_log("name", self$recipe$name)
      self$instance <- instance
      self$envir <- orderly_environment(envir)
      self$parameters <- recipe_parameters(self$recipe, parameters)
      self$recipe$resolve_dependencies(use_draft, parameters, remote)
      self$tags <- union(self$recipe$tags,
                         recipe_validate_tags(tags, self$config, NULL))
    },

    run_prepare = function(message = NULL, id_file = NULL) {
      self$create(id_file)
      self$create_workdir()
      ## This feels more like something to do in the read section, but
      ## we need the id for this to work properly.
      self$changelog <- changelog_load(
        self$recipe$name, self$id, self$recipe$changelog$contents,
        message, self$config)
      self$preflight()
    },

    run_execute = function(echo = TRUE) {
      self$set_current()
      on.exit(recipe_current_run_clear(), add = TRUE)

      withr::with_envvar(self$envvar, {
        withr::with_dir(self$workdir, {
          self$prepare_environment()
          source(self$recipe$script, local = self$envir, # nolint
                 echo = echo, max.deparse.length = Inf)
        })
      })
    },

    run_cleanup = function() {
      self$postflight()
      self$write_orderly_run_rds()
    },

    commit = function(capture_log, ...) {
      logfile <- file.path(path_draft(self$config$root),
                           self$name, self$id, "orderly.log")
      conditional_capture_log(
        capture_log, logfile,
        orderly_commit(self$id, root = self$config, locate = FALSE, ...))
      path_rds <- path_orderly_run_rds(
        file.path(self$config$root, "archive", self$name, self$id))
      post_success(readRDS(path_rds), self$config)
    },

    set_current = function(test = FALSE) {
      d <- list(id = self$id,
                name = self$name,
                root = self$config$root,
                depends = self$recipe$depends)
      recipe_current_run_set(d, self$workdir, test)
    },

    git_checkout = function(ref, fetch) {
      if (is.null(ref)) {
        return(function() NULL)
      }
      if (fetch) {
        git_fetch(self$config$root)
      }
      prev <- git_detach_head_at_ref(ref, self$config$root)
      function() git_checkout_branch(prev, TRUE, self$config$root)
    },

    create = function(id_file) {
      self$id <- new_report_id()
      orderly_log("id", self$id)
      if (!is.null(id_file)) {
        orderly_log("id_file", id_file)
        writelines_atomic(self$id, id_file)
      }
    },

    ## This needs to be a private function as it assumes we're in the
    ## correct directory...
    copy_files = function() {
      src <- file.path(path_src(self$config$root), self$recipe$name)
      recipe <- self$recipe

      file_copy(file.path(src, "orderly.yml"), "orderly.yml")
      recipe_copy_script(recipe, src)
      recipe_copy_readme(recipe, src)
      recipe_copy_sources(recipe, src)
      recipe_copy_global(recipe, self$config)
      recipe_copy_resources(recipe, src)
      recipe_copy_depends(recipe)

      self$inputs <- recipe$inputs()

      recipe
    },

    create_workdir = function() {
      workdir <- file.path(path_draft(self$config$root),
                           self$recipe$name, self$id)
      dir_create(workdir)
      self$workdir <- workdir
      withr::with_dir(self$workdir, self$copy_files())
    },

    prepare_environment_parameters = function() {
      if (!is.null(self$parameters)) {
        list2env(self$parameters, self$envir)
        recipe_substitute(self$recipe, self$parameters)
      }
    },

    prepare_environment_secrets = function() {
      if (!is.null(self$recipe$secrets)) {
        secrets <- resolve_secrets(self$recipe$secrets, self$config)
        list2env(secrets, self$envir)
      }
    },

    prepare_environment_environment = function() {
      if (!is.null(self$recipe$environment)) {
        env_vars <- lapply(names(self$recipe$environment), function(name) {
          sys_getenv(self$recipe$environment[[name]],
                     sprintf("orderly.yml:environment:%s", name))
        })
        names(env_vars) <- names(self$recipe$environment)
        list2env(env_vars, self$envir)
      }
    },

    prepare_environment_data = function() {
      if (length(self$recipe$data) > 0 || !is.null(self$recipe$connection)) {
        con <- orderly_db("source", self$config, instance = self$instance)
        on.exit(lapply(con, DBI::dbDisconnect))
        self$data <- list()

        self$data$instance <- lapply(con, attr, "instance", exact = TRUE)

        views <- self$recipe$views
        for (v in names(views)) {
          orderly_log("view", sprintf("%s : %s", views[[v]]$database, v))
          sql <- temporary_view(v, views[[v]]$query)
          DBI::dbExecute(con[[views[[v]]$database]], sql)
        }

        data <- list()
        for (v in names(self$recipe$data)) {
          database <- self$recipe$data[[v]]$database
          query <- self$recipe$data[[v]]$query
          withCallingHandlers(
            data[[v]] <- DBI::dbGetQuery(con[[database]], query),
            error = function(e)
              orderly_log("data", sprintf("%s => %s: <error>", database, v)))
          orderly_log("data",
                      sprintf("%s => %s: %s x %s",
                              database, v, nrow(data[[v]]), ncol(data[[v]])))
        }

        if (length(data) > 0L) {
          list2env(data, self$envir)
          self$data$data <- data
        }

        if (!is.null(self$recipe$connection)) {
          self$data$con <- list()
          for (i in names(self$recipe$connection)) {
            self$data$con[[i]] <- con[[self$recipe$connection[[i]]]]
          }
          list2env(self$data$con, self$envir)
          ## Ensure that only unexported connections are closed when
          ## we exit this method (happens above in the on.exit()!)
          con <- con[setdiff(list_to_character(self$recipe$connection, FALSE),
                             names(self$config$database))]
        }
      }
    },

    prepare_environment = function() {
      check_missing_packages(self$recipe$packages)

      self$prepare_environment_parameters()
      self$prepare_environment_secrets()
      self$prepare_environment_environment()
      self$prepare_environment_data()

      for (p in self$recipe$packages) {
        library(p, character.only = TRUE) # nolint
      }
      for (s in self$recipe$sources) {
        source(s, self$envir) # nolint
      }
    },

    preflight = function() {
      self$preflight_info <- list(
        n_dev = length(grDevices::dev.list()),
        n_sink = sink.number(),
        git = git_info(self$config$root),
        random_seed = random_seed(),
        time = Sys.time())
      orderly_log("start", as.character(self$preflight_info$time))
    },

    postflight = function() {
      time <- Sys.time()
      elapsed <- time - self$preflight_info$time
      self$time <- list(start = self$preflight_info$time,
                        end = time,
                        elapsed = time - self$preflight_info$time)
      orderly_log("end", as.character(time))
      orderly_log("elapsed", sprintf("Ran report in %s", format(elapsed)))
      withr::with_dir(self$workdir, {
        recipe_check_device_stack(self$preflight_info$n_dev)
        recipe_check_sink_stack(self$preflight_info$n_sink)
        recipe_check_connections(self$recipe)
      })

      hash_artefacts <-
        withr::with_dir(self$workdir, recipe_check_artefacts(self$recipe))
      ## Ensure that inputs were not modified when the report was run:

      ## TODO: Move this bit of processing into the recipe I think
      artefacts <- self$recipe$artefacts
      artefacts <- data_frame(
        format = list_to_character(artefacts[, "format"], FALSE),
        description = list_to_character(artefacts[, "description"], FALSE),
        order = seq_len(nrow(artefacts)))
      n <- lengths(self$recipe$artefacts[, "filenames"])
      ## TODO: workdir here is terrible
      file_info_artefacts <- data_frame(
        order = rep(seq_along(n), n),
        filename = names(hash_artefacts),
        file_hash = unname(hash_artefacts),
        file_size = file_size(file.path(self$workdir, names(hash_artefacts))))

      ## TODO: make this less weird
      recipe_check_hashes(
        self$inputs,
        withr::with_dir(self$workdir, self$recipe$inputs()),
        "input", "inputs")

      depends <- self$recipe$depends
      if (!is.null(depends)) {
        pre <- data_frame(filename = depends$as, file_hash = depends$hash)
        post <- withr::with_dir(
          self$workdir,
          file_info(pre$filename)[c("filename", "file_hash")])
        recipe_check_hashes(pre, post, "dependency", "dependencies")
        depends <- depends[c("name", "id", "filename", "as", "hash",
                             "id_requested", "is_latest", "is_pinned")]
      }

      ## All the information about data - it's a little more complicated
      ## than the other types of inputs because there are *two* sizes at
      ## present.  We should probably drop the csv one tbh and render to
      ## csv as required?
      if (is.null(self$recipe$data)) {
        data_info <- NULL
      } else {
        con_rds <- orderly_db("rds", self$config, FALSE)
        con_csv <- orderly_db("csv", self$config, FALSE)
        hash_data_csv <- con_csv$mset(self$data$data)
        hash_data_rds <- con_rds$mset(self$data$data)
        data_info <- data_frame(
          name = names(self$recipe$data),
          database = vcapply(self$recipe$data, "[[", "database",
                             USE.NAMES = FALSE),
          query = vcapply(self$recipe$data, "[[", "query", USE.NAMES = FALSE),
          hash = unname(hash_data_rds),
          size_csv = file_size(con_csv$filename(hash_data_rds)),
          size_rds = file_size(con_rds$filename(hash_data_csv)))
      }

      if (is.null(self$recipe$views)) {
        view_info <- view_info <- NULL
      } else {
        view_info <- data_frame(
          name = names(self$recipe$views),
          database = vcapply(self$recipe$views, "[[", "database",
                             USE.NAMES = FALSE),
          query = vcapply(self$recipe$views, "[[", "query", USE.NAMES = FALSE))
      }

      self$postflight_info <- list(
        artefacts = artefacts,
        file_info_artefacts = file_info_artefacts,
        data_info = data_info,
        view_info = view_info)
    },

    metadata = function() {
      recipe <- self$recipe

      ## TODO: should this be done in recipe?
      if (length(recipe$fields) == 0L) {
        extra_fields <- NULL
      } else {
        extra_fields <- as_data_frame(recipe$fields)
      }

      ## TODO: should this be done in recipe?
      if (is.null(recipe$views)) {
        views <- NULL
      } else {
        views <- data_frame(
          name = names(recipe$views),
          database = vcapply(recipe$views, "[[", "database", USE.NAMES = FALSE),
          query = vcapply(recipe$views, "[[", "query", USE.NAMES = FALSE))
      }

      list(id = self$id,
           name = recipe$name,
           parameters = self$parameters,
           date = as.character(Sys.time()),
           displayname = recipe$displayname %||% NA_character_,
           description = recipe$description %||% NA_character_,
           extra_fields = extra_fields,
           connection = !is.null(recipe$connection),
           packages = recipe$packages,
           random_seed = self$preflight_info$random_seed,
           instance = self$data$instance,
           file_info_inputs = self$inputs,
           ## TODO: migration to fix this double handling of artefacts?
           file_info_artefacts = self$postflight_info$file_info_artefacts,
           global_resources = recipe$global_resources,
           artefacts = self$postflight_info$artefacts,
           depends = self$recipe$depends,
           elapsed = as.numeric(self$time$elapsed, "secs"),
           changelog = self$changelog,
           tags = self$tags,
           git = self$preflight_info$git,
           batch_id = self$batch_id,
           data = self$postflight_info$data_info,
           view = self$postflight_info$view_info)
    },

    write_orderly_run_rds = function() {
      session <- withr::with_envvar(self$envvar, session_info())
      session$meta <- self$metadata()
      ## NOTE: git is here twice for some reason
      session$git <- session$meta$git
      session$archive_version <- cache$current_archive_version
      saveRDS(session, path_orderly_run_rds(self$workdir))
    }
  ))
