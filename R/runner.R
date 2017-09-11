##' An orderly runner.
##'
##' @title Orderly runner
##' @param path Path to use
##'
##' @param timeout Timeout to use while waiting for R subprocess to
##'   start on run
##'
##' @param max_processes The maximum number of concurrent R processes to run.
##'
##' @export
orderly_runner <- function(path, timeout = 2.0, max_processes = 4L) {
  R6_orderly_runner$new(path, timeout, max_processes)
}

R6_orderly_runner <- R6::R6Class(
  "orderly_runner",
  public = list(
    orderly = NULL,
    path = NULL,
    timeout = NULL,
    running = NULL,
    config = NULL,
    max_processes = NULL,

    initialize = function(path, timeout, max_processes) {
      self$path <- path
      self$config <- orderly_config_get(path)
      bin <- tempfile()
      dir.create(bin)
      self$orderly <- write_script(bin)
      self$timeout <- timeout
      self$running <- new.env(parent = emptyenv())
      self$max_processes <- max_processes
    },

    rebuild = function() {
      orderly_rebuild(self$config, FALSE)
    },

    cleanup = function(name = NULL, draft = TRUE, data = TRUE,
                       failed_only = FALSE) {
      orderly_cleanup(name = name, config = self$config, draft = draft,
                      data = data, failed_only = failed_only)
    },

    ## It would be nicer to keep a pool of processes handy and cycle
    ## them through.  So on startup, create a pool of processes that
    ## will wait for a task (perhaps reading a pipe).  On exit we'd
    ## replenish the process pool.
    ##
    ## TODO: max_processes > 1 will interact poorly with anything that
    ## checks out a different branch before use.
    run = function(name, parameters = NULL, commit = TRUE) {
      if (self$n_active() > self$max_processes) {
        stop("too many active processes")
      }

      if (!is.null(parameters)) {
        assert_scalar_character(parameters) # really must be json
        parameters <- c("--parameters", parameters)
      }
      id_file <- tempfile()
      args <- c("--root", self$path, "run", "--print-log", parameters,
                if (commit) NULL else "--no-commit",
                "--id-file", id_file, name)

      px <- processx::process$new(self$orderly, args,
                                  stdout = "|", stderr = "|")

      id <- process_wait(px, id_file, timeout = 1, poll = 0.02)
      key <- sprintf("%s/%s", name, id)
      dat <- list(name = name, id = id, key = key, process = px,
                  stdout = process_stream(px, TRUE),
                  stderr = process_stream(px, FALSE))
      self$running[[key]] <- dat
      id
    },

    status = function(name, id, output = FALSE) {
      key <- sprintf("%s/%s", name, id)
      obj <- self$running[[key]]
      out <- NULL

      if (is.null(obj)) {
        path <- self$.find(key)
        if (is.null(path)) {
          status <- "unknown"
        } else {
          status <- basename(dirname(dirname(path)))
          if (status == "draft") {
            if (!file.exists(path_orderly_run_yml(path))) {
              status <- "error"
            }
          }
          if (output) {
            out <- list(stderr = readlines_if_exists(path_stderr(path)),
                        stdout = readlines_if_exists(path_stdout(path)))
          }
        }
      } else {
        if (obj$process$is_alive()) {
          status <- "running"
          if (output) {
            out <- list(stderr = obj$stderr(),
                        stdout = obj$stdout())
          }
        } else {
          self$.cleanup_key(key)
          return(self$status(name, id, output))
        }
      }

      list(status = status, output = out)
    },

    commit = function(name, id) {
      orderly_commit(id, name, config = self$config)
      TRUE
    },

    publish = function(name, id, value = TRUE) {
      orderly_publish(id, value, name, config = self$config)
      value
    },

    n_active = function() {
      keys <- ls(self$running)
      sum(vlapply(keys, function(k) self$running[[k]]$process$is_alive()))
    },

    ## More stuff that could work it's way into the querying...
    .find = function(key) {
      path <- file.path(path_archive(self$path), key)
      if (!file.exists(path)) {
        path <- file.path(path_draft(self$path), key)
        if (!file.exists(path)) {
          path <- NULL
        }
      }
      path
    },

    .cleanup_key = function(key) {
      obj <- self$running[[key]]
      exists <- !is.null(obj)
      if (exists) {
        id <- basename(key)
        name <- dirname(key)

        dest <- self$.find(key)
        if (!is.null(dest)) {
          writeLines(obj$stdout(), path_stdout(dest))
          writeLines(obj$stderr(), path_stderr(dest))
        }

        rm(list = key, envir = self$running)
      }
      exists
    }
  ))

readlines_if_exists <- function(path, misisng = NULL) {
  if (file.exists(path)) {
    readLines(path)
  } else {
    missing
  }
}

path_stderr <- function(path) {
  file.path(path, "orderly.stderr")
}
path_stdout <- function(path) {
  file.path(path, "orderly.stdout")
}

process_stream <- function(px, output = TRUE) {
  dat <- character(0)
  callback <- if (output) px$read_output_lines else px$read_error_lines
  function() {
    dat <<- c(dat, callback())
    dat
  }
}

process_wait <- function(px, filename, timeout = 1, poll = 0.02) {
  t_stop <- Sys.time() + timeout
  while (px$is_alive()) {
    if (file.exists(filename)) {
      break
    }
    if (Sys.time() > t_stop) {
      stop("did not start in time")
    }
    Sys.sleep(poll)
    message(".", appendLF = FALSE)
  }
  message("started")
  id <- readLines(filename)
}
