##' An orderly runner.  This is used to run reports remotely.  It's
##' designed to be used in conjunction with montagu-reporting-api, so
##' there is no "draft" stage.
##'
##' @title Orderly runner
##' @param path Path to use
##' @param allow_ref Allow git to change branches/ref for run
##' @export
orderly_runner <- function(path, allow_ref = TRUE) {
  R6_orderly_runner$new(path, allow_ref)
}

RUNNER_QUEUED  <- "queued"
RUNNER_RUNNING <- "running"
RUNNER_SUCCESS <- "success"
RUNNER_ERROR   <- "error"
RUNNER_KILLED  <- "killed"
RUNNER_UNKNOWN <- "unknown"

## TODO: through here we need to wrap some calls up in success/fail so
## that I can get that pushed back through the API.
R6_orderly_runner <- R6::R6Class(
  "orderly_runner",
  cloneable = FALSE,
  public = list(
    path = NULL,
    config = NULL,
    allow_ref = FALSE,

    orderly_bin = NULL,
    process = NULL,

    path_log = NULL,
    path_id = NULL,

    con = NULL,
    data = NULL,
    has_git = NULL,

    initialize = function(path, allow_ref) {
      self$path <- path
      self$config <- orderly_config_get(path)
      self$has_git <- file.exists(file.path(path, ".git"))
      if (!self$has_git) {
        message("Not enabling git features as this is not version controlled")
      }
      self$allow_ref <- allow_ref && self$has_git &&
        git_run(c("rev-parse", "HEAD"), root = path, check = FALSE)$success

      bin <- tempfile()
      dir.create(bin)
      self$orderly_bin <- write_script(bin)

      ## This ensures that the index will be present, which will be
      ## useful if something else wants to access the database!
      DBI::dbDisconnect(orderly_db("destination", self$config, FALSE))

      self$data <- runner_queue()

      self$path_log <- path_runner_log(path)
      self$path_id <- path_runner_id(path)
      dir.create(self$path_log, FALSE, TRUE)
      dir.create(self$path_id, FALSE, TRUE)
    },

    queue = function(name, parameters = NULL, ref = NULL, update = FALSE,
                     timeout = 600) {
      if (!self$allow_ref && !is.null(ref)) {
        stop("Reference switching is disabled in this runner")
      }
      if (update && self$has_git) {
        if (is.null(ref)) {
          self$git_pull()
        } else {
          self$git_fetch()
        }
      }
      if (!is.null(ref) && !git_ref_exists(ref, self$path)) {
        stop(sprintf("Did not find git reference '%s'", ref))
        ## Lock down the reference at this point in time (so that
        ## subsequent builds will not affect where we find the source).
        ref <- git_ref_to_sha(ref)
      }
      assert_scalar_numeric(timeout)
      key <- self$data$insert(name, parameters, ref, timeout)
      orderly_log("queue", sprintf("%s (%s)", key, name))
      key
    },

    status = function(key, output = FALSE) {
      out <- NULL
      if (identical(key, self$process$key)) {
        state <- RUNNER_RUNNING
        id <- readlines_if_exists(self$process$id_file, NA_character_)
      } else {
        d <- self$data$status(key)
        state <- d$state
        id <- d$id
      }
      ## TODO: This should move into a separate field but that
      ## requires getting changes through the reporting api.  We'll do
      ## that in a second pass and move the data from here to that
      ## field.
      if (state == "queued") {
        queue <- self$data$get()
        i <- (queue[, "state"] %in% c(RUNNER_QUEUED, RUNNER_RUNNING)) &
          seq_len(nrow(queue)) < which(queue[, "key"] == key)
        stdout <- paste(queue[i, "state"], queue[i, "key"], queue[i, "name"],
                        sep = ":")
        out <- list(stdout = stdout, stderr = NULL)
      } else if (output) {
        out <- self$.read_logs(key)
      }
      list(key = key, status = state, id = id, output = out)
    },

    queue_status = function(output = FALSE, limit = 50) {
      queue <- tail(self$data$get_df(), limit)
      if (is.null(self$process)) {
        status <- "idle"
        current <- NULL
      } else {
        status <- "running"

        current <- self$process[c("key", "name", "start_at", "kill_at")]
        now <- Sys.time()
        current$elapsed <- as.numeric(now - current$start_at, "secs")
        current$remaining <- as.numeric(current$kill_at - now, "secs")
        if (output) {
          current$output <- self$.read_logs(current$key)
        }
      }
      list(status = status, queue = queue, current = current)
    },

    ## This one could quite easily move into the montagu api; it
    ## doesn't take too much to do (write one file and write to the
    ## SQL database)
    publish = function(name, id, value = TRUE) {
      orderly_publish(id, value, name, config = self$config)
      value
    },

    rebuild = function() {
      orderly_rebuild(self$config, FALSE)
    },

    kill = function(key) {
      current <- self$process$key
      if (identical(key, current)) {
        self$.kill_current()
      } else if (is.null(current)) {
        stop(sprintf("Can't kill '%s' - not currently running a report", key))
      } else {
        stop(sprintf("Can't kill '%s' - currently running '%s'", key, current))
      }
    },

    git_status = function() {
      ret <- git_status(self$path)
      ret$branch <- git_branch_name(self$path)
      ret$hash <- git_ref_to_sha("HEAD", self$path)
      ret
    },

    git_fetch = function() {
      res <- git_fetch(self$path)
      if (length(res$output) > 0L) {
        orderly_log("fetch", res$output)
      }
      invisible(res)
    },

    git_pull = function() {
      res <- git_pull(self$path)
      if (length(res$output) > 0L) {
        orderly_log("pull", res$output)
      }
      invisible(res)
    },

    cleanup = function(name = NULL, draft = TRUE, data = TRUE,
                       failed_only = FALSE) {
      orderly_cleanup(name = name, config = self$config, draft = draft,
                      data = data, failed_only = failed_only)
    },

    poll = function() {
      key <- self$process$key
      if (!is.null(self$process)) {
        if (self$process$px$is_alive()) {
          if (Sys.time() > self$process$kill_at) {
            self$.kill_current()
            ret <- "timeout"
          } else {
            ret <- "running"
          }
        } else {
          self$.cleanup()
          ret <- "finish"
        }
      } else if (self$.run_next()) {
        ret <- "create"
        key <- self$process$key
      } else {
        ret <-"idle"
      }
      attr(ret, "key") <- key
      ret
    },

    .cleanup = function(state = NULL) {
      ok <- self$process$px$get_exit_status() == 0L
      key <- self$process$key
      if (is.null(state)) {
        state <- if (ok) RUNNER_SUCCESS else RUNNER_ERROR
      }
      ## First, ensure that things are going to be sensibly set even
      ## if we fail:
      process <- self$process
      self$process <- NULL
      process$px <- NULL

      ## Force cleanup of the process so that the I/O completes
      gc()

      orderly_log(state, sprintf("%s (%s)", process$key, process$name))

      if (file.exists(process$id_file)) {
        id <- readLines(process$id_file)
        base <- if (state == RUNNER_SUCCESS) path_archive else path_draft
        p <- file.path(base(self$path), process$name, id)
        if (file.exists(p)) {
          file_copy(process$stderr, file.path(p, "orderly.log"))
          ## This should be empty if the redirection works as expected
          if (file.size(process$stdout) > 0L) {
            file_copy(process$stdout, file.path(p, "orderly.log.stdout"))
          }
        }
      } else {
        id <- NA_character_
      }

      self$data$set_state(key, state, id)
    },

    .kill_current = function() {
      p <- self$process
      orderly_log("kill", p$key)
      ret <- p$px$kill()
      self$.cleanup(RUNNER_KILLED)
      ret
    },

    .read_logs = function(key) {
      list(stderr = readlines_if_exists(path_stderr(self$path_log, key)),
           stdout = readlines_if_exists(path_stdout(self$path_log, key)))
    },

    .run_next = function() {
      dat <- self$data$next_queued()
      if (is.null(dat)) {
        return(FALSE)
      }
      key <- dat$key
      orderly_log("run", sprintf("%s (%s)", key, dat$name))
      self$data$set_state(key, RUNNER_RUNNING)
      id_file <- file.path(self$path_id, key)
      args <- c("--root", self$path,
                "run", dat$name, "--print-log", "--id-file", id_file,
                if (!is.na(dat$parameters)) c("--parameters", dat$parameters),
                if (!is.na(dat$ref)) c("--ref", dat$ref))

      ## NOTE: sending stdout/stderr to "|" causes a big slowdown (as
      ## in 5-10x longer to run than not going through processx). File
      ## output seems not to have the same problem but if it does this
      ## can be swapped in very easily for
      ##
      ##   px <- sys::exec_background(self$orderly_bin, args,
      ##                              std_out = log_out, std_err = log_err)
      ##
      ## which just returns a PID (rather than an R6 object).
      ##
      ## The only other (non-test) place that needs updating is
      ## px$is_alive() and px$get_exit_status() become
      ## sys::exec_status(px, FALSE)
      ##
      ## There is also one test case that needs tweaking.
      log_out <- path_stdout(self$path_log, key)
      log_err <- path_stderr(self$path_log, key)
      px <- processx::process$new(self$orderly_bin, args,
                                  stdout = log_out, stderr = log_err)
      start_at <- Sys.time()
      self$process <- list(px = px,
                           key = key,
                           name = dat$name,
                           start_at = start_at,
                           kill_at = start_at + dat$timeout,
                           id_file = id_file,
                           stdout = log_out,
                           stderr = log_err)
      TRUE
    }
  ))

path_stderr <- function(path, key) {
  file.path(path, paste0(key, ".stderr"))
}
path_stdout <- function(path, key) {
  file.path(path, paste0(key, ".stdout"))
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

runner_queue <- function() {
  cols <- c("key", "state", "name", "parameters", "ref", "id", "timeout")
  data <- matrix(character(0), 0, length(cols))
  colnames(data) <- cols

  list(
    get = function() {
      data
    },

    get_df = function() {
      ret <- as.data.frame(data, stringsAsFactors = FALSE)
      ret$timeout <- as.numeric(ret$timeout)
      ret
    },

    length = function() {
      sum(data[, "state"] == RUNNER_QUEUED)
    },

    insert = function(name, parameters = NULL, ref = NULL, timeout = 600) {
      existing <- data[, "key"]
      repeat {
        key <- ids::adjective_animal()
        if (!(key %in% existing)) {
          break
        }
      }
      new <- data[NA_integer_, , drop = TRUE]
      new[["key"]] <- key
      new[["name"]] <- name
      new[["state"]] <- RUNNER_QUEUED
      new[["parameters"]] <- parameters %||% NA_character_
      new[["ref"]] <- ref %||% NA_character_
      new[["timeout"]] <- timeout
      data <<- rbind(data, new, deparse.level = 0)
      key
    },

    next_queued = function() {
      i <- data[, "state"] == RUNNER_QUEUED
      if (any(i)) {
        i <- which(i)[[1L]]
        ret <- as.list(data[i, ])
        ret$timeout <- as.numeric(ret$timeout)
        ret
      } else {
        NULL
      }
    },

    status = function(key) {
      d <- data[data[, "key"] == key, , drop = FALSE]
      if (nrow(d) == 0L) {
        list(state = RUNNER_UNKNOWN, id = NA_character_)
      } else {
        d <- d[1L, ]
        list(state = d[["state"]], id = d[["id"]])
      }
    },

    set_state = function(key, state, id = NULL) {
      i <- data[, "key"] == key
      if (any(i)) {
        data[i, "state"] <<- state
        if (!is.null(id)) {
          data[i, "id"] <<- id
        }
        TRUE
      } else {
        FALSE
      }
    })
}
