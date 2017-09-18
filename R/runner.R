##' An orderly runner.  This is used to run reports remotely.  It's
##' designed to be used in conjunction with montagu-reporting-api, so
##' there is no "draft" stage.
##'
##' @title Orderly runner
##' @param path Path to use
##' @param allow_branch_change Allow git to change branches
##' @export
orderly_runner <- function(path, allow_branch_change = FALSE) {
  R6_orderly_runner$new(path, allow_branch_change)
}

RUNNER_QUEUED  <- "queued"
RUNNER_RUNNING <- "running"
RUNNER_SUCCESS <- "success"
RUNNER_ERROR   <- "error"
RUNNER_UNKNOWN <- "unknown"

## TODO: through here we need to wrap some calls up in success/fail so
## that I can get that pushed back through the API.
R6_orderly_runner <- R6::R6Class(
  "orderly_runner",
  cloneable = FALSE,
  public = list(
    path = NULL,
    config = NULL,
    allow_branch_change = FALSE,

    orderly_bin = NULL,
    process = NULL,

    path_log = NULL,
    path_id = NULL,

    con = NULL,
    data = NULL,

    initialize = function(path, allow_branch_change) {
      self$path <- path
      self$config <- orderly_config_get(path)
      self$allow_branch_change <- allow_branch_change

      bin <- tempfile()
      dir.create(bin)
      self$orderly_bin <- write_script(bin)

      self$data <- runner_queue()

      self$path_log <- path_runner_log(path)
      self$path_id <- path_runner_id(path)
      dir.create(self$path_log, FALSE, TRUE)
      dir.create(self$path_id, FALSE, TRUE)
    },

    queue = function(name, parameters = NULL, ref = NULL) {
      key <- self$data$insert(name, parameters, ref)
      orderly_log("queue", sprintf("%s (%s)", key, name))
      key
    },

    status = function(key, output = FALSE) {
      out <- NULL
      if (identical(key, self$process$key)) {
        state <- RUNNER_RUNNING
        id <- readlines_if_exists(self$process$id_file, NA_character_)
        if (output) {
          out <- list(stderr = self$process$stderr(),
                      stdout = self$process$stdout())
        }
      } else {
        d <- self$data$status(key)
        state <- d$state
        id <- d$id
        if (output && !is.na(id)) {
          out <- self$.read_logs(key)
        }
      }
      list(key = key, status = state, id = id, output = out)
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

    cleanup = function(name = NULL, draft = TRUE, data = TRUE,
                       failed_only = FALSE) {
      orderly_cleanup(name = name, config = self$config, draft = draft,
                      data = data, failed_only = failed_only)
    },

    poll = function() {
      if (!is.null(self$process)) {
        if (self$process$px$is_alive()) {
          "running"
        } else {
          self$.cleanup()
          "finish"
        }
      } else if (self$.run_next()) {
        "create"
      } else {
        "idle"
      }
    },

    .cleanup = function() {
      ok <- self$process$px$get_exit_status() == 0L
      key <- self$process$key
      state <- if (ok) RUNNER_SUCCESS else RUNNER_ERROR
      ## First, ensure that things are going to be sensibly set
      ## even if we fail:
      process <- self$process
      self$process <- NULL
      orderly_log(state, sprintf("%s (%s)", process$key, process$name))

      ## Then process logs
      path_log <- file.path(self$path_log, key)
      dir.create(path_log, FALSE, TRUE)
      writeLines(process$stdout(), path_stdout(path_log))
      writeLines(process$stderr(), path_stderr(path_log))

      if (file.exists(process$id_file)) {
        id <- readLines(process$id_file)
        base <- if (state == RUNNER_SUCCESS) path_archive else path_draft
        p <- file.path(base(self$path), process$name, id)
        if (file.exists(p)) {
          file.copy(c(path_stdout(path_log), path_stderr(path_log)), p)
        }
      } else {
        id <- NA_character_
      }

      self$data$set_state(key, state, id)
    },

    .read_logs = function(key) {
      path_log <- file.path(self$path_log, key)
      list(stderr = readlines_if_exists(path_stderr(path_log)),
           stdout = readlines_if_exists(path_stdout(path_log)))
    },

    .run_next = function() {
      dat <- self$data$next_queued()
      if (is.null(dat)) {
        return(FALSE)
      }
      orderly_log("run", sprintf("%s (%s)", dat$key, dat$name))
      self$data$set_state(dat$key, RUNNER_RUNNING)
      id_file <- file.path(self$path_id, dat$key)
      args <- c("--root", self$path,
                "run", dat$name, "--print-log", "--id-file", id_file,
                if (!is.na(dat$parameters)) c("--parameters", dat$parameters),
                if (!is.na(dat$ref)) c("--ref", dat$ref))
      px <- processx::process$new(self$orderly_bin, args,
                                  stdout = "|", stderr = "|")
      self$process <- list(px = px,
                           key = dat$key,
                           name = dat$name,
                           id_file = id_file,
                           stdout = process_stream(px, TRUE),
                           stderr = process_stream(px, FALSE))
      TRUE
    }
  ))

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

runner_queue <- function() {
  cols <- c("key", "state", "name", "parameters", "ref", "id")
  data <- matrix(character(0), 0, length(cols))
  colnames(data) <- cols

  list(
    get = function() {
      data
    },

    length = function() {
      sum(data[, "state"] == RUNNER_QUEUED)
    },

    insert = function(name, parameters = NULL, ref = NULL) {
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
      data <<- rbind(data, new, deparse.level = 0)
      key
    },

    next_queued = function() {
      i <- data[, "state"] == RUNNER_QUEUED
      if (any(i)) {
        i <- which(i)[[1L]]
        as.list(data[i, ])
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
