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

orderly_runner_tbl <- "orderly_runner"

## TODO: through here we need to wrap some calls up in success/fail so
## that I can get that pushed back through the API.
R6_orderly_runner <- R6::R6Class(
  "orderly_runner",
  public = list(
    path = NULL,
    config = NULL,
    allow_branch_change = FALSE,

    orderly_bin = NULL,
    process = NULL,

    path_log = NULL,
    path_id = NULL,

    con = NULL,

    initialize = function(path, allow_branch_change) {
      self$path <- path
      self$config <- orderly_config_get(path)
      self$allow_branch_change <- allow_branch_change

      bin <- tempfile()
      dir.create(bin)
      self$orderly_bin <- write_script(bin)

      self$con <- orderly_db("destination", self$config)
      sqlite_init_table(self$con, orderly_runner_tbl, orderly_runner_db_cols())

      self$path_log <- path_runner_log(path)
      self$path_id <- path_runner_id(path)
      dir.create(self$path_log, FALSE, TRUE)
      dir.create(self$path_id, FALSE, TRUE)
    },

    finalize = function() {
      DBI::dbDisconnect(self$con)
    },

    queue = function(name, parameters = NULL, ref = NULL) {
      key <- ids::adjective_animal()
      d <- data_frame(key = key,
                      state = "queued",
                      name = name,
                      parameters = parameters %||% NA_character_,
                      ref = ref %||% NA_character_)
      DBI::dbWriteTable(self$con, orderly_runner_tbl, d, append = TRUE)
      key
    },

    status = function(key, output = FALSE) {
      out <- NULL
      if (identical(key, self$process$key)) {
        state <- "running"
        id <- readlines_if_exists(self$process$id_file, NA_character_)
        if (output) {
          out <- list(stderr = self$process$stderr(),
                      stdout = self$process$stdout())
        }
      } else {
        sql <- sprintf("SELECT state, name, orderly_id from %s WHERE key = $1",
                       orderly_runner_tbl)
        dat <- DBI::dbGetQuery(self$con, sql, key)
        if (nrow(dat) == 0L) {
          state <- "unknown"
          id <- NA_character_
        } else {
          state <- dat$state
          id <- dat$orderly_id
        }
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
      state <- if (ok) "success" else "failure"
      ## First, ensure that things are going to be sensibly set
      ## even if we fail:
      process <- self$process
      self$process <- NULL

      ## Then process logs
      path_log <- file.path(self$path_log, key)
      dir.create(path_log, FALSE, TRUE)
      writeLines(process$stdout(), path_stdout(path_log))
      writeLines(process$stderr(), path_stderr(path_log))

      if (file.exists(process$id_file)) {
        id <- readLines(process$id_file)
        base <- if (state == "success") path_archive else path_draft
        p <- file.path(base(self$path), process$name, id)
        if (file.exists(p)) {
          file.copy(c(path_stdout(path_log), path_stderr(path_log)), p)
        }
      } else {
        id <- NA_character_
      }

      orderly_runner_set_state(self$con, key, state, id)
    },

    .read_logs = function(key) {
      path_log <- file.path(self$path_log, key)
      list(stderr = readlines_if_exists(path_stderr(path_log)),
           stdout = readlines_if_exists(path_stdout(path_log)))
    },

    .run_next = function() {
      sql <- sprintf(
        "SELECT * FROM %s WHERE state = 'queued' ORDER BY id LIMIT 1",
        orderly_runner_tbl)
      dat <- DBI::dbGetQuery(self$con, sql)
      if (nrow(dat) == 0L) {
        return(FALSE)
      }

      orderly_runner_set_state(self$con, dat$key, "running", NA_character_)
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

orderly_runner_db_cols <- function() {
  c("id" = "INTEGER PRIMARY KEY AUTOINCREMENT",
    "key" = "TEXT",
    "state" = "TEXT", # queued, running, success, failure
    "name" = "TEXT",
    "parameters" = "TEXT", # json
    "ref" = "TEXT", # nullable
    "orderly_id" = "TEXT") # nullable
}

orderly_runner_process <- function(root) {
  config <- orderly_config(root)

  con <- orderly_db("destination", config)
  on.exit(DBI::dbDisconnect(con))

  DBI::dbBegin(con)
  sql <- sprintf("SELECT * FROM %s WHERE state = 'queued' ORDER BY id LIMIT 1",
                 orderly_runner_tbl)
  dat <- DBI::dbGetQuery(con, sql)
  if (nrow(dat) > 0L) {
    orderly_runner_set_state(con, dat$key, "running", NA_character_)
    DBI::dbCommit(con)
  } else {
    DBI::dbRollback(con)
    return(NULL)
  }

  ref <- if (is.na(dat$ref)) NULL else jsonlite::fromJSON(dat$ref)
  parameters <- if (is.na(dat$parameters))
                  NULL else jsonlite::fromJSON(dat$parameters)

  id <- NULL
  tryCatch({
    id_file <- tempfile() # to help with failures
    id <- orderly_run(name, parameters, config = config, id_file = id_file)
    orderly_commit(id, config = config)
    sql <- sprintf("UPDATE %s SET state = $1, id = $2 WHERE key = $3",
                   orderly_runner_tbl)
    orderly_runner_set_state(con, dat$key, "success", id)
  }, error = function(e) {
    if (file.exists(id_file)) {
      id <- tryCatch(readLines(id_file), function(e) NA_character_)
    } else {
      id <- NA_character_
    }
    orderly_runner_set_state(con, dat$key, "error", id)
  })

  dat$key
}

orderly_runner_set_state <- function(con, key, state, id) {
  sql <- sprintf("UPDATE %s SET state = $1, orderly_id = $2 WHERE key = $3",
                 orderly_runner_tbl)
  DBI::dbExecute(con, sql, list(state, id, key))
}

process <- function(command, args) {
  px <- processx::process$new(command, args, stdout = "|", stderr = "|")
  list(px = px,
       key = key,
       stdout = process_stream(px, TRUE),
       stderr = process_stream(px, FALSE))
}
