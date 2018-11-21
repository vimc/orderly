wait_while <- function(continue, timeout = NULL, poll = 0.02) {
  timeout <- timeout %||% (if (nzchar(Sys.getenv("R_COVR", ""))) 10 else 2)
  t_quit <- Sys.time() + timeout
  while (continue()) {
    Sys.sleep(poll)
    if (Sys.time() > t_quit) {
      stop("Timeout reached")
    }
  }
}
wait_for_path <- function(path, ...) {
  wait_while(function() !file.exists(path), ...)
}
wait_for_process_termination <- function(process, ...) {
  wait_while(function() process$is_alive(), ...)
}

wait_while_running <- function(runner, ...) {
  wait_while(function() runner$poll() == "running")
}
wait_for_id <- function(runner, key, ...) {
  st <- NULL
  continue <- function() {
    st <<- runner$status(key)
    st$status == "running" && is.na(st$id)
  }
  wait_while(continue)
  st$id
}

runner_start <- function(runner, name, ...) {
  key <- runner$queue(name, ...)
  runner$poll()
  id <- wait_for_id(runner, key)
  list(name = name, key = key, id = id)
}

runner_start_interactive <- function(runner) {
  dat <- runner_start(runner, "interactive")
  wait_for_path(file.path(runner$path, "draft", dat$name, dat$id, "started"))
  dat
}

append_lines <- function(text, filename) {
  prev <- readLines(filename)
  writeLines(c(prev, text), filename)
}

Sys.setenv(R_TESTS = "")
