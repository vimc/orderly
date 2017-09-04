exiftool_locate <- function() {
  path <- Sys.which("exiftool")
  if (!nzchar(path)) {
    path <- NULL
  }
  path
}

## Simple version:
exiftool_write_field <- function(filename, field, string) {
  if (!is.null(cache$exiftool)) {
    args <- c(sprintf("-%s=%s", field, shQuote(string)), filename)
    code <- system2(cache$exiftool, args, stderr = FALSE, stdout = FALSE)
    if (code != 0) {
      stop("Error watermarking file!")
    }
  }
}

exiftool_read_field <- function(filename, field) {
  if (is.null(cache$exiftool)) {
    stop("exiftool is not installed; can't read image watermark")
  }
  args <- c("-b", sprintf("-%s", field), filename)
  str <- system2(cache$exiftool, args, stdout = TRUE)
  if (length(str) == 0L) "" else str
}

## Daemon version
exiftool_process_start <- function(must_start = FALSE) {
  if (!is.null(cache$exiftool_process)) {
    if (must_start) {
      stop("exiftool already running")
    }
  } else {
    if (is.null(cache$exiftool)) {
      "exiftool not installed: can't start process"
    }
    orderly_log("exiftool", "starting exiftool process")
    argfile <- tempfile()
    writeLines(character(0), argfile)
    args <- c("-stay_open", "True", "-@", argfile)
    cache$exiftool_process <-
      list(process = processx::process$new(cache$exiftool, args,
                                           stdout = "|", stderr = "|"),
           argfile = argfile)
  }
}

exiftool_process_send_command <- function(args, execute = TRUE, start = TRUE) {
  if (is.null(cache$exiftool_process$argfile)) {
    if (start) {
      exiftool_process_start()
    } else {
      stop("exiftool process not running")
    }
  }
  argfile <- cache$exiftool_process$argfile
  send <- paste0(c(args, if (execute) "-execute"), "\n", collapse = "")
  cat(send, file = argfile, append = TRUE)

  ## Poll for output; this is all a bit nasty:
  output <- character(0)
  p <- cache$exiftool_process$process
  t <- Sys.time() + 1
  while (p$is_alive()) {
    lines <- p$read_output_lines()
    if (length(lines) == 0) {
      if (Sys.time() > t) {
        stop("It's all gone Pete Tong")
      }
      Sys.sleep(0.01)
      next
    }

    done <- lines == "{ready}"
    if (any(done)) {
      if (sum(done) > 1 || which(done) != length(done)) {
        stop("this should not happen...")
      }
      output <- c(output, lines[!done])
      return(output)
    } else {
      output <- c(output, lines)
    }
  }
}

exiftool_process_stop <- function() {
  if (!is.null(cache$exiftool_process)) {
    exiftool_process_send_command(c("-stay_open", "False"), FALSE, FALSE)
  }
}

exiftool_process_write_field <- function(filename, field, string,
                                         start = TRUE) {
  args <- c(sprintf("-%s=%s", field, string), filename)
  exiftool_process_send_command(args, start = start)
}

exiftool_process_read_field <- function(filename, field, string,
                                        start = TRUE) {
  args <- c(sprintf("-%s", field), filename)
  str <- exiftool_process_send_command(args, start = start)
  if (length(str) == 0) {
    ""
  } else {
    sub(sprintf("^%s\\s*:\\s*", field), "", str, ignore.case = TRUE)
  }
}
