## WARNING: This whole bit of code is a bit of a hot mess.  The idea
## is to generate a set of data for use with the automated testing for
## the API.  For that we want to simulate a set of orderly runs in
## different states.  I want some changes to code and some examples of
## different types of reports that *could* be generated.  This means
## that it's something that will grow over time.
##
## To be particularly complicated, I also want this to match the data
## format that we use in montagu, so have to set some additional
## fields through which requires tweaking some of the examples.  It's
## all a bit nasty really.

read_demo_yml <- function(path) {
  e <- new.env(parent = parent.env(.GlobalEnv))
  before <- file.path(path, "before.R")
  if (file.exists(before)) {
    sys.source(before, e)
  }

  dat <- yaml_read(file.path(path, "demo.yml"))
  ## Push time back so that we don't end up with time in the future
  day <- 60 * 60 * 24
  t <- Sys.time() - 365 * day
  for (i in seq_along(dat)) {
    name <- sprintf("demo.yml[%d]", i)
    check_fields(dat[[i]], name, "name", c("publish", "before", "parameters"))
    dat[[i]]$time <- t

    b <- dat[[i]]$before
    if (!is.null(b)) {
      if (exists(b, e, mode = "function", inherits = FALSE)) {
        dat[[i]]$before <- get(b, e, mode = "function", inherits = FALSE)
      } else {
        stop(sprintf("function %s not found in before.R (for %s)", b, name))
      }
    }
    dat[[i]]$i <- i

    t <- t + runif(1, 0.1 * day, 5 * day)
  }
  dat
}

demo_change_time <- function(id, time, path) {
  id_new <- new_report_id(time)
  name <- orderly_find_name(id, path)
  date <- sprintf("date: %s", as.character(time))
  p <- file.path(path_draft(path), name, id_new)
  stopifnot(file.rename(file.path(path_draft(path), name, id), p))

  yml <- path_orderly_run_yml(p)
  dat <- yaml_read(yml)
  dat$date <- as.character(time)
  dat$id <- id_new
  writeLines(yaml::as.yaml(dat), yml)

  orderly_commit(id_new, name)

  id_new
}

create_orderly_demo <- function(path = tempfile()) {
  prepare_orderly_example("demo", path)

  dat <- read_demo_yml(path)
  config <- orderly_config(path)
  orderly_default_config_set(config)
  on.exit(orderly_default_config_set(NULL))

  f <- function(x) {
    if (x$i > 1) {
      orderly_log_break()
    }
    orderly_log("demo", sprintf("%d / %d", x$i, length(dat)))
    if (!is.null(x$before)) {
      withr::with_dir(path, x$before())
    }
    id <- orderly_run(x$name, x$parameters, echo = FALSE)
    id_new <- demo_change_time(id, x$time, path)
    if (isTRUE(x$publish)) {
      orderly_publish(id_new)
    }
    id_new
  }

  res <- vcapply(dat, f)

  path
}

fake_db <- function(con, seed = 1) {
  set.seed(seed)

  id <- ids::adjective_animal(20)
  n <- 200

  d <- data.frame(id = seq_along(id),
                  name = id,
                  number = stats::runif(length(id)))
  DBI::dbWriteTable(con, "thing", d, overwrite = TRUE)

  d <- data.frame(id = seq_len(n),
                  thing = sample(length(id), n, replace = TRUE),
                  value = stats::rnorm(n),
                  stringsAsFactors = FALSE)
  DBI::dbWriteTable(con, "data", d, overwrite = TRUE)
}

prepare_orderly_example <- function(name, path = tempfile()) {
  src <- orderly_file(file.path("examples", name))
  orderly_init(path, quiet = TRUE)
  src_files <- dir(src, full.names = TRUE)
  file.copy(src_files, path, overwrite = TRUE, recursive = TRUE)

  if (file.exists(file.path(path, "source.R"))) {
    generator <- source(file.path(path, "source.R"), local = TRUE)$value
  } else {
    generator <- fake_db
  }
  con <- orderly_db("source", config = path)
  on.exit(DBI::dbDisconnect(con))
  generator(con)
  path
}
