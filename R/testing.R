## Create an example set that includes some orderly runs (compared
## with prepare_orderly_example below that simply prepares the
## source).  This is used to create a set of data for testing the API.
## See inst/demo/demo.yml for more information.
create_orderly_demo <- function(path = tempfile()) {
  prepare_orderly_example("demo", path)

  dat <- read_demo_yml(path)

  orderly_default_config_set(orderly_config(path))
  on.exit(orderly_default_config_set(NULL))

  for (i in seq_along(dat)) {
    if (i > 1) {
      orderly_log_break()
    }
    orderly_log("demo", sprintf("%d / %d", i, length(dat)))

    x <- dat[[i]]
    if (!is.null(x$before)) {
      withr::with_dir(path, x$before())
    }
    id <- orderly_run(x$name, x$parameters, echo = FALSE)
    id_new <- demo_change_time(id, x$time, path)
    if (isTRUE(x$publish)) {
      orderly_publish(id_new)
    }
  }

  path
}

## This is a really rubbish set of test data.  It requires an open
## "source" database and will write out two tables.  This is used by
## the examples.
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

## Copy an example directory from 'inst/' and set up the source
## database ready to be used.
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

    t <- t + stats::runif(1, 0.1 * day, 5 * day)
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

## This version will eventually go into a yml thing but it's a bit
## nasty to deal with at the moment.  This means it's not easily
## extendable...
##
## After building this we have two branches 'master' with
build_git_demo <- function() {
  path <- prepare_orderly_example("demo", file.path(tempfile(), "demo"))

  dir.create(file.path(path, "extra"))
  move <- setdiff(dir(file.path(path, "src"), pattern = "^[^.]+$"), "minimal")
  file.rename(file.path(path, "src", move),
              file.path(path, "extra", move))

  git_run("init", root = path)
  writeLines(c("source.sqlite", "orderly.sqlite",
               "archive", "data", "draft", "extra", "runner"),
             file.path(path, ".gitignore"))
  git_run(c("add", "."), root = path)
  git_run(c("add", "-f", "archive", "data", "draft"), root = path)
  git_run(c("commit", "-m", "'initial import'"), root = path)
  stopifnot(git_is_clean(path))

  prev <- git_checkout_branch("other", root = path, create = TRUE)

  file.rename(file.path(path, "extra", "other"),
              file.path(path, "src", "other"))
  git_run(c("add", "."), root = path)
  git_run(c("commit", "-m", "'add other'"), root = path)

  git_checkout_branch("master", root = path)

  archive <- zip_dir(path)
  options(orderly.server.demo = archive)
  archive
}

unzip_git_demo <- function() {
  path <- tempfile()
  dir.create(path, FALSE, TRUE)
  demo <- getOption("orderly.server.demo", build_git_demo())
  utils::unzip(demo, exdir = path)
  file.path(path, "demo")
}
