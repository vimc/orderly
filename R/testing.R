## Create an example set that includes some orderly runs (compared
## with prepare_orderly_example below that simply prepares the
## source).  This is used to create a set of data for testing the API.
## See inst/demo/demo.yml for more information.
create_orderly_demo <- function(path = tempfile()) {
  prepare_orderly_example("demo", path)
  run_orderly_demo(path)
}


run_orderly_demo <- function(path) {
  dat <- read_demo_yml(path)

  for (i in seq_along(dat)) {
    if (i > 1) {
      orderly_log_break()
    }
    orderly_log("demo", sprintf("%d / %d", i, length(dat)))

    x <- dat[[i]]
    if (!is.null(x$before)) {
      withr::with_dir(path, x$before())
    }
    id <- orderly_run(x$name, x$parameters, echo = FALSE, root = path)
    id_new <- demo_change_time(id, x$time, path)
    if (isTRUE(x$publish)) {
      orderly_publish(id_new, root = path)
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
  DBI::dbWriteTable(con$source, "thing", d, overwrite = TRUE)

  d <- data.frame(id = seq_len(n),
                  thing = sample(length(id), n, replace = TRUE),
                  value = stats::rnorm(n),
                  stringsAsFactors = FALSE)
  DBI::dbWriteTable(con$source, "data", d, overwrite = TRUE)
}

## Copy an example directory from 'inst/' and set up the source
## database ready to be used.
prepare_orderly_example <- function(name, path = tempfile()) {
  src <- orderly_file(file.path("examples", name))
  orderly_init(path, quiet = TRUE)
  src_files <- dir(src, full.names = TRUE)
  file_copy(src_files, path, overwrite = TRUE, recursive = TRUE)

  if (file.exists(file.path(path, "source.R"))) {
    generator <- source(file.path(path, "source.R"), local = TRUE)$value
  } else {
    generator <- fake_db
  }
  con <- orderly_db("source", root = path)
  on.exit(lapply(con, DBI::dbDisconnect))
  if (length(con) > 0L) {
    generator(con)
  }
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

  rds <- path_orderly_run_rds(p)
  dat <- readRDS(rds)
  dat$time <- time
  dat$meta$id <- id_new
  dat$meta$date <- as.character(time)
  saveRDS(dat, rds)

  changelog <- dat$meta$changelog
  if (!is.null(changelog)) {
    changelog$report_version[changelog$report_version == id] <- id_new
    dat$meta$changelog <- changelog
  }

  orderly_commit(id_new, name, root = path)

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
  git_run(c("config", "user.email", "email@example.com"), root = path,
          check = TRUE)
  git_run(c("config", "user.name", "orderly"), root = path, check = TRUE)
  writeLines(c("source.sqlite", "orderly.sqlite",
               "archive", "data", "draft", "extra", "runner", "upstream"),
             file.path(path, ".gitignore"))
  git_run(c("add", "."), root = path, check = TRUE)
  git_run(c("add", "-f", "archive", "data", "draft"), root = path, check = TRUE)
  git_run(c("commit", "-m", "'initial-import'"), root = path, check = TRUE)
  stopifnot(git_is_clean(path))

  prev <- git_checkout_branch("other", root = path, create = TRUE)

  file.rename(file.path(path, "extra", "other"),
              file.path(path, "src", "other"))
  unlink(file.path(path, "extra"), recursive = TRUE)
  git_run(c("add", "."), root = path)
  git_run(c("commit", "-m", "'add-other'"), root = path)

  git_checkout_branch("master", root = path)

  archive <- zip_dir(path)
  options(orderly.server.demo = archive)
  archive
}

unzip_git_demo <- function(path = tempfile()) {
  tmp <- tempfile()
  dir.create(tmp, FALSE, TRUE)
  demo <- getOption("orderly.server.demo", build_git_demo())
  utils::unzip(demo, exdir = tmp)
  dir.create(path, FALSE, TRUE)
  src <- dir(file.path(tmp, "demo"), full.names = TRUE, all.files = TRUE,
             no.. = TRUE)
  file_copy(src, path, recursive = TRUE)
  unlink(tmp, recursive = TRUE)
  path
}

prepare_orderly_git_example <- function(path = tempfile(), run_report = FALSE) {
  path_upstream <- file.path(path, "upstream")
  unzip_git_demo(path)
  unzip_git_demo(path_upstream)

  git_run(c("remote", "add", "origin", basename(path_upstream)), path)
  git_fetch(path)
  git_run(c("branch", "--set-upstream-to", "origin/master", "master"), path)

  writeLines("new", file.path(path_upstream, "new"))
  git_run(c("add", "."), path_upstream)
  git_run(c("commit", "-m", "orderly"), path_upstream)

  if (run_report) {
    id <- orderly_run("minimal", root = path)
    orderly_commit(id, root = path)
  }

  c(origin = path_upstream, local = path)
}
