##' Set up one of the orderly examples included with the package.
##' These are not intended to be starting points for new orderly
##' repositories, but are used in the package examples and vignettes.
##' @title Set up an orderly example
##'
##' @param name Name of the example
##'
##' @param path Destination to create the example - if it exists
##'   already it must be an empty directory.  By default, creates a
##'   new temporary directory
##'
##' @param run_demo Logical, indicating if the example is configured
##'   as a "demo" (i.e., with a set of reports to be run and
##'   committed), should these be run?
##'
##' @param quiet Logical, indicating if informational messages should
##'   be suppressed when running the demo.
##'
##' @param git Logical, indicating if we should create an basic git
##'   repository along with the demo. This will have the default
##'   orderly .gitignore set up, and a remote which is itself (so that
##'   git pull and git fetch run without error, though they will do
##'   nothing).
##'
##' @return Returns the path to the orderly example
##' @export
##' @examples
##' # Create a new copy of the "minimal" example
##' path <- orderly1::orderly_example("minimal")
##' dir(path)
##'
##' # Example reports within this repository:
##' orderly1::orderly_list(path)
orderly_example <- function(name, path = tempfile(), run_demo = FALSE,
                            quiet = FALSE, git = FALSE) {
  path <- prepare_orderly_example(name, path, git = git)
  if (run_demo && file.exists(path_demo_yml(path))) {
    run_orderly_demo(path, quiet)
  }
  path
}


## Create an example set that includes some orderly runs (compared
## with prepare_orderly_example below that simply prepares the
## source).  This is used to create a set of data for testing the API.
## See inst/demo/demo.yml for more information.
create_orderly_demo <- function(path = tempfile(), quiet = FALSE,
                                git = FALSE) {
  orderly_example("demo", path, TRUE, quiet, git)
}


prepare_git_example_from_source <- function(source_path, path) {
  temp <- file.path(tempfile(), "demo")
  fs::dir_copy(source_path, temp)
  generate_source_db(temp)
  run_orderly_demo(temp)
  build_git_demo(temp)
  res <- prepare_orderly_git_example(path, run_report = FALSE)
  options(orderly.server.demo = NULL)
  unlink(temp, recursive = TRUE)
  res
}


run_orderly_demo <- function(path, quiet = FALSE) {
  if (quiet) {
    oo <- options(orderly.nolog = TRUE)
    on.exit(options(oo))
  }

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
      legacy_orderly_publish(x$name, id_new, TRUE, path)
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
prepare_orderly_example <- function(name, path = tempfile(), testing = FALSE,
                                    git = FALSE) {
  if (testing) {
    src <- file.path("examples", name)
    stopifnot(file.exists(src))
  } else {
    src <- orderly_file(file.path("examples", name))
  }
  orderly_init(path, quiet = TRUE)
  path <- normalizePath(path, mustWork = TRUE)
  src_files <- dir(src, full.names = TRUE)
  file_copy(src_files, path, overwrite = TRUE, recursive = TRUE)

  generate_source_db(path)

  if (git) {
    prepare_basic_git(path, quiet = TRUE)
  }

  path
}


generate_source_db <- function(path) {
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
}


read_demo_yml <- function(path) {
  e <- new.env(parent = parent.env(.GlobalEnv))
  before <- file.path(path, "before.R")
  if (file.exists(before)) {
    sys.source(before, e)
  }

  dat <- yaml_read(path_demo_yml(path))
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
  changelog <- dat$meta$changelog
  if (!is.null(changelog)) {
    changelog$report_version[changelog$report_version == id] <- id_new
    dat$meta$changelog <- changelog
  }

  saveRDS(dat, rds)

  orderly_commit(id_new, name, root = path)

  id_new
}


## This version will eventually go into a yml thing but it's a bit
## nasty to deal with at the moment.  This means it's not easily
## extendable...
##
## After building this we have two branches 'master' with
build_git_demo <- function(path = NULL) {
  if (is.null(path)) {
    path <- prepare_orderly_example("demo", file.path(tempfile(), "demo"))
  }
  dir.create(file.path(path, "extra"))
  move <- setdiff(dir(file.path(path, "src"), pattern = "^[^.]+$"),
                  c("minimal", "global"))
  file.rename(file.path(path, "src", move),
              file.path(path, "extra", move))

  git_run("init", root = path)
  git_run(c("config", "user.email", "email@example.com"), root = path,
          check = TRUE)
  git_run(c("config", "user.name", "orderly"), root = path, check = TRUE)
  orderly_use_gitignore(path, prompt = FALSE, show = FALSE)
  gitignore <- readLines(file.path(path, ".gitignore"))
  ## Ignore "extra" dir created above and "upstream" used to store a remote git
  ## repo for testing in orderly.server
  writeLines(c(gitignore, "extra", "upstream"),
             file.path(path, ".gitignore"))
  git_run(c("add", "."), root = path, check = TRUE)
  git_run(c("add", "-f", "archive", "data", "draft"), root = path, check = TRUE)
  git_run(c("commit", "-m", "'initial-import'"), root = path, check = TRUE)
  ensure_default_branch_is_master(path)
  stopifnot(git_is_clean(path))

  prev <- git_checkout_branch("other", root = path, create = TRUE)

  file.rename(file.path(path, "extra", move),
              file.path(path, "src", move))
  unlink(file.path(path, "extra"), recursive = TRUE)
  git_run(c("add", "."), root = path)
  git_run(c("commit", "-m", "'add-other'"), root = path)

  git_checkout_branch("master", root = path)

  archive <- zip_dir(path)
  options(orderly.server.demo = archive)
  archive
}

unzip_git_demo <- function(path = tempfile(), default_branch = "master") {
  tmp <- tempfile()
  dir.create(tmp, FALSE, TRUE)
  demo <- getOption("orderly.server.demo", build_git_demo())
  zip::unzip(demo, exdir = tmp)
  dir.create(path, FALSE, TRUE)
  src <- dir(file.path(tmp, "demo"), full.names = TRUE, all.files = TRUE,
             no.. = TRUE)
  file_copy(src, path, recursive = TRUE)
  unlink(tmp, recursive = TRUE)
  if (default_branch != "master") {
    ## If git changes it's mind about what the default branch is called,
    ## this will fail, we should probably detect this, but it's likely
    ## that will break build_git_demo too
    gert::git_branch_move("master", default_branch, force = TRUE,
                          repo = path)
  }
  path
}

prepare_orderly_git_example <- function(path = tempfile(), run_report = FALSE,
                                        branch = default_branch,
                                        default_branch = "master") {
  path_upstream <- file.path(path, "upstream")
  unzip_git_demo(path, default_branch)
  unzip_git_demo(path_upstream, default_branch)

  git_checkout_branch(branch, root = path)
  git_checkout_branch(branch, root = path_upstream)

  git_run(c("remote", "add", "origin",
            normalizePath(path_upstream, mustWork = TRUE)), path)
  git_fetch(path)
  git_run(c("branch", "--set-upstream-to", sprintf("origin/%s", branch),
            branch), path)

  writeLines("new", file.path(path_upstream, "new"))
  git_run(c("add", "."), path_upstream)
  git_run(c("commit", "-m", "orderly"), path_upstream)

  if (run_report) {
    id <- orderly_run("minimal", root = path)
    orderly_commit(id, root = path)
  }

  c(origin = path_upstream, local = path)
}


prepare_basic_git <- function(path, quiet) {
  suppressMessages(
    orderly_use_gitignore(path, prompt = FALSE, show = FALSE))
  gert::git_init(path)
  withr::with_dir(
    path,
    gert::git_add(".", repo = path))
  gert::git_commit("Init repo", repo = path,
                   author = "T User <test.user@example.com>")
  ensure_default_branch_is_master(path)
  gert::git_remote_add(path, "origin", repo = path)
  gert::git_fetch(remote = "origin", repo = path, verbose = !quiet)
  gert::git_branch_set_upstream("origin/master", "master", repo = path)
  path
}


## We assume master downstream, and in tests
ensure_default_branch_is_master <- function(path) {
  if (gert::git_branch(repo = path) == "main") {
    gert::git_branch_move("main", "master", repo = path)
  }
}
