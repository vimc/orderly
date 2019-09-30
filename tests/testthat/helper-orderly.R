with_wd <- function(path, code) {
  owd <- setwd(path)
  on.exit(setwd(owd))
  force(code)
}

skip_if_no_git <- function() {
  if (nzchar(Sys.which("git"))) {
    return()
  }
  testthat::skip("git was not found on the path")
}

skip_on_windows <- function() {
  testthat::skip_on_os("windows")
}


## Via wikimedia:
MAGIC_PNG <- as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a))

with_sqlite <- function(path, fun) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con))
  fun(con)
}


has_internet <- function() {
  !is.null(suppressWarnings(utils::nsl("www.google.com")))
}


skip_if_no_internet <- function() {
  skip_on_windows()
  skip_on_cran() # not worth it
  if (has_internet()) {
    return()
  }
  testthat::skip("no internet")
}

unpack_reference <- function(version, path = tempfile()) {
  src <- sprintf("reference/%s.zip", version)
  if (!file.exists(src)) {
    msg <- sprintf("Reference data %s not available", version)
    if (identical(Sys.getenv("TRAVIS"), "true")) {
      stop(msg)
    } else {
      testthat::skip(msg)
    }
  }
  zip::unzip(src, exdir = path)
  file.path(path, version)
}


prepare_orderly_remote_example <- function(path = tempfile()) {
  path_remote <- file.path(path, "remote")
  path_local <- file.path(path, "local")

  prepare_orderly_example("depends", path_remote)

  id1 <- orderly_run("example", root = path_remote, echo = FALSE)
  id2 <- orderly_run("example", root = path_remote, echo = FALSE)
  orderly_commit(id1, root = path_remote)
  orderly_commit(id2, root = path_remote)
  remote_path <- orderly_remote_path(path_remote)

  path_local <- prepare_orderly_example("depends")

  ## Patch the report to use non-draft dependencies:
  p <- file.path(path_local, "src", "depend", "orderly.yml")
  d <- sub("draft: true", "draft: false", readLines(p))
  writeLines(d, p)

  r <- list(remote = list(
              default = list(
                driver = "orderly::orderly_remote_path",
                args = list(path = path_remote))))
  append_lines(yaml::as.yaml(r), file.path(path_local, "orderly_config.yml"))

  config <- orderly_config(path_local)
  remote <- get_remote(NULL, config)

  list(path_remote = path_remote,
       path_local = path_local,
       config = config,
       remote = remote,
       id1 = id1,
       id2 = id2)
}


patch_orderly_config <- function(path) {
  p <- file.path(path, "orderly_config.yml")
  dat <- yaml_read(p)
  dat$database <- list(source = dat$source)
  dat$source <- NULL
  writeLines(yaml::as.yaml(dat), p)
}


## Quieten down the SQLite warning about unused connections as it
## makes testing for silentness dependent on the order of tests.
##
## ?RSQLite::SQLite says
##
## > Connections are automatically cleaned-up after they're deleted and
## > reclaimed by the GC. You can use ‘DBI::dbDisconnect()’ to
## > terminate the connection early, but it will not actually close
## > until all open result sets have been closed (and you'll get a
## > warning message to this effect).
##
## which suggests that there's no good reason to need to disconnect,
## and we do try to but it's a bit of a faff.
local({
  suppressWarnings({
    DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    gc()
  })
})


new_counter <- function() {
  e <- new.env(parent = emptyenv())
  e$x <- 0L
  function() {
    e$x <- e$x + 1L
    e$x
  }
}
