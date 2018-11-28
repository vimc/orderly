with_wd <- function(path, code) {
  owd <- setwd(path)
  on.exit(setwd(owd))
  force(code)
}

read_orderly_db <- function(path) {
  con <- orderly_db("destination", path)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbReadTable(con, "orderly")
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

skip_if_no_vault_server <- function() {
  testthat::skip_if_not_installed("vaultr")
  if (is.null(vaultr::vault_test_server())) {
    testthat::skip("vault test server not running")
  }
}

start_vault <- function() {
  Sys.unsetenv("VAULTR_CACHE_DIR")
  Sys.unsetenv("VAULT_ADDR")
  with_vault <- !is.null(vaultr::vault_test_server_start())
  if (with_vault) {
    cl <- vaultr::vault_test_client()
    message("Writing test data")
    ## Some test data to use
    cl$write("/secret/users/alice", list(password = "ALICE"))
    cl$write("/secret/users/bob", list(password = "BOB"))
  }
}

reset_vault <- function() {
  cache$vault <- NULL
  vaultr::vault_clear_token_cache(session = TRUE, persistent = FALSE)
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
  if (has_internet()) {
    return()
  }
  testthat::skip("no internet")
}

unpack_reference <- function(version, path = tempfile()) {
  unzip(sprintf("reference/%s.zip", version), exdir = path)
  file.path(path, version)
}


prepare_orderly_remote_example <- function(api, path = tempfile()) {
  path_remote <- file.path(path, "remote")
  path_local <- file.path(path, "local")

  prepare_orderly_example("depends", path_remote)

  id1 <- orderly_run("example", config = path_remote, echo = FALSE)
  id2 <- orderly_run("example", config = path_remote, echo = FALSE)
  orderly_commit(id1, config = path_remote)
  orderly_commit(id2, config = path_remote)
  remote_path <- orderly_remote_path(path_remote)

  path_local <- prepare_orderly_example("depends")

  ## Patch the report to use non-draft dependencies:
  p <- file.path(path_local, "src", "depend", "orderly.yml")
  d <- sub("draft: true", "draft: false", readLines(p))
  writeLines(d, p)

  if (api) {
    append_lines(c("api_server:",
                   "  default:",
                   "    host: example.com",
                   "    port: 443",
                   "    username: me",
                   "    password: password"),
                 file.path(path_local, "orderly_config.yml"))
    config <- orderly_config(path_local)
    config$api_server$default$server$token <- "123"
    remote <- get_remote(NULL, config)
  } else {
    remote <- orderly_remote_path(path_remote)
    config <- orderly_config(path_local)
  }

  list(path_remote = path_remote,
       path_local = path_local,
       config = config,
       remote = remote,
       id1 = id1,
       id2 = id2)
}
