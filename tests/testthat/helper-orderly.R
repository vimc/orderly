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

skip_if_no_vault_server <- function() {
  testthat::skip_if_not_installed("vaultr")
  if (is.null(vaultr::vault_test_server())) {
    testthat::skip("vault test server not running")
  }
}

start_vault <- function() {
  with_vault <- !is.null(vaultr::vault_test_server_start())
  if (with_vault) {
    cl <- vaultr::vault_test_client()
    message("Writing test data")
    ## Some test data to use
    cl$write("/secret/users/alice", list(password = "ALICE"))
    cl$write("/secret/users/bob", list(password = "BOB"))
  }
}

## Via wikimedia:
MAGIC_PNG <- as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a))
