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

## Via wikimedia:
MAGIC_PNG <- as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a))
