function(con) {
  DBI::dbWriteTable(con, "mtcars", mtcars)
}
