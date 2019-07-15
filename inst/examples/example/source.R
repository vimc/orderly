function(con) {
  DBI::dbWriteTable(con$source, "mtcars", mtcars)
}
