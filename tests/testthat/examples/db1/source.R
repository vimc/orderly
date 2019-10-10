function(con) {
  DBI::dbWriteTable(con$source1, "mtcars", mtcars)
}
