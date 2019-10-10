function(con) {
  DBI::dbWriteTable(con$source1, "mtcars", mtcars)
  DBI::dbWriteTable(con$source2, "iris", iris)
}
