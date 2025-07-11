function(con) {
  orderly1:::fake_db(con)
  DBI::dbWriteTable(con$source, "mtcars", mtcars)
}
