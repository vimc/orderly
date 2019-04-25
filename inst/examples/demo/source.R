function(con) {
  orderly:::fake_db(con)
  DBI::dbWriteTable(con$source, "mtcars", mtcars)
}
