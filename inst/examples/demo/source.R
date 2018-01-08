function(con) {
  orderly:::fake_db(con)
  DBI::dbWriteTable(con, "mtcars", mtcars)
}
