with_wd <- function(path, code) {
  owd <- setwd(path)
  on.exit(setwd(owd))
  force(code)
}

prepare_minimal <- function() {
  path <- tempfile()
  suppressMessages(orderly_init(path, quiet = TRUE))
  fake_db(DBI::dbConnect(RSQLite::SQLite(), file.path(path, "source.sqlite")))
  file_copy(orderly_file("minimal_config.yml"),
            file.path(path, "orderly_config.yml"),
            overwrite = TRUE)
  path_example <- file.path(path, "src", "example")
  dir.create(path_example)
  file.copy(orderly_file("minimal_report.yml"),
            file.path(path_example, "orderly.yml"))
  file.copy(orderly_file("minimal_script.R"),
            file.path(path_example, "script.R"))
  path
}

prepare_example <- function() {
  path <- tempfile()
  suppressMessages(orderly_init(path, quiet = TRUE))
  file_copy(orderly_file("orderly_config_sqlite.yml"),
            file.path(path, "orderly_config.yml"),
            overwrite = TRUE)
  local({
    con <- orderly::orderly_db("source", path, FALSE)
    DBI::dbWriteTable(con, "mtcars", mtcars)
  })

  path_example <- file.path(path, "src", "example")
  dir.create(path_example)
  file.copy(orderly_file("report_example.yml"),
            file.path(path_example, "orderly.yml"))
  file.copy(orderly_file("report_example.R"),
            file.path(path_example, "script.R"))
  path
}

## Via wikimedia:
MAGIC_PNG <- as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a))
