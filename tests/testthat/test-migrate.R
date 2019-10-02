context("migrations")

test_that("0.3.2 -> 0.3.3", {
  oo <- options(orderly.nowarnings = TRUE)
  on.exit(options(oo))

  path <- unpack_reference("0.3.2")
  cmp <- unpack_reference("0.4.8")
  orderly_migrate(path, to = "0.3.3")
  patch_orderly_config(path)

  d <- orderly_list_archive(path)
  p <- file.path(d$name, d$id)

  i <- which(d$name == "depend")[[1]]
  m1 <- readRDS(file.path(path_archive(path), path_orderly_run_rds(p[[i]])))
  m2 <- readRDS(file.path(path_archive(cmp),  path_orderly_run_rds(p[[i]])))
  expect_equal(m1, m2)

  pp <- file.path(path_archive(path), p[[i]])
  expect_true(file.exists(path_orderly_run_rds_backup(pp, "0.3.3")))
  expect_equal(readLines(path_orderly_archive_version(path)), "0.3.3")
})


test_that("roll back migration", {
  oo <- options(orderly.nowarnings = TRUE)
  on.exit(options(oo))

  path <- unpack_reference("0.3.2")
  hash <- hash_files(list.files(path, recursive = TRUE, full.names = TRUE))
  orderly_migrate(path, to = "0.3.3")
  migrate_rollback(path, "0.3.3", "0.0.0")

  expect_equal(readLines(path_orderly_archive_version(path)), "0.0.0")
  unlink(path_orderly_archive_version(path))
  expect_equal(
    hash_files(list.files(path, recursive = TRUE, full.names = TRUE)),
    hash)
})


test_that("failed migrations are rolled back", {
  oo <- options(orderly.nowarnings = TRUE)
  on.exit(options(oo))

  path <- unpack_reference("0.3.2")
  hash <- hash_files(list.files(path, recursive = TRUE, full.names = TRUE))

  counter <- new_counter()
  fun <- function(data, path, config) {
    if (counter() >= 3L) {
      stop("some sort of migration failure")
    }
    ## any old bit to indicate a change:
    data$updated <- TRUE
    list(changed = TRUE, data = data)
  }

  config <- orderly_config(path)
  expect_error(migrate_apply(path, "0.3.3", fun, config, FALSE, FALSE),
               "some sort of migration failure")
  cmp <- hash_files(list.files(path, recursive = TRUE, full.names = TRUE))
  expect_equal(cmp[basename(names(cmp)) != "orderly_archive_version"], hash)
})


test_that("failed migrations can be skipped", {
  oo <- options(orderly.nowarnings = TRUE)
  on.exit(options(oo))

  path <- unpack_reference("0.3.2")
  hash <- hash_files(list.files(path, recursive = TRUE, full.names = TRUE))

  counter <- new_counter()
  fun <- function(data, path, config) {
    if (counter() >= 5L) {
      stop("some sort of migration failure")
    }
    ## any old bit to indicate a change:
    data$updated <- TRUE
    list(changed = TRUE, data = data)
  }

  patch_orderly_config(path)
  config <- orderly_config(path)
  migrate_apply(path, "0.3.3", fun, config, FALSE, TRUE)

  id <- "20170805-220525-1dc8fb81"

  expect_equal(dir(file.path(path, path_archive_broken())), "depend")
  expect_equal(dir(file.path(path, path_archive_broken(), "depend")), id)
  p <- file.path(path, path_archive_broken(), "depend", id)
  expect_true(is_directory(p))
  expect_true(is_directory(file.path(path_archive(path), "depend")))
  expect_false(file.exists(file.path(path_archive(path), "depend", id)))
})


test_that("failed migrations warned in dry run", {
  oo <- options(orderly.nowarnings = TRUE)
  on.exit(options(oo))

  path <- unpack_reference("0.3.2")
  hash <- hash_files(list.files(path, recursive = TRUE, full.names = TRUE))

  counter <- new_counter()
  fun <- function(data, path, config) {
    if (counter() >= 5L) {
      stop("some sort of migration failure")
    }
    ## any old bit to indicate a change:
    data$updated <- TRUE
    list(changed = TRUE, data = data)
  }

  patch_orderly_config(path)
  config <- orderly_config(path)
  expect_message(
    migrate_apply(path, "0.3.3", fun, config, TRUE, TRUE),
    "this report would be moved to")
})


test_that("dry run", {
  oo <- options(orderly.nowarnings = TRUE)
  on.exit(options(oo))

  path <- unpack_reference("0.3.2")
  hash <- hash_files(list.files(path, recursive = TRUE, full.names = TRUE))
  orderly_migrate(path, to = "0.3.3", dry_run = TRUE)
  expect_equal(
    hash_files(list.files(path, recursive = TRUE, full.names = TRUE)),
    hash)
})


test_that("migrate_plan default is used", {
  oo <- options(orderly.nowarnings = TRUE)
  on.exit(options(oo))

  path <- unpack_reference("0.3.2")
  config <- orderly_config(path)
  expect_equal(migrate_plan(config$archive_version), available_migrations())
  expect_equal(migrate_plan(config$archive_version, to = "0.0.1"),
               set_names(character(), character()))
})


test_that("mixed migration", {
  oo <- options(orderly.nowarnings = TRUE)
  on.exit(options(oo))

  path <- unpack_reference("0.3.2")

  curr <- as.character(cache$current_archive_version)

  ## Need to work around an intentional assertion
  writeLines(curr, path_orderly_archive_version(path))
  id <- orderly_run("example", root = path, echo = FALSE)
  orderly_commit(id, root = path)

  unlink(path_orderly_archive_version(path))

  msg <- capture_messages(
    orderly_migrate(path, to = curr))
  expect_true(
    any(grepl(sprintf("[ ok         ]  example/%s", id), msg, fixed = TRUE)))
})


test_that("require migration", {
  oo <- options(orderly.nowarnings = TRUE)
  on.exit(options(oo))

  path <- unpack_reference("0.3.2")
  expect_error(orderly_run("example", root = path, echo = FALSE),
               "orderly archive needs migrating from 0.0.0 =>", fixed = TRUE)
  expect_error(orderly_run("example", root = path, echo = FALSE),
               "Run orderly::orderly_migrate() to fix", fixed = TRUE)
  orderly_migrate(path)
  expect_error(orderly_run("example", root = path, echo = FALSE), NA)
})


test_that("can't commit old version", {
  oo <- options(orderly.nowarnings = TRUE)
  on.exit(options(oo))

  path <- unpack_reference("0.3.2")
  patch_orderly_config(path)
  contents <- orderly_list_archive(path)

  id <- contents$id[contents$name == "depend"][[1L]]
  file.rename(file.path(path, "archive", "depend", id),
              file.path(path, "draft", "depend", id))
  orderly_migrate(path)
  orderly_rebuild(path)
  expect_error(
    orderly_commit(id, root = path),
    "This report was built with an old version of orderly; please rebuild",
    fixed = TRUE)
})


test_that("don't migrate new orderly", {
  oo <- options(orderly.nowarnings = TRUE)
  on.exit(options(oo))

  path <- prepare_orderly_example("minimal")
  p <- path_orderly_archive_version(path)
  unlink(p)
  check_orderly_archive_version(orderly_config(path))
  expect_true(file.exists(p))
  expect_equal(read_orderly_archive_version(path),
               as.character(cache$current_archive_version))
})


## Here's a test that fails prior to 0.5.1
test_that("database migrations", {
  oo <- options(orderly.nowarnings = TRUE)
  on.exit(options(oo))

  path <- unpack_reference("0.5.1")
  con <- orderly_db("destination", path, validate = FALSE)
  dat <- DBI::dbReadTable(con, "report_version")
  DBI::dbDisconnect(con)
  expect_false("published" %in% names(dat))

  orderly_migrate(root = path)

  id <- orderly_run("minimal", root = path, echo = FALSE)
  expect_error(
    orderly_commit(id, root = path),
    "orderly db needs rebuilding with orderly::orderly_rebuild()")

  orderly_rebuild(path)

  con <- orderly_db("destination", path, validate = FALSE)
  dat <- DBI::dbReadTable(con, "report_version")
  DBI::dbDisconnect(con)

  expect_error(orderly_commit(id, root = path), NA)
})


test_that("automatic migrations", {
  oo <- options(orderly.nowarnings = TRUE)
  on.exit(options(oo))

  path <- unpack_reference("0.5.1")
  patch_orderly_config(path)
  con <- orderly_db("destination", path, validate = FALSE)
  dat <- DBI::dbReadTable(con, "report_version")
  DBI::dbDisconnect(con)
  expect_false("published" %in% names(dat))

  expect_true(orderly_rebuild(root = path, if_schema_changed = TRUE))
  expect_false(orderly_rebuild(root = path, if_schema_changed = TRUE))
  expect_true(orderly_rebuild(root = path, if_schema_changed = FALSE))
})


test_that("migrate 0.5.4 -> 0.5.5", {
  oo <- options(orderly.nowarnings = TRUE)
  on.exit(options(oo))

  path <- unpack_reference("0.5.4")
  orderly_migrate(path, to = "0.5.5")
  orderly_rebuild(path)

  patch_orderly_config(path)
  con <- orderly_db("destination", path, validate = FALSE)
  dat <- DBI::dbReadTable(con, "report_version")
  DBI::dbDisconnect(con)

  expect_equal(dat$connection == 1,
               dat$report == "connection")
})


test_that("rebuild db with incorrect schema information", {
  oo <- options(orderly.nowarnings = TRUE)
  on.exit(options(oo))

  path <- unpack_reference("0.5.17")
  patch_orderly_config(path)
  con <- orderly_db("destination", path, validate = FALSE)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  DBI::dbExecute(
    con,
    "DELETE FROM orderly_schema_tables WHERE name is 'parameters'")

  expect_true("parameters" %in% DBI::dbListTables(con))
  expect_false("parameters" %in%
               DBI::dbReadTable(con, "orderly_schema_tables")$name)

  expect_warning(
    orderly_rebuild(path),
    "While rebuilding the orderly database, we will delete additional",
    fixed = TRUE)
})


test_that("migrate 0.5.18 -> 0.6.0", {
  oo <- options(orderly.nowarnings = TRUE)
  on.exit(options(oo))

  path <- unpack_reference("0.5.18")

  reports <- subset(orderly_list_archive(path), name == "minimal")
  p <- file.path(path, "archive", "minimal", reports$id[[1]])
  expect_null(readRDS(path_orderly_run_rds(p))$meta$data, "data.frame")

  orderly_migrate(path, to = "0.6.0")
  orderly_rebuild(path)

  expect_is(readRDS(path_orderly_run_rds(p))$meta$data, "data.frame")
})


test_that("migrate 0.6.0 -> 0.6.1", {
  oo <- options(orderly.nowarnings = TRUE)
  on.exit(options(oo))

  path <- unpack_reference("0.6.0")

  reports <- subset(orderly_list_archive(path), name == "use_resource")
  p <- file.path(path, "archive", "use_resource", reports$id[[1]])
  expect_null(readRDS(path_orderly_run_rds(p))$meta$hash_readme)

  orderly_migrate(path, to = "0.6.1")

  expect_equal(
    readRDS(path_orderly_run_rds(p))$meta$hash_readme,
    c("README.md" = "499b37b7fbc174e2fd8559ad96a06178"))
})


test_that("migrate 0.6.1 -> 0.6.2", {
  oo <- options(orderly.nowarnings = TRUE)
  on.exit(options(oo))

  path <- unpack_reference("0.6.0")

  reports <- subset(orderly_list_archive(path), name == "use_resource")
  p <- file.path(path, "archive", "use_resource", reports$id[[1]])
  expect_null(readRDS(path_orderly_run_rds(p))$meta$hash_orderly_yml)

  orderly_migrate(path, to = "0.6.2")

  expect_equal(
    readRDS(path_orderly_run_rds(p))$meta$hash_orderly_yml,
    c("orderly.yml" = "23f4542f02ad69ddafc6fab2a4391e6c"))
})


test_that("migrate 0.6.1 -> 0.6.7", {
  oo <- options(orderly.nowarnings = TRUE)
  on.exit(options(oo))

  path <- unpack_reference("0.6.0")

  reports <- subset(orderly_list_archive(path), name == "changelog")
  p <- file.path(path, "archive", "changelog", reports$id[[2]])
  expect_null(readRDS(path_orderly_run_rds(p))$meta$changelog)

  orderly_migrate(path, to = "0.6.7")
  orderly_rebuild(path)

  changelog <- readRDS(path_orderly_run_rds(p))$meta$changelog
  expect_is(changelog, "data.frame")
  expect_true("id" %in% names(changelog))
  con <- orderly_db("destination", root = path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  cmp <- DBI::dbReadTable(con, "changelog")
  cmp$from_file <- as.logical(cmp$from_file)
  i <- match(changelog$id, cmp$id)
  expect_false(any(is.na(i)))
  expect_true(all(names(changelog) %in% names(cmp)))
  expect_equivalent(changelog, cmp[i, names(changelog)])
})


test_that("migrate => 0.6.8", {
  oo <- options(orderly.nowarnings = TRUE)
  on.exit(options(oo))

  path <- unpack_reference("0.6.0")

  con <- orderly_db("destination", path, validate = FALSE)

  old <- list(file = DBI::dbReadTable(con, "file"),
              file_artefact = DBI::dbReadTable(con, "file_artefact"),
              file_input = DBI::dbReadTable(con, "file_input"),
              data = DBI::dbReadTable(con, "data"))
  DBI::dbDisconnect(con)

  orderly_migrate(path, to = "0.6.8")
  orderly_rebuild(path)

  con <- orderly_db("destination", path)
  new <- list(file = DBI::dbReadTable(con, "file"),
              file_artefact = DBI::dbReadTable(con, "file_artefact"),
              file_input = DBI::dbReadTable(con, "file_input"),
              data = DBI::dbReadTable(con, "data"))
  DBI::dbDisconnect(con)

  expect_setequal(old$file$hash, new$file$hash)
  expect_equal(old$file$size[match(new$file$hash, old$file$hash)],
               new$file$size)

  expect_setequal(new$data$hash, old$data$hash)
  expect_equal(old$data$size_csv[match(new$data$hash, old$data$hash)],
               new$data$size_csv)
  expect_equal(old$data$size_rds[match(new$data$hash, old$data$hash)],
               new$data$size_rds)

  expect_equal(old$file_artefact, new$file_artefact)

  i <- order(old$file_input$report_version,
             old$file_input$filename,
             old$file_input$file_purpose)
  j <- order(new$file_input$report_version,
             new$file_input$filename,
             new$file_input$file_purpose)
  old_file_input <- old$file_input[i, -1]
  new_file_input <- new$file_input[j, -1]
  rownames(old_file_input) <- rownames(new_file_input) <- NULL
  expect_equal(old_file_input, new_file_input)
})


test_that("patch modified artefact", {
  oo <- options(orderly.nowarnings = TRUE)
  on.exit(options(oo))
  path <- unpack_reference("0.6.0")
  orderly_migrate(path, to = "0.6.8")

  sql <- paste("SELECT file_artefact.* from file_artefact",
               "JOIN report_version_artefact",
               "  ON report_version_artefact.id = file_artefact.artefact",
               "WHERE report_version_artefact.report_version = $1")

  list <- orderly_list_archive(path)
  id <- list$id[list$name == "multi-artefact"]

  con <- orderly_db("destination", path, validate = FALSE)
  old <- DBI::dbGetQuery(con, sql, id)
  DBI::dbDisconnect(con)

  path_rds <- file.path(path, "archive", "multi-artefact", id,
                        "orderly_run.rds")

  ## modify an artefact:
  p <- file.path(path, "archive", "multi-artefact", id, "subset.csv")
  h1 <- hash_files(p, FALSE)
  txt <- readLines(p)
  writeLines(txt[-length(txt)], p)
  h2 <- hash_files(p, FALSE)

  ## Preflight check:
  expect_equal(readRDS(path_rds)$meta$file_info_artefacts$file_hash[[2]], h1)
  expect_equal(old$file_hash[old$filename == "subset.csv"], h1)

  ## Do the migration
  orderly_migrate(path, "0.7.12")
  orderly_rebuild(path)

  ## Confirm we pick up the changes
  con <- orderly_db("destination", path, validate = FALSE)
  new <- DBI::dbGetQuery(con, sql, id)
  DBI::dbDisconnect(con)

  expect_equal(readRDS(path_rds)$meta$file_info_artefacts$file_hash[[2]], h2)
  expect_equal(new$file_hash[new$filename == "subset.csv"], h2)
})


test_that("migrate => 0.7.15", {
  oo <- options(orderly.nowarnings = TRUE)
  on.exit(options(oo))

  path <- unpack_reference("0.6.0")
  orderly_migrate(path, to = "0.7.15")
  orderly_rebuild(path)

  list <- orderly_list_archive(path)
  id <- list$id[list$name == "global"]

  d <- readRDS(path_orderly_run_rds(file.path(path, "archive", "global", id)))
  expect_equal(d$meta$global_resources, c("data.csv" = "data.csv"))

  con <- orderly_db("destination", path, validate = FALSE)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  tab <- DBI::dbReadTable(con, "file_input_global")
  expect_equal(nrow(tab), 1)
})
