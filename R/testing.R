create_orderly_demo <- function(path = tempfile()) {
  if (file.exists(path)) {
    if (!is_directory(path) || length(dir(path)) > 0) {
      stop(sprintf("path %s already exists - delete first", path))
    }
  }
  suppressMessages(orderly_init(path, quiet = TRUE))

  txt <- readLines(orderly_file("minimal_config.yml"))
  fields <- c("fields:",
              "  requester:",
              "    required: true",
              "    type: character",
              "  author:",
              "    required: true",
              "    type: character",
              "  comment:",
              "    required: false",
              "    type: character")
  writeLines(c(txt, fields), file.path(path, "orderly_config.yml"))

  extra1 <- c("author: Researcher McResearcherface",
             "requester: Funder McFunderface",
             "comment: This is a comment")
  extra2 <- c("author: Dr Serious",
             "requester: ACME",
             "comment: This is another comment")

  ## Here's a handle to the source database
  con <- orderly::orderly_db("source", path, FALSE)
  fake_db(con)

  path_minimal <- file.path(path, "src", "minimal")
  dir.create(path_minimal)
  writeLines(c(readLines(orderly_file("minimal_report.yml")), extra1),
             file.path(path_minimal, "orderly.yml"))
  file.copy(orderly_file("minimal_script.R"),
            file.path(path_minimal, "script.R"))

  path_other <- file.path(path, "src", "other")
  dir.create(path_other)
  writeLines(c(readLines(orderly_file("other_report.yml")), extra2),
             file.path(path_other, "orderly.yml"))
  file.copy(orderly_file("other_script.R"),
            file.path(path_other, "script.R"))

  ids <- character()

  config <- orderly_config(path)
  ids[[1]] <- orderly_run("minimal", config = config, echo = FALSE)
  ids[[2]] <- orderly_run("minimal", config = config, echo = FALSE)
  ids[[3]] <- orderly_run("other", list(nmin = 0), config = config,
                          echo = FALSE)

  ## Update the db
  fake_db(con, 2)
  DBI::dbDisconnect(con)
  ids[[4]] <- orderly_run("minimal", config = config, echo = FALSE)
  ids[[5]] <- orderly_run("other", list(nmin = 0), config = config,
                          echo = FALSE)
  ids[[6]] <- orderly_run("other", list(nmin = 0.5), config = config,
                          echo = FALSE)
  ## Update the code
  txt <- readLines(file.path(path_other, "script.R"))
  writeLines(c("extract$number <- extract$number * 1.2", txt),
             file.path(path_other, "script.R"))

  ids[[7]] <- orderly_run("other", list(nmin = 0), config = config,
                          echo = FALSE)
  ## Then let's create a series of times and update things.  Push the
  ## times back through to ~1 week ago
  dt <- sort(runif(length(ids), 0, 60 * 60 * 24 * 7), decreasing = TRUE)
  time0 <- as.POSIXct("2017-07-12 08:28:23 BST")
  time <- time0 - dt

  fixup <- function(id, time, root) {
    id_new <- new_report_id(time)
    name <- orderly_find_name(id, root)
    date <- sprintf("date: %s", as.character(time))
    p <- file.path(path_draft(root), name, id_new)
    stopifnot(file.rename(file.path(path_draft(root), name, id), p))

    yml <- path_orderly_run_yml(p)
    dat <- yaml_read(yml)
    dat$date <- as.character(time)
    dat$id <- id_new
    writeLines(yaml::as.yaml(dat), yml)

    orderly_commit(id_new, name, path)
    id_new
  }

  res <- vcapply(seq_along(ids), function(i) fixup(ids[[i]], time[[i]], path))

  for (id in res[c(2, 6, 7)]) {
    orderly_publish(id, config = config)
  }

  path
}

fake_db <- function(con, seed = 1) {
  set.seed(seed)

  id <- ids::adjective_animal(20)
  n <- 200

  d <- data.frame(id = seq_along(id),
                  name = id,
                  number = runif(length(id)))
  DBI::dbWriteTable(con, "thing", d, overwrite = TRUE)

  d <- data.frame(id = seq_len(n),
                  thing = sample(length(id), n, replace = TRUE),
                  value = rnorm(n),
                  stringsAsFactors = FALSE)
  DBI::dbWriteTable(con, "data", d, overwrite = TRUE)
}
