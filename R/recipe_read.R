recipe_read <- function(path) {
  filename <- file.path(path, "orderly.yml")
  if (!file.exists(filename)) {
    stop("Did not find file 'orderly.yml' at path ", path)
  }
  info <- yaml_read(filename)

  required <- c("script", # filename
                "requester", # is this blank?  other information here
                "artefacts",
                "author",
                "data")
  optional <- c("parameters", # character >= 1
                "comment",    # character
                "views",
                "packages",
                "resources")
  check_fields(info, filename, required, optional)

  fieldname <- function(name) {
    sprintf("%s:%s", filename, name)
  }

  check_artefact <- function(nm) {
    v <- c("format", "description")
    x <- info$artefacts[[nm]]
    check_fields(x, sprintf("%s:artefacts:%s", filename, nm), v, NULL)
    for (i in v) {
      assert_character(x[[i]], fieldname(sprintf("artefacts:%s:%s", nm, i)))
    }
    c(name = nm, unlist(x[v]))
  }
  if (length(info$artefacts) == 0L) {
    stop("At least one artefact required")
  }
  assert_named(info$artefacts, TRUE)
  info$artefacts <-
    t(vapply(names(info$artefacts), check_artefact, character(3)))
  rownames(info$artefacts) <- NULL

  assert_scalar_character(info$script, fieldname("script"))
  assert_scalar_character(info$author, fieldname("author"))
  assert_scalar_character(info$requester, fieldname("requester"))

  if (!is.null(info$comment)) {
    assert_scalar_character(info$comment, fieldname("comment"))
  }
  if (!is.null(info$parameters)) {
    assert_character(info$parameters, fieldname("parameters"))
  }
  if (!is.null(info$packages)) {
    assert_character(info$packages, fieldname("packages"))
  }
  if (!is.null(info$resources)) {
    assert_character(info$resources, fieldname("resources"))
    msg <- info$resources[file.exists(info$resources)]
    if (length(msg) > 0L) {
      stop("Declared resources missing: ", paste(msg, collapse = ", "))
    }
    ## TODO: this is not quite right because the files need to be
    ## tested (as done here) with names within that directory.
    err <- info$resources[!is_within_dir(file.path(path, info$resources), path)]
    if (length(msg) > 0L) {
      stop("Declared resources not in right place: ",
           paste(err, collapse = ", "))
    }
    ## TODO: At this point we should also return the normalised
    ## *relative* path perhaps to prevent valid, but absolute, paths
    ## being used.
  }

  ## Then some processing:
  assert_named(info$data, TRUE, fieldname("data"))
  info$data <- string_or_filename(info$data, path, fieldname("data"))
  info$resources <- c(info$resources, attr(info$data, "files"))
  if (!is.null(info$views)) {
    assert_named(info$data, TRUE, fieldname("data"))
    info$views <- string_or_filename(info$views, path, fieldname("views"))
    info$resources <- c(info$resources, attr(info$views, "files"))
    info$views <- paste0(sprintf("CREATE TEMPORARY VIEW %s AS\n",
                                 names(info$views)), info$views)
  }

  if (!file.exists(file.path(path, info$script))) {
    stop(sprintf("script file %s does not exist", info$script))
  }

  ## TODO: we probably need to reference any additional on-disk
  ## resources here too
  info$script_hash <- unname(tools::md5sum(file.path(path, info$script)))
  info$hash <- digest::digest(info)
  ## Must add this after hashing
  info$path <- path

  info
}

valid_formats <- function() {
  c("png", "pdf", "html", "csv", "multi")
}

string_or_filename <- function(x, path, name) {
  if (is.list(x)) {
    if (all(vlapply(x, is.character))) {
      x <- vcapply(x, identity)
    }
  }
  assert_character(x, name)
  i <- grepl("\\.sql$", x)
  if (any(i)) {
    files <- x[i]
    msg <- !file.exists(file.path(path, files))
    if (any(msg)) {
      stop(sprintf("File for %s does not exist: %s",
                   name, paste(msg, collapse = ", ")))
    }
    x[i] <- vcapply(file.path(path, files), read_lines)
    attr(x, "files") <- unname(files)
  }
  x
}
