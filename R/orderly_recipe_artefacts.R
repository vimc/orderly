## TODO(VIMC-3519): this copies over all the logic from before but
## there are some things I would like to see handled: we should
## simplify the format, and move format into the body like
##
## artefacts:
##   - format: staticgraph
##     description: A graph of things
##     filenames:
##       - filename.png
##
## Which is simpler and will be easier to edit.
recipe_validate_artefacts <- function(artefacts, config, filename) {
  if (length(artefacts) == 0L) {
    stop("At least one artefact required")
  }

  if (is.character(artefacts)) {
    msg <- c("Your artefacts are misformatted.  You must provide a 'type'",
             "and a description for each, and each logical artefact may",
             "contain multiple files.  For example, you might use",
             "",
             "artefacts:",
             "  - data:",
             "      description: These are data for x, y, z",
             "      filenames:",
             sprintf("        - %s", artefacts),
             "",
             sprintf("other alternatives to 'data' are %s",
                     paste(squote(setdiff(valid_formats(), "data")),
                           collapse = ", ")))
    stop(paste(msg, collapse = "\n"), call. = FALSE)
  }

  if (!is.null(names(artefacts)) && length(artefacts) != 1L) {
    if (any(names(artefacts) %in% valid_formats())) {
      artefacts <- utils::tail(artefacts, 3)
      correct <- list(artefacts = lapply(seq_along(artefacts),
                                         function(i) artefacts[i]))
      msg <- c("Your artefacts look incorrectly formatted; they must be",
               "an _ordered map_.  Currently you have something like",
               "",
               indent(yaml::as.yaml(list(artefacts = artefacts)), 4),
               "",
               "but you should reformat that as something like",
               "",
               indent(yaml::as.yaml(correct), 4),
               "",
               "otherwise with duplicate entries with the same report type",
               "your yaml will be invalid (this format is permitted for",
               "single artefacts only)")
      message(paste(msg, collapse = "\n"))
    }
    stop("Expected an ordered map!")
  }

  if (is.null(names(artefacts)) && all(lengths(artefacts) == 1L)) {
    artefacts <- ordered_map_to_list(artefacts)
  }

  assert_named(artefacts, FALSE, "artefacts")
  ## Then this part, which is basically going to become a migration
  ## once we change the format, but which doing here simplifies the
  ## next bit:
  for (i in seq_along(artefacts)) {
    artefacts[[i]]$format <- names(artefacts)[[i]]
    artefacts[[i]]$index <- i
  }
  artefacts <- unname(artefacts)

  res <- t(vapply(artefacts, recipe_validate_artefact1,
                  vector("list", 3L), config, filename))
  colnames(res) <- c("filenames", "description", "format")

  filenames <- unlist(res[, "filenames"])
  dups <- unique(filenames[duplicated(filenames)])
  if (length(dups) > 0L) {
    stop("Duplicate artefact filenames are not allowed: ",
         paste(squote(dups), collapse = ", "))
  }

  if (any(grepl("^README(|.md)$", filenames, ignore.case = TRUE))) {
    stop("README.md should not be listed as an artefact")
  }

  res
}


recipe_validate_artefact1 <- function(artefact, config, filename) {
  index <- artefact$index

  ## NOTE: this means that we silently ignore the format and index
  ## fields if present, which they probably will not be
  v <- c("filenames", "description", "format", "index")
  check_fields(artefact, sprintf("%s:artefacts[%d]", filename, index), v, NULL)

  assert_scalar_character(
    artefact$description,
    sprintf("%s:artefacts[%d]$description:%s", filename, index))
  assert_character(
    artefact$filenames,
    sprintf("%s:artefacts[%d]$filenames:%s", filename, index))

  if (!(artefact$format %in% valid_formats())) {
    stop(sprintf(
      "Unknown artefact type: '%s' for '%s:artefacts[%d]'; should be one of %s",
      artefact$format, filename, artefact$index,
      paste(squote(valid_formats()), collapse = ", ")),
      call. = FALSE)
  }

  artefact[c("filenames", "description", "format")]
}


valid_formats <- function() {
  c("staticgraph", "interactivegraph", "data", "report", "interactivehtml")
}
