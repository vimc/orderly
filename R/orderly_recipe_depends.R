recipe_validate_depends <- function(depends, config, filename) {
  if (is.null(depends)) {
    return(NULL)
  }

  ## Deal with yaml weirdness:
  if (is.null(names(depends))) {
    depends <- ordered_map_to_list(depends)
  }

  for (i in seq_along(depends)) {
    depends[[i]]$index <- i
    depends[[i]]$name <- names(depends)[[i]]
  }

  rbind_df(lapply(depends, recipe_validate_depend1, filename))
}


recipe_validate_depend1 <- function(depend, filename) {
  name <- depend$name
  v <- c("id", "use", "index", "name")
  check_fields(depend, sprintf("%s:depends:%s", filename, name), v, "draft")

  assert_character(depend$id, sprintf("%s:depends:%s:id", filename, name))
  if (!is.null(depend$draft)) {
    msg <- c("Using 'draft:' within an ordery.yml is deprecated and",
             "will be removed in a future version of orderly.  Please",
             "use the 'use_draft' argument to control draft usage.",
             "If you want to use a recent version of a report that you",
             'are developing simultaneously, use_draft = "newer"',
             "will probably do what you want.")
    orderly_warning(flow_text(msg))
    assert_scalar_logical(depend$draft,
                          sprintf("%s:depends:%s:draft", filename, name))
  } else {
    depend$draft <- NA
  }

  assert_named(depend$use, TRUE, sprintf("%s:depends:%s:use", filename, name))
  err <- !vlapply(depend$use, function(x) is.character(x) && length(x) == 1)
  if (any(err)) {
    stop(sprintf("%s:depends:%s:use must all be scalar character",
                 filename, name),
         call. = FALSE)
  }

  depend$filename <- list_to_character(depend$use, FALSE)
  depend$as <- names(depend$use)
  depend$use <- NULL
  depend$is_pinned <- depend$id != "latest"

  ## Bit of a faff here to get the format into something that will
  ## serialise and interrogate nicely.
  as_data_frame(depend)
}


recipe_resolve_dependencies <- function(self, use_draft, parameters, remote) {
  if (is.null(self$depends)) {
    return(NULL)
  }

  if (!is.null(remote)) {
    if (use_draft) {
      stop("Can't use 'use_draft' with remote")
    }
    remote <- get_remote(remote, self$config)
  }

  depends_split <- unname(split(self$depends, self$depends$index))
  for (i in seq_along(depends_split)) {
    name <- depends_split[[i]]$name[[1]]
    id <- depends_split[[i]]$id[[1]]
    filename <- depends_split[[i]]$filename
    if (is.null(remote)) {
      ## This is ugly but needs to stay for another couple of versions
      use_draft_i <- depends_split[[i]]$draft[[1]]
      use_draft_i <- if (is.na(use_draft_i)) use_draft else use_draft_i
      res <- resolve_dependencies_local(id, name, self$config, parameters,
                                        use_draft_i)
    } else {
      res <- resolve_dependencies_remote(id, name, self$config, remote)
    }
    info <- resolve_dependencies_validate(id, name, res$path, filename)
    extra <- as_data_frame(c(res, info))
    depends_split[[i]] <- cbind(depends_split[[i]], extra)
  }

  self$depends <- rbind_df(depends_split)
}
