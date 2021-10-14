orderly_graph_src <- function(name, config, direction = "downstream",
                              max_depth = Inf, recursion_limit = 100,
                              ref = NULL, validate = TRUE) {
  ## Start by reading all the src files; this would ideally be done
  ## with some caching layer I think.  The other option would be to
  ## make sure that we can rip through this really fast by not
  ## validating everything on the recipe read (just migrating) then
  ## dealing with just the depends.
  if (!is.null(ref)) {
    assert_has_git(config$root)
  }
  all_reports <- orderly_list_internal(config, ref)

  missing_reports <- name[!(name %in% all_reports)]
  if (length(missing_reports > 0)) {
    ref_msg <- ""
    if (!is.null(ref)) {
      ref_msg <- sprintf(" at git ref '%s'", ref)
    }
    stop(sprintf("%s %s%s.",
                 ngettext(length(missing_reports), "Unknown source report",
                          "Unknown source reports"),
                 paste0(paste0("'", missing_reports, "'"), collapse = ", "),
                 ref_msg))
  }

  if (validate) {
    if (!is.null(ref)) {
      stop("Non-null ref not supported when reading yml with validate = TRUE")
    }
    get_dependencies <- function(name) {
      deps <- orderly_recipe$new(name, config, develop = TRUE)$depends
      if (length(deps) > 0) {
        unique(data_frame(name = name, id = NA_character_, child = deps$name,
                          date = NA_character_, child_id = deps$id,
                          out_of_date = FALSE))
      } else {
        data_frame(name = character(0), id = character(0), child = character(0),
                   date = character(0), child_id = character(0),
                   out_of_date = logical(0))
      }
    }
    deps <- lapply(all_reports, get_dependencies)
    deps <- do.call(rbind.data.frame, deps)
  } else {
    if (is.null(ref)) {
      ## TODO: Bypass git here - will be slower than just reading from current
      ## checked out branch
      ref <- "HEAD"
    }
    deps <- lapply(all_reports, read_yml_quick, config, ref)
    names(deps) <- all_reports
  }

  ## Conveying the version information here is super difficult; if
  ## we're interested in how 'example' is used in downstream reports
  ## then the identifier information is tricky.
  if (direction == "downstream") {
    ## Invert the dependency tree - this leaves ids in a weird place
    ## but I guess that is ok.
    deps <- data_frame(
      name = deps$child,
      id = deps$child_id,
      date = deps$date,
      child = deps$name,
      child_id = deps$id,
      out_of_date = FALSE
    )
  }

  ## We then can work with this fairly easily I think
  roots <- lapply(name, function(nm) {
    list(name = name)
  })
  tree <- report_tree$new(roots, direction, type = "src", depth = max_depth)
  tree$add_edges(deps)
  tree
}


read_yml_quick <- function(name, config, ref) {
  path <- file.path("src", name, "orderly.yml")
  lines <- git_show(path, ref = ref, root = config$root, check = TRUE)
  raw <- yaml_load(lines$output)
  yml <- recipe_migrate(raw, config, path)
  depends <- yml$depends
  if (is.null(depends)) {
    return(NULL)
  }
  ## Deal with yaml weirdness:
  if (is.null(names(depends))) {
    depends <- ordered_map_to_list(depends)
  }
  deps <- lapply(names(depends), function(dep) {
    list(name = dep,
         id = depends[[dep]]$id)
  })
  do.call(rbind.data.frame, unique(deps))
}


build_tree_src <- function(parent, deps, depth, limit, seen = NULL) {
  if (limit < 0) {
    stop("The tree is very large or degenerate.")
  }
  name <- parent$name
  if (any(seen == name)) {
    loop <- c(seen[which(seen == name):length(seen)], name)
    stop(paste("Detected circular dependency:",
               paste(squote(loop), collapse = " -> ")),
         call. = FALSE)
  }
  if (is.null(deps[[name]]) || depth == 0) {
    return(NULL)
  }
  d <- deps[[name]]
  for (i in seq_len(NROW(d))) {
    child <- report_vertex$new(parent, d$name[[i]], d$id[[i]], FALSE)
    parent$add_child(child)
    build_tree_src(child, deps, depth - 1, limit - 1, c(seen, name))
  }
  child
}
