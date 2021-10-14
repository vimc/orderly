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
    src <- lapply(all_reports, orderly_recipe$new, config, develop = TRUE)
    names(src) <- all_reports
    deps <- lapply(src, function(x) unique(x$depends[c("name", "id")]))
  } else {
    if (is.null(ref)) {
      ##TODO
    } else {
      deps <- lapply(all_reports, read_yml_quick, config, ref)
      names(deps) <- all_reports
    }
  }

  ## Conveying the version information here is super difficult; if
  ## we're interested in how 'example' is used in downstream reports
  ## then the identifier information is tricky.
  if (direction == "downstream") {
    ## Invert the dependency tree - this leaves ids in a weird place
    ## but I guess that is ok.
    len <- viapply(deps, NROW, USE.NAMES = FALSE)
    parent <- unlist(lapply(deps, "[[", "name"), FALSE, FALSE)
    deps <- split(data_frame(
      name = rep(names(deps), len),
      id = unlist(lapply(deps, "[[", "id"), FALSE, FALSE)), parent)
  }

  ## We then can work with this fairly easily I think
  roots <- lapply(name, function(nm) {
    list(name = name)
  })
  tree <- report_tree$new(roots, direction, depth = max_depth)
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
