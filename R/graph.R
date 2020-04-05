orderly_graph_src <- function(name, config, direction = "downstream",
                              max_depth = 100, show_all = FALSE) {
  ## Start by reading all the src files; this would ideally be done
  ## with some caching layer I think.  The other option would be to
  ## make sure that we can rip through this really fast by not
  ## validating everything on the recipe read (just migrating) then
  ## dealing with just the depends.
  nms <- orderly_list(config)

  if (!(name %in% nms)) {
    stop(sprintf("Unknown source report '%s'", name), call. = FALSE)
  }

  src <- lapply(nms, orderly_recipe$new, config, develop = TRUE)
  names(src) <- nms

  deps <- lapply(src, function(x) unique(x$depends[c("name", "id")]))

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
  root <- report_vertex$new(NULL, name, "latest", FALSE)
  build_tree_src(root, deps, max_depth)
  report_tree$new(root, direction)
}


## TODO: no max-depth checking here yet, but I think that we can deal
## with that a different way; probably easiest to just check to see if
## we've already visited a report for now.  We could also to a
## topological sort on the deps and work with that, but that will not
## work with the more complex dependency trees.
build_tree_src <- function(parent, deps, depth) {
  if (depth < 0) {
    stop("The tree is very large or degenerate.")
  }
  name <- parent$name
  if (is.null(deps[[name]])) {
    return(NULL)
  }
  d <- deps[[name]]
  for (i in seq_len(nrow(d))) {
    child <- report_vertex$new(parent, d$name[[i]], d$id[[i]], FALSE)
    parent$add_child(child)
    build_tree_src(child, deps, depth - 1)
  }
  child
}
