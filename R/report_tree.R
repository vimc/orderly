report_tree <- R6::R6Class(
  "report_tree",
  private = list(
    message = NULL,

    ## recursive tree printer for a single root,
    ## the output is of the form
    ## report A [20190416-114345-c44facf2]
    ## ├──report B [20190416-130945-4a2ba689]
    ## │   ├──report C [20190416-131150-eb04cd40]
    ## │   ├──report D [20190416-131204-59cd1b3a]
    ## │   └──report E [20190924-155201-93c00275]
    ## ├──report F [20190416-131351-3317b336]
    ## │   └──report G [20190416-131150-eb04cd40]
    ## └──report H [20190924-155201-93c00275]
    ##
    ## * vertex is the current vertex to be printed
    ## * prefix is a offset string to get the indentation correct - it will be
    ##   of the form "│       │   " (i.e. some combination of "│   "
    ##   and "    ")
    ## * tree_string is final printed string (with line breaks and colouring)
    format_helper = function(vertex, prefix, tree_string, chars) {
      # append the current vertex to the end of the print string
      console_colour <- if (FALSE) crayon::red else crayon::blue
      if (is.null(vertex$id) || is.na(vertex$id)) {
        id <- ""
      } else {
        id <- sprintf(" [%s]", vertex$id)
      }
      tree_string <- sprintf("%s%s\n", tree_string, console_colour(
        sprintf("%s%s", vertex$name, id)))

      children <- private$get_children(vertex, self$show_all)
      number_children <- length(children)
      for (i in seq_along(children)) {
        is_last <- (i == number_children)

        child <- children[[i]]
        console_colour <-
          if (child$out_of_date) crayon::red else crayon::blue

        ## the start of the line = prefix + either +-- or `--
        line_prefix <- paste0(prefix,
                              chars[[if (is_last) "leaf_last" else "leaf"]])
        tree_string <- sprintf("%s%s", tree_string,
                               console_colour(line_prefix))

        ## increase indentation with either "|   " or "    " (4 characters!)
        vertical <- if (is_last) " " else chars[["vertical"]]
        prefix <- sprintf("%s%s   ", prefix, vertical)

        tree_string <- private$format_helper(child, prefix, tree_string, chars)

        ## decrease the indentation by 4 characters
        prefix <- substr(prefix, 1, nchar(prefix) - 4)
      }
      tree_string
    },

    get_children = function(vertex, show_all) {
      if (is.na(vertex$name)) {
        return(NULL)
      }
      if (self$type == "src") {
        ## We only care about names in src mode as ids have not
        ## been generated yet
        edges_to_keep <- self$edges$name == vertex$name
      } else {
        edges_to_keep <- self$edges$name == vertex$name &
          self$edges$id == vertex$id
      }
      children <- self$edges[edges_to_keep, ]
      if (!show_all) {
        child <- unique(children$child)
        latest_children <- lapply(child, function(name) {
          x <- children[children$child == name, ]
          latest <- x[x$date == max(x$date), ]
          ## Edge case where 2 reports have the exact same date time
          ## I think this is probably not going to happen in real use
          ## but does happen in tests so we need to deal with it
          ## Use the last one as this is the most recently added to the db
          if (nrow(latest) > 1) {
            latest <- latest[nrow(latest), ]
          }
          latest
        })
        children <- do.call(rbind.data.frame, latest_children)
      }

      lapply(seq_len(nrow(children)), function(row) {
        list(name = children[row, "child"],
             id = children[row, "child_id"],
             out_of_date = children[row, "out_of_date"])
      })
    },

    vertex_to_dep = function(vertex, show_all) {
      children <- private$get_children(vertex, show_all)
        list(
          name = vertex$name,
          id = vertex$id,
          out_of_date = vertex$out_of_date,
          dependencies = lapply(children, private$vertex_to_dep, show_all)
        )
    }
  ),

  public = list(
    roots = NULL,
    direction = NULL,
    depth = NULL,
    edges = NULL,
    show_all = TRUE,
    type = NULL,
    initialize = function(roots, direction, depth, type = NULL,
                          show_all = TRUE) {
      self$roots <- roots
      self$direction <- direction
      self$depth <- depth
      self$edges <- data_frame(name = character(0),
                               id = character(0),
                               date = character(0),
                               child = character(0),
                               child_id = character(0),
                               out_of_date = logical(0))
      self$show_all <- show_all
      self$type <- type
    },

    add_edges = function(edges) {
      if (!(all(colnames(self$edges) %in% colnames(edges)))) {
        stop(sprintf(
          "Edges does not match expected colnames, must have columns %s",
          paste0(colnames(self$edges), collapse = ", ")))
      }
      self$edges <- unique(rbind(self$edges, edges))
    },

    set_message = function(new_message) {
      old_message <- private$message
      private$message <- new_message
      old_message
    },

    get_tree = function(root_name, show_all = TRUE) {
      root_names <- vcapply(self$roots, "[[", "name")
      if (!(root_name %in% root_names)) {
        stop("Cannot build tree from non root node")
      }
      root <- self$roots[[which(root_name == root_names)]]
      private$vertex_to_dep(root, show_all = show_all)
    },

    format = function(utf8 = NULL) {
      ## if the tree has a warning message, append it to the front in red
      header <- NULL
      if (!is.null(private$message)) {
        header <- crayon::red(private$message)
      }
      strings <- vcapply(self$roots, private$format_helper, "", "",
                         tree_chars(utf8))
      paste0(c(header, strings), collapse = "\n")
    }
  )
)


## https://en.wikipedia.org/wiki/Box-drawing_character
tree_chars <- function(utf8 = NULL) {
  utf8 <- utf8 %||% l10n_info()[["UTF-8"]]
  if (utf8) {
    chars <- c(join = "\U251C",
               join_last = "\U2514",
               vertical = "\U2502",
               horizontal = "\U2500")
  } else {
    chars <- c(join = "+",
               join_last = "`",
               vertical = "|",
               horizontal = "-")
  }

  chars[["leaf"]] <- sprintf("%s%s%s", chars[["join"]],
                             chars[["horizontal"]], chars[["horizontal"]])
  chars[["leaf_last"]] <- sprintf("%s%s%s", chars[["join_last"]],
                                  chars[["horizontal"]], chars[["horizontal"]])

  chars
}
