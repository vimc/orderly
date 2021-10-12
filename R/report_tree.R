report_tree <- R6::R6Class(
  "report_tree",
  private = list(
    message = NULL,
    ## recursive tree printer, the output is of the form
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
    format_helper = function(name, id, prefix, tree_string, chars) {
      ## if the tree has a warning message, append it to the front in the red
      if (!is.null(private$message)) {
        tree_string <- paste(tree_string, crayon::red(private$message), "\n",
                             collapse = "")
        private$message <- NULL
      }

      # append the current vertex to the end of the print string
      console_colour <- if (FALSE) crayon::red else crayon::blue
      tree_string <- sprintf("%s%s\n", tree_string,
                                       console_colour(sprintf("%s [%s]", name, id)))

      children <- self$edges[self$edges$name == name & self$edges$id == id, ]
      number_children <- nrow(children)
      if (number_children > 0) { ## print children in necessary
        for (i in seq_len(number_children)) {
          is_last <- (i == number_children)

          child <- children[i, ]
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

          tree_string <- private$format_helper(
            child$child, child$child_id, prefix, tree_string, chars)

          ## decrease the indentation by 4 characters
          prefix <- substr(prefix, 1, nchar(prefix) - 4)
        }
      }
      tree_string
    }
  ),
  public = list(
    roots = NULL,
    direction = NULL,
    depth = NULL,
    edges = NULL,
    initialize = function(roots, direction, depth) {
      self$roots <- roots
      self$direction <- direction
      self$depth <- depth
      self$edges <- data.frame(name = character(0),
                               id = character(0),
                               child = character(0),
                               child_id = character(0),
                               out_of_date = character(0))
    },
    add_edges = function(edges) {
      if (!(all(colnames(self$edges) %in% colnames(edges)))) {
        stop("Edges does not match expected colnames, must have columns %s",
             paste0(colnames(edges), collapse = ", "))
      }
      self$edges <- rbind(self$edges, edges)
    },
    # add_child = function(parent, name, id, out_of_date) {
    #   child <- report_vertex$new(parent, name, id, out_of_date)
    #   parent$add_child(child)
    #   child
    # },
    set_message = function(new_message) {
      old_message <- private$message
      private$message <- new_message
      old_message
    },
    format = function(utf8 = NULL) {
      names <- vcapply(self$roots, "[[", "name")
      ids <- vcapply(self$roots, "[[", "id")
      chars <- tree_chars(utf8)
      chars <- lapply(self$roots, function(x) {
        chars
      })
      strings <- Map(private$format_helper, names, ids, "", "", chars)
      paste0(strings, collapse = "\n")
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
