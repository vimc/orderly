Vertex <- R6::R6Class("Vertex",
  public = list(
    parent = NULL,
    children = list(),
    name = NULL,
    id = NULL,
    out_of_date = NULL,
    initialize = function(parent, name, id, out_of_date) {
      # add some type safety here
      self$parent <- parent
      self$name <- name
      self$id <- id
      self$out_of_date <- out_of_date
    },
    add_child = function(child) {
      self$children <- append(self$children, list(child))
    },
    format = function() {
      sprintf("%s [%s]", self$name, self$id)
    }
  )
)


Tree <- R6::R6Class("Tree",
  private = list(
    message = NULL,
    direction = NULL,
    vertices = NULL,
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
    ##   of the form "│       │   " (i.e. some combination of "│   " and "    ")
    ## * tree_string is final printed string (with line breaks and colouring)
    format_helper = function(vertex = self$root,
                             prefix = "",
                             tree_string = "") {
      ## if the tree has a warning message, append it to the front in the red
      if (!is.null(private$message)) {
        tree_string <- paste(tree_string, crayon::red(private$message), "\n",
                             collapse = "")
        private$message <- NULL
      }

      # append the current vertex to the end of the print string
      console_colour <-
                       if (vertex$out_of_date) {crayon::red} else {crayon::blue}
      tree_string <- sprintf("%s%s\n", tree_string,
                                       console_colour(vertex$format()))

      number_children <- length(vertex$children)
      if (number_children > 0) { ## print children in necessary
        i <- 1 ## we need to keep track of when we are at the last child
        for (child in vertex$children) {
          is_last <- (i == number_children)

          console_colour <-
                        if (child$out_of_date) {crayon::red} else {crayon::blue}

          ## the start of the line = prefix + either └── or ├──
          line_prefix <- sprintf("%s%s--", prefix,
                                           if (is_last) {"+`"} else {"+"})

          tree_string <- sprintf("%s%s", tree_string,
                                         console_colour(line_prefix))

          ## increase indentation with either "│   " or "    " (4 characters!)
          prefix <- sprintf("%s%s   ", prefix,  if (is_last) {" "} else {"|"})

          tree_string <- private$format_helper(child, prefix, tree_string)

          ## decrease the indentation by 4 characters
          prefix <- substr(prefix, 1, nchar(prefix) - 4)
          i <- i + 1
        }
      }
      tree_string
    }
  ),
  public = list(
    root = NULL,
    initialize = function(root, direction) {
      private$vertices <- append(private$vertices, list(root))
      self$root <- root
      private$message <- NULL
      private$direction <- direction
    },
    add_child = function(parent, name, id, out_of_date) {
      child <- Vertex$new(parent, name, id, out_of_date)
      parent$add_child(child)
      private$vertices <- append(private$vertices, list(child))
      child
    },
    set_message = function(new_message) {
      old_message <- private$message
      private$message <- new_message
      old_message
    },
    get_direction = function() {
      private$direction
    },
    format = function() {
      private$format_helper()
    }
  )
)
