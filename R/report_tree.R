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
    vertices = NULL,
    # this is stupidly hacky to get the formatting right
    format_helper = function(vertex = self$root, fvector = c(), str="") {
      ## if the tree has a warning message, append it to the front in the red
      if (!is.null(private$message)) {
        str <- paste(str, crayon::red(private$message), "\n", collapse = "")
        private$message <- NULL
      }

      console_colour <- if (vertex$out_of_date) {crayon::red} else {crayon::blue}

      if (length(fvector) == 0) {
        str <- paste(str, console_colour(sprintf("%s", vertex$format())), "\n", collapse = "")
      } else {
        spacing <- paste(vapply(head(fvector, -1),
                                function(x) { if (x) {"| "} else {"  "}},
                                character(1)),
                         collapse = "")


        str <- paste(str, console_colour(sprintf("%s|___%s", spacing, vertex$format())), "\n", collapse = "")
      }

      number_children <- length(vertex$children)
      if (number_children > 0) {
        i <- 1 ## we need to keep track of when we are at the end of the vector
        for (child in vertex$children) {
          is_last <- (i != number_children)
          str <- private$format_helper(child, c(fvector, is_last), str)
          i <- i + 1
        }
      }
      str
    }
  ),
  public = list(
    root = NULL,
    initialize = function(root) {
      private$vertices <- append(private$vertices, list(root))
      self$root <- root
      private$message <- NULL
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
    format = function() {
      private$format_helper()
    }
  )
)
