do_plot <- function(d) {
  barplot(setNames(d$number, d$name), las = 2)
}
