changelog_parse <- function(txt) {
  if (length(txt) == 0L) {
    return(data_frame(type = character(0), value = character(0)))
  }
  re_header <- "^\\[(.+?)\\]\\s*$"
  i <- grep(re_header, txt)
  if (length(i) == 0L || i[[1L]] != 1L) {
    stop("Invalid changelog - first line is not a label", call. = FALSE)
  }
  type <- sub(re_header, "\\1", txt[i])

  n <- diff(c(i, length(txt) + 1L)) - 1L
  err <- i[n == 0L]
  if (length(err) == 1L) {
    stop("Invalid changelog - empty entry on line ", err)
  } else if (length(err) > 0L) {
    stop("Invalid changelog - empty entries on lines ",
         paste(err, collapse = ", "))
  }

  j <- c(i[-1] - 1, length(txt))
  value <- Map(function(i, j) paste(txt[i:j], collapse = "\n"), i + 1L, j)

  data_frame(type = type, value = list_to_character(value))
}
