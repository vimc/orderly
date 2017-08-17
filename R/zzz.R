cache <- new.env(parent = emptyenv())
.onLoad <- function(...) {
  cache$exiftool <- exiftool_locate()
}
