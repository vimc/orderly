## Send slack messages!
slack_post_success <- function(url, server_name, server_url, server_is_primary,
                               name, id, text, author, elapsed) {
  if (!requireNamespace("httr", quietly = TRUE)) {
    message("not sending messages as httr is not available")
    return(invisible(NULL))
  }
  unbox <- jsonlite::unbox
  report_url <- sprintf("%s/reports/%s/%s/", server_url, name, id)
  title <- sprintf("Ran report '%s'", name)
  fallback <- sprintf("Created id '%s'; view at %s", id, report_url)
  ## 'warning' is actually quite a nice yellow colour
  col <- if (server_is_primary) "good" else "warning"
  id <- sprintf("`%s`", id)

  data <- list(username = unbox("orderly"),
               icon_emoji = unbox(":ambulance:"),
               attachments = list(list(
                 title = unbox(title),
                 text = unbox(text),
                 color = unbox(col),
                 fallback = unbox(fallback),
                 fields = list(
                   list(title = "server", value = server_name, short = TRUE),
                   list(title = "id", value = id, short = TRUE),
                   list(title = "author", value = author, short = TRUE),
                   list(title = "elapsed", value = elapsed, short = TRUE)),
                 actions = list(list(
                   type = unbox("button"),
                   text = unbox(":clipboard: View report"),
                   style = unbox("primary"),
                   url = unbox(report_url))))))
  ## Never fail on sending the hook - but send an informational
  ## message instead.
  tryCatch({
    r <- httr::POST(url, body = data, encode = "json")
    httr::stop_for_status(r)
  }, error = function(e) message("NOTE: issue running slack hook\n", e$message))
  invisible(NULL)
}
