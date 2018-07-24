## Send slack messages!
slack_post_success <- function(dat, config) {
  if (!is.null(config$api_server_identity)) {
    server <- config$api_server[[config$api_server_identity]]
    slack_url <- resolve_secrets(server$slack_url, config)[[1L]]
    if (!is.null(slack_url)) {
      elapsed <- format(as.difftime(dat$elapsed, units = "secs"), digits = 2)
      if (is.null(dat$git)) {
        git <- NULL
      } else {
        git <- sprintf("%s@%s", dat$git$branch, dat$git$sha_short)
      }
      do_slack_post_success(slack_url,
                            server$server$name,
                            server$server$url_www,
                            isTRUE(server$primary),
                            dat$name,
                            dat$id,
                            git,
                            elapsed)
    }
  }
}


do_slack_post_success <- function(slack_url,
                                  server_name, server_url, server_is_primary,
                                  name, id, git, elapsed) {
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

  fields <- list(
    list(title = "server", value = server_name, short = TRUE),
    list(title = "id", value = id, short = TRUE),
    list(title = "elapsed", value = elapsed, short = TRUE))
  if (!is.null(git)) {
    fields <- c(fields, list(list(title = "git", value = git, short = TRUE)))
  }

  data <- list(username = unbox("orderly"),
               icon_emoji = unbox(":ambulance:"),
               attachments = list(list(
                 title = unbox(title),
                 color = unbox(col),
                 fallback = unbox(fallback),
                 fields = fields,
                 actions = list(list(
                   type = unbox("button"),
                   text = unbox(":clipboard: View report"),
                   style = unbox("primary"),
                   url = unbox(report_url))))))
  ## Never fail on sending the hook - but send an informational
  ## message instead.
  tryCatch({
    r <- httr::POST(slack_url, body = data, encode = "json")
    httr::stop_for_status(r)
  }, error = function(e) message("NOTE: issue running slack hook\n", e$message))
  invisible(NULL)
}
