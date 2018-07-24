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
      data <- slack_data(dat, server$server$name, server$server$url_www,
                         isTRUE(server$primary))
      do_slack_post_success(slack_url, data)
    }
  }
}


slack_data <- function(dat, server_name, server_url, server_is_primary) {
  elapsed <- format(as.difftime(dat$elapsed, units = "secs"), digits = 2)
  if (is.null(dat$git)) {
    git <- NULL
  } else {
    git <- sprintf("%s@%s", dat$git$branch, dat$git$sha_short)
  }
  id <- dat$id

  report_url <- sprintf("%s/reports/%s/%s/", server_url, dat$name, id)
  title <- sprintf("Ran report '%s'", dat$name)
  text <- sprintf("On server '%s' in %s", server_name, elapsed)
  fallback <- sprintf("Ran '%s' as '%s'; view at %s", dat$name, id, report_url)
  ## NOTE: 'warning' is actually quite a nice yellow colour
  col <- if (server_is_primary) "good" else "warning"

  fields <- list(list(title = "id", value = sprintf("`%s`", id), short = TRUE))
  if (!is.null(git)) {
    fields <- c(fields, list(list(title = "git", value = git, short = TRUE)))
  }

  list(username = "orderly",
       icon_emoji = ":ambulance:",
       attachments = list(list(
         title = title,
         text = text,
         color = col,
         fallback = fallback,
         fields = fields,
         actions = list(list(
           type = "button",
           text = ":clipboard: View report",
           style = "primary",
           url = report_url)))))
}


do_slack_post_success <- function(slack_url, data) {
  if (!requireNamespace("httr", quietly = TRUE)) {
    message("not sending messages as httr is not available")
    return(invisible(NULL))
  }

  ## Never fail on sending the hook - but send an informational
  ## message instead.
  r <- tryCatch({
    r <- httr::POST(slack_url, body = data, encode = "json")
    httr::stop_for_status(r)
    r
  }, error = function(e) {
    message("NOTE: running slack hook failed\n", e$message)
    r
  })
  invisible(r)
}
