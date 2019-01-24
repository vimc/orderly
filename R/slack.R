## Send slack messages!
slack_post_success <- function(dat, config) {
  if (!is.null(config$remote_identity)) {
    remote <- config$remote[[config$remote_identity]]
    slack_url <- resolve_secrets(remote$slack_url, config)[[1L]]
    if (!is.null(slack_url)) {
      data <- slack_data(dat, remote$name, remote$url, remote$primary)
      do_slack_post_success(slack_url, data)
    }
  }
}


slack_data <- function(dat, remote_name, remote_url, remote_is_primary) {
  elapsed <- format(as.difftime(dat$elapsed, units = "secs"), digits = 2)
  if (is.null(dat$git)) {
    git <- NULL
  } else {
    git <- sprintf("%s@%s", dat$git$branch, dat$git$sha_short)
    if (is.null(dat$git$github_url)) {
      git <- sprintf("%s@%s", dat$git$branch, dat$git$sha_short)
    } else {
      git <- sprintf("<%s/tree/%s|%s>@<%s/tree/%s|%s>",
                     dat$git$github_url, dat$git$branch, dat$git$branch,
                     dat$git$github_url, dat$git$sha_short, dat$git$sha_short)
    }
  }
  id <- dat$id

  report_url <- sprintf("%s/reports/%s/%s/", remote_url, dat$name, id)
  title <- sprintf("Ran report '%s'", dat$name)
  text <- sprintf("on server *%s* in %s", remote_name, elapsed)
  fallback <- sprintf("Ran '%s' as '%s'; view at %s", dat$name, id, report_url)
  ## NOTE: 'warning' is actually quite a nice yellow colour
  col <- if (remote_is_primary) "good" else "warning"

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
           name = "link",
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
