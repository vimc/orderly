## Send slack messages!

slack_post_success <- function(dat, config) {
  if (!is.null(config$remote_identity)) {
    remote <- config$remote[[config$remote_identity]]
    driver <- get_remote(config$remote_identity, config)
    report_url <- driver$url_report(dat$meta$name, dat$meta$id)
    slack_url <- remote$slack_url

    if (!is.null(slack_url)) {
      assert_scalar_character(slack_url, "slack_url")
      data <- slack_data(dat, remote$name, report_url, remote$primary)
      do_slack_post_success(slack_url, data)
    }
  }
}


slack_data <- function(dat, remote_name, report_url, remote_is_primary) {
  id <- dat$meta$id
  name <- dat$meta$name
  elapsed <- format(as.difftime(dat$meta$elapsed, units = "secs"), digits = 2)
  git <- dat$git

  if (!is.null(git)) {
    branch <- git$branch %||% "(detached)"
    sha <- git$sha_short
    if (!is.null(git$github_url)) {
      sha <- sprintf("<%s/tree/%s|%s>", git$github_url, sha, sha)
    }
    if (!is.null(git$github_url) && !is.null(git$branch)) {
      branch <- sprintf("<%s/tree/%s|%s>", git$github_url, branch, branch)
    }
    git <- sprintf("%s@%s", branch, sha)
  }

  title <- sprintf("Ran report '%s'", name)
  text <- sprintf("on server *%s* in %s", remote_name, elapsed)
  fallback <- sprintf("Ran '%s' as '%s'; view at %s", name, id, report_url)
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
    NULL
  })
  invisible(r)
}
