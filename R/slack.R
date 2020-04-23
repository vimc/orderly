## Send slack messages!

post_success <- function(dat, config) {
  opts <- config$server_options()

  response <- NULL
  if (!is.null(opts)) {
    remote <- get_remote(opts$name, config)
    opts <- resolve_env(opts, error = FALSE)

    ## TODO(VIMC-3544): This moves into the object itself, using some
    ## sort of data field, so we might use remote$data$slack_url and
    ## remote$data$primary
    slack_url <- opts$slack_url
    teams_url <- opts$teams_url

    report_url <- remote$url_report(dat$meta$name, dat$meta$id)
    response <- list()
    if (!is.null(slack_url)) {
      assert_scalar_character(slack_url, "slack_url")
      data <- slack_data(dat, opts$name, report_url, opts$primary)
      response$slack <- do_post_success(slack_url, data, "slack")
    }
    if (!is.null(teams_url)) {
      assert_scalar_character(teams_url, "teams_url")
      data <- teams_data(dat, opts$name, report_url, opts$primary)
      response$teams <- do_post_success(teams_url, data, "teams")
    }
  }
  response
}

slack_data <- function(dat, remote_name, report_url, remote_is_primary) {
  content <- prepare_content(dat, remote_name, report_url, "<{url}|{label}>",
                             escape = FALSE)
  fields <- list(list(title = "id", value = content$id, short = TRUE))

  if (!is.null(dat$git)) {
    branch <- dat$git$branch %||% "(detached)"
    sha <- dat$git$sha_short
    if (!is.null(dat$git$github_url)) {
      sha <- sprintf("<%s/tree/%s|%s>", dat$git$github_url, sha, sha)
    }
    if (!is.null(dat$git$github_url) && !is.null(dat$git$branch)) {
      branch <- sprintf("<%s/tree/%s|%s>", dat$git$github_url, branch, branch)
    }
    fields <- c(fields, list(list(title = "git",
                                  value = sprintf("%s@%s", branch, sha),
                                  short = TRUE)))
  }

  ## NOTE: 'warning' is actually quite a nice yellow colour
  col <- if (remote_is_primary) "good" else "warning"

  list(username = "orderly",
       icon_emoji = ":ambulance:",
       attachments = list(list(
         title = content$title,
         text = content$text,
         color = col,
         fallback = content$fallback,
         fields = fields,
         actions = list(list(
           name = "link",
           type = "button",
           text = ":clipboard: View report",
           style = "primary",
           url = report_url)))))
}

teams_data <- function(dat, remote_name, report_url, remote_is_primary) {
  content <- prepare_content(dat, remote_name, report_url, "[{label}]({url})",
                             escape = TRUE)
  facts <- list(list(name = "id:", value = content$id))

  if (!is.null(dat$git)) {
    branch <- dat$git$branch %||% "(detached)"
    sha <- dat$git$sha_short
    if (!is.null(dat$git$github_url)) {
      sha <- sprintf("[%s](%s/tree/%s)", sha, dat$git$github_url, sha)
    }
    if (!is.null(dat$git$github_url) && !is.null(dat$git$branch)) {
      branch <- sprintf("[%s](%s/tree/%s)", branch, dat$git$github_url, branch)
    }
    facts <- c(facts, list(list(name = "git:",
                                value = sprintf("%s@%s", branch, sha))))
  }

  ## Using same colours as slack default for "good" and "warning" messages
  col <- if (remote_is_primary) "2EB886" else "DAA038"

  list(
    "@type" = "MessageCard",
    "@context" = "http://schema.org/extensions",
    summary = content$fallback,
    themeColor = col,
    sections = list(
      list(
        activityTitle = content$title,
        activityText = content$text,
        activityImage = paste0("https://cdn.pixabay.com/photo/2017/",
                               "06/10/07/18/list-2389219_960_720.png")
      ),
      list(
        facts = facts
      )
    ),
    potentialAction = list(
      list(
        "@type" = "OpenUri",
        name = "View report",
        targets = list(
          list(
            os = "default",
            uri = report_url
          )
        )
      )
    )
  )
}

prepare_content <- function(dat, remote_name, report_url, link_format,
                            escape) {
  id <- dat$meta$id
  name <- dat$meta$name
  elapsed <- format(as.difftime(dat$meta$elapsed, units = "secs"), digits = 2)

  if (escape) {
    qname <- sprintf("`%s`", name)
  } else {
    qname <- sprintf("'%s'", name)
  }

  list(
    fallback = sprintf("Ran '%s' as '%s'; view at %s", name, id, report_url),
    title = sprintf("Ran report %s", qname),
    text = sprintf("on server *%s* in %s", remote_name, elapsed),
    id = sprintf("`%s`", id)
  )
}

do_post_success <- function(url, data, destination) {
  if (!requireNamespace("httr", quietly = TRUE)) {
    message("not sending messages as httr is not available")
    return(invisible(NULL))
  }

  ## Never fail on sending the hook - but send an informational
  ## message instead.
  r <- tryCatch({
    r <- httr::POST(url, body = data, encode = "json")
    httr::stop_for_status(r)
    r
  }, error = function(e) {
    message(sprintf("NOTE: running %s hook failed\n", destination), e$message)
    NULL
  })
  invisible(r)
}
