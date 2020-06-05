## It's not totally clear to me that these sorts of shenanigans will
## work all that reliably, and testing it is going to be an absolute
## horror show.  It might be somewhat eased by working in detached
## head mode because then it's easy enough to move around the tree
## without doing a whole heap of resets.
git_run <- function(args, root = NULL, check = FALSE) {
  git <- sys_which("git")
  if (!is.null(root)) {
    args <- c("-C", root, args)
  }
  res <- system3(git, args)
  if (check && !res$success) {
    stop(sprintf("Error code %d running command:\n%s",
                 res$code, paste0("  > ", res$output, collapse = "\n")))
  }
  res
}

git_detach_head_at_ref <- function(ref, root = NULL) {
  if (!git_is_clean(root)) {
    stop("working directory must be clean")
  }
  prev <- git_branch_name(root)
  if (prev == "HEAD") {
    stop("HEAD is already detached")
  }
  orderly_log("checkout", sprintf("%s; was %s", ref, prev))
  git_run(c("checkout", "--detach", ref), root = root, check = TRUE)
  prev
}

git_ref_to_sha <- function(ref, root = NULL, check = FALSE) {
  assert_scalar_character(ref)
  res <- git_run(c("rev-parse", ref), root = root, check = FALSE)
  if (res$success) {
    res$output
  } else if (check) {
    stop(sprintf("Git reference '%s' not found", ref), call. = FALSE)
  } else {
    NA_character_
  }
}

git_ref_exists <- function(ref, root = NULL) {
  assert_scalar_character(ref)
  git_run(c("merge-base", ref, "HEAD"), root = root, check = FALSE)$success
}

git_status <- function(root = NULL, ignore_untracked = FALSE) {
  args <- c("status", "--porcelain",
            if (ignore_untracked) "--untracked-files=no")
  res <- git_run(args, root = root, check = TRUE)
  res$clean <- length(res$output) == 0L
  res
}

git_branch_name <- function(root = NULL) {
  res <- git_run(c("rev-parse", "--abbrev-ref", "HEAD"),
                 root = root, check = TRUE)
  res$output
}

git_is_clean <- function(root, ignore_untracked = FALSE) {
  git_status(root, ignore_untracked)$clean
}

git_checkout_branch <- function(name, force = FALSE, root = NULL,
                                create = FALSE) {
  if (!force && !git_is_clean(root)) {
    stop("working directory must be clean")
    ## if force and unclean we might want to do a hard reset?
  }
  prev <- git_branch_name(root)
  ##      ^ this does not return anything sensible when we were in
  ##        detached head state; detect HEAD and get the hash perhaps?
  args <- c("checkout", if (create) "-b", name)
  orderly_log("git", sprintf("checkout %s; was %s", name, prev))
  git_run(args, root = root, check = TRUE)
  prev
}

git_fetch <- function(root = NULL) {
  orderly_log("git", "fetch")
  git_run("fetch", root = root, check = TRUE)
}

git_pull <- function(root = NULL) {
  orderly_log("git", "pull")
  git_run("pull", root = root, check = TRUE)
}

git_branches_no_merged <- function(root = NULL, include_master = FALSE) {
  branches <- git_run(c("for-each-ref", "refs/remotes/origin",
                        "--sort=-committerdate",
                        "--format='%(refname:lstrip=3),%(committerdate:unix)'",
                        "--no-merged=origin/master"),
                      root = root, check = TRUE)$output
  if (isTRUE(include_master)) {
    master <- git_run(c("for-each-ref", "refs/remotes/origin/master",
                        "--format='%(refname:lstrip=3),%(committerdate:unix)'"),
                      root = root, check = TRUE)$output
    branches <- c(master, branches)
  }
  branches <- utils::read.table(text = branches, stringsAsFactors = FALSE,
                                sep = ",", col.names = c("name", "last_commit"))
  branches <- branches[branches$name != "gh-pages", ]
  branches$last_commit_age <- calculate_age(branches$last_commit)
  branches$last_commit <- convert_unix_to_iso_time(branches$last_commit)
  branches
}

## This gets last 25 commits from master
## if not master then gets the unmerged commits (limit 25)
git_commits <- function(branch, root = NULL) {
  if (branch == "master") {
    args <- c("log", "--pretty='%h,%cd'", "--date=unix", "--max-count=25",
              sprintf("refs/remotes/origin/%s", branch))
  } else {
    remote_branch <- sprintf("refs/remotes/origin/%s", branch)
    args <- c("log", "--pretty='%h,%cd'", "--date=unix", "--max-count=25",
              paste0("--cherry refs/remotes/origin/master...", remote_branch),
              remote_branch)
  }
  commits <- git_run(args, root = root, check = TRUE)$output
  commits <- utils::read.table(text = commits, stringsAsFactors = FALSE,
                               sep = ",", col.names = c("id", "date_time"))
  commits$age <- calculate_age(commits$date_time)
  commits$date_time <- convert_unix_to_iso_time(commits$date_time)
  ## ID can be parsed as an integer by read.table if by chance the id contains
  ## only numbers
  commits$id <- as.character(commits$id)
  commits
}

get_reports <- function(branch, commit, root) {
  if (branch == "master") {
    ## Get all reports in commit if on master branch
    args <- c("ls-tree", "--name-only", "-d", sprintf("%s:src/", commit))
  } else {
    ## Get only files which have changed from master copy
    ## Note this could inclue files as well as directories.
    ## How can we exclude them?
    args <- c("diff-tree", "--name-only",
              sprintf("refs/remotes/origin/master:src/..%s:src/", commit))
  }
  git_run(args, root = root, check = TRUE)$output
}
