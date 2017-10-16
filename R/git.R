## It's not totally clear to me that these sorts of shenanigans will
## work all that reliably, and testing it is going to be an absolute
## horror show.  It might be somewhat eased by working in detached
## head mode because then it's easy enough to move around the tree
## without doing a whole heap of resets.

with_branch <- function(name, expr, remote = "origin", root = NULL) {
  prev <- git_checkout_remote_branch(name, remote, root)
  on.exit(git_checkout_branch(prev, force = force, root = root))
  force(expr)
}

git_run <- function(args, root = NULL, check = FALSE) {
  git <- sys_which("git")
  if (!is.null(root)) {
    args <- c("-C", root, args)
  }
  res <- system3(git, args)
  if (check && !res$success) {
    stop(sprintf("Error code %d running command", res$code))
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

git_ref_to_sha <- function(ref, root = NULL) {
  assert_scalar_character(ref)
  res <- git_run(c("rev-parse", ref), root = root, check = FALSE)
  if (res$success) {
    res$output
  } else {
    NA_character_
  }
}

git_ref_exists <- function(ref, root = NULL) {
  assert_scalar_character(ref)
  git_run(c("merge-base", ref, "HEAD"), root = root, check = FALSE)$success
}

git_status <- function(root = NULL, ignore_untracked = FALSE) {
  args <- c("status", "--porcelain=v1",
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

git_checkout_remote_branch <- function(name, remote = "origin", root = NULL) {
  if (!git_is_clean(root)) {
    stop("working directory must be clean")
  }
  prev <- git_branch_name(root)
  args <- c("checkout", "-B", name, sprintf("%s/%s", remote, name))
  orderly_log("checkout",
              sprintf("%s (%s/%s); was %s", name, remote, name, prev))
  git_run(args, root = root, check = TRUE)
  prev
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
  orderly_log("checkout", sprintf("%s; was %s", name, prev))
  git_run(args, root = root, check = TRUE)
  prev
}

git_fetch <- function(root = NULL) {
  git_run("fetch", root = root, check = TRUE)
}

git_pull <- function(root = NULL) {
  git_run("pull", root = root, check = TRUE)
}

is_sha <- function(x) {
  grepl("^[[:xdigit:]]{40}", x)
}
