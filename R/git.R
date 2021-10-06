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

git_show <- function(path, ref = NULL, root = NULL) {
  if (is.null(ref)) {
    ref <- "HEAD"
  }
  path <- sprintf("%s:%s", ref, path)
  git_run(c("show", path), root = root, check = TRUE)
}

git_reports <- function(ref = NULL, root = NULL) {
  if (is.null(ref)) {
    ref <- "HEAD"
  }
  git_run(c("ls-tree", "--name-only", "-d", sprintf("%s:src/", ref)),
          root = root, check = TRUE)
}
