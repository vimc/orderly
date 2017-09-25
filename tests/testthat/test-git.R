context("git")

test_that("status", {
  path <- unzip_git_demo()
  expect_equal(git_status(path),
               list(success = TRUE, code = 0,
                    output = character(0), clean = TRUE))
  expect_true(git_is_clean(path))

  writeLines("hello", file.path(path, "hello"))
  expect_equal(git_status(path),
               list(success = TRUE, code = 0,
                    output = "?? hello", clean = FALSE))
  expect_false(git_is_clean(path))
})

test_that("branches", {
  path <- unzip_git_demo()
  sha1 <- git_ref_to_sha("master", path)
  sha2 <- git_ref_to_sha("other", path)

  expect_match(sha1, "^[[:xdigit:]]{40}$")
  expect_match(sha2, "^[[:xdigit:]]{40}$")
  expect_true(sha1 != sha2)

  expect_equal(git_ref_to_sha("HEAD", path), sha1)
  expect_equal(git_branch_name(path), "master")
})

test_that("detch head & restore", {
  path <- unzip_git_demo()
  prev <- git_detach_head_at_ref("other", path)
  expect_equal(prev, "master")
  expect_equal(git_branch_name(path), "HEAD")
  expect_equal(git_ref_to_sha("HEAD", path),
               git_ref_to_sha("other", path))
  expect_equal(git_checkout_branch(prev, TRUE, path), "HEAD")
  expect_equal(git_branch_name(path), "master")
  expect_equal(git_ref_to_sha("HEAD", path),
               git_ref_to_sha("master", path))
})

test_that("fetch / detach / pull", {
  path1 <- unzip_git_demo()
  path2 <- tempfile()
  git_run(c("clone", "--", path1, path2), check = TRUE)
  writeLines("new", file.path(path1, "new"))
  git_run(c("add", "."), path1)
  git_run(c("commit", "-m", "orderly"), path1)

  git_fetch(path2)

  expect_equal(git_ref_to_sha("HEAD", path1),
               git_ref_to_sha("origin/master", path2))

  prev <- git_detach_head_at_ref("origin/master", path2)
  expect_equal(prev, "master")
  expect_equal(git_ref_to_sha("HEAD", path1),
               git_ref_to_sha("HEAD", path2))
  expect_equal(git_branch_name(path2), "HEAD")
  expect_true(file.exists(file.path(path2, "new")))

  git_checkout_branch(prev, TRUE, path2)
  expect_false(file.exists(file.path(path2, "new")))

  git_pull(path2)
  expect_true(file.exists(file.path(path2, "new")))
})

test_that("detect missing ref", {
  path1 <- unzip_git_demo()
  path2 <- tempfile()
  git_run(c("clone", "--", path1, path2), check = TRUE)
  writeLines("new", file.path(path1, "new"))
  git_run(c("add", "."), path1)
  git_run(c("commit", "-m", "orderly"), path1)

  sha1 <- git_ref_to_sha("HEAD", path1)
  sha2 <- git_ref_to_sha("HEAD", path2)

  expect_true(git_ref_exists(sha1, path1))
  expect_true(git_ref_exists(sha2, path2))

  expect_true(git_ref_exists(sha2, path1))
  expect_false(git_ref_exists(sha1, path2))

  expect_true(git_ref_exists("master", path1))
  expect_true(git_ref_exists("master", path2))

  expect_false(git_ref_exists("origin/master", path1))
  expect_true(git_ref_exists("origin/master", path2))

  expect_false(git_ref_exists("origin/other", path1))
  expect_true(git_ref_exists("origin/other", path2))

  expect_true(git_ref_exists("other", path1))
  expect_false(git_ref_exists("other", path2))

  expect_false(git_ref_exists("foo", path1))
  expect_false(git_ref_exists("foo", path2))
})

test_that("run in detached head", {
  path <- unzip_git_demo()
  orderly_run("other", list(nmin = 0), config = path, ref = "other",
              echo = FALSE)

  expect_equal(orderly_list(path), "minimal")
  d <- orderly_list_drafts(path)
  expect_equal(nrow(d), 1L)
  expect_equal(d$name, "other")

  rds <- readRDS(path_orderly_run_rds(file.path(path, "draft", d$name, d$id)))
  expect_equal(rds$git$sha, git_ref_to_sha("other", path))
  expect_null(rds$git$branch)
  expect_null(rds$git$status)
})

test_that("run missing ref", {
  path1 <- unzip_git_demo()
  path2 <- tempfile()
  git_run(c("clone", "--", path1, path2), check = TRUE)
  writeLines("new", file.path(path1, "new"))
  git_run(c("add", "."), path1)
  git_run(c("commit", "-m", "orderly"), path1)

  sha1 <- git_ref_to_sha("HEAD", path1)
  sha2 <- git_ref_to_sha("HEAD", path2)

  runner <- orderly_runner(path2)

  expect_false(git_ref_exists(sha1, path2))

  expect_error(runner$queue("minimal", ref = sha1),
               "Did not find git reference")
  expect_equal(runner$data$length(), 0)

  id <- runner$queue("minimal", ref = sha1, update = TRUE)
  expect_is(id, "character")
  expect_equal(runner$data$length(), 1)

  expect_true(git_ref_exists(sha1, path2))
  expect_equal(git_ref_to_sha("HEAD", path2), sha2)

  id <- runner$queue("minimal", ref = NULL)
  expect_equal(git_ref_to_sha("HEAD", path2), sha2)

  id <- runner$queue("minimal", ref = NULL, update = TRUE)
  expect_equal(git_ref_to_sha("HEAD", path2), sha1)
})
