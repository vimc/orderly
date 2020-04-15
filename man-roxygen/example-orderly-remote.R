## We need two orderly repositories here - one as a "local" and one as
## a "remote" (see ?orderly_pull_archive)
path_remote <- orderly::orderly_example("demo")
path_local <- orderly::orderly_example("demo")

## Configure our remote:
path_config <- file.path(path_local, "orderly_config.yml")
txt <- readLines(path_config)
writeLines(c(
  txt,
  "remote:",
  "  default:",
  "    driver: orderly::orderly_remote_path",
  "    args:",
  paste("      path:", path_remote)),
  path_config)

## Get our remote:
remote <- orderly::orderly_remote(root = path_local)

## Can use the remote's methods to interact directly - actual methods
## depend on the remote driver being used.
remote$list_reports()
