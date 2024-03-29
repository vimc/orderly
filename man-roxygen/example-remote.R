# Suppose we have a "remote" orderly repository at some path.
# This might be read-only for you in practice and available via a
# network filesystem or a dropbox folder synced to your computer.
# We'll populate this with a pair of reports:
path_remote <- orderly::orderly_example("demo")
id <- orderly::orderly_run("other", list(nmin = 0),
                           root = path_remote, echo = FALSE)
orderly::orderly_commit(id, root = path_remote)
id <- orderly::orderly_run("use_dependency",
                           root = path_remote, echo = FALSE)
orderly::orderly_commit(id, root = path_remote)

# We'll create a an object to interact with this remote using
# orderly_remote_path.
remote <- orderly::orderly_remote_path(path_remote)

# We can use this object directly
remote$list_reports()
remote$list_versions("other")

# More typically one will interact with the functions
# orderly_pull_archive and orderly_pull_dependencies.

# Now, suppose that you have your "local" copy of this; it shares
# the same source (ordinarily these would both be under version
# control with git):
path_local <- orderly::orderly_example("demo")

# If we wanted to run the report "use_dependency" we need to have
# a copy of the report "other", on which it depends:
try(orderly::orderly_run("use_dependency", root = path_local))

# We can "pull" dependencies of a report before running
orderly::orderly_pull_dependencies("use_dependency", remote = remote,
                                   root = path_local)

# Now we can run the report because we have a local copy of the
# dependency:
orderly::orderly_run("use_dependency", root = path_local)

# We can also directly pull previously run reports:
orderly::orderly_pull_archive("use_dependency", id, remote = remote,
                              root = path_local)
orderly::orderly_list_archive(root = path_local)
