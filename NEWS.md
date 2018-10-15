# 0.5.2

* The database schema includes a `published` field (VIMC-2251)

# 0.5.0

* Send a slack message after a report has been run from the commandline tool, and by extension the runner (VIMC-1931)
* New internal database structure (VIMC-1930)

# 0.4.8

* Fix running reports on montagu servers (VIMC-1911)

# 0.4.7

* Reset working directory if `orderly_test_start` fails on startup (VIMC-1870)
* Extend the templates to allow for multiple files and versions of templates (VIMC-1877)

# 0.4.6

* Also output times with `orderly::orderly_run_info` (#24/VIMC-1818)

# 0.4.5

* Copy a template report from the `orderly/template.yml` wiithin the orderly root if available, then falling back on the version in the package.
* `push_archive`, the inverse of `pull_archive` for copying an archive report elsewhere (VIMC-1811).  Currently only supported for `remote` as a path.
* `orderly::orderly_run_info()` can return information about the current run, including information on dependencies.

# 0.4.4

* Print total elapsed time taken to print report
* Runner can kill reports
* Git is reset to previous HEAD immediately after preparing workdir
* `orderly_run` gains a `--fetch` argument, used when `--ref` is specified to run `git fetch` before resolving `ref`
* The orderly runner looks up a the `sha` for a `ref` at the point of queuing, rather than the point of running
* The cli tool gets a `--pull` and `--fetch` argument

# 0.4.1

* Support for adding a message to a report

# 0.4.0

* Support for downloading reports from locations other than montagu.  This includes a breaking change, where ther `server` argument to `pull_dependencies` and `pull_report` changes to `from` to `remote`.  The structure here will probably change a little further in a future version.

# 0.3.4

* Support for creating "shiny apps" as artefacts
