# 0.4.5

* Copy a template report from the `orderly/template.yml` wiithin the orderly root if available, then falling back on the version in the package.
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
