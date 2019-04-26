# 0.6.0

* Allow multiple databases (or no database) to be be used in the orderly configuration (VIMC-2795)

# 0.5.18

* Remove old db schema (VIMC-2769)

# 0.5.17

* `README.md` files are automatically copied as resources from source folder. Additionally it is an error to include `README.md` as an artefact (VIMC-2230)
* Fix passing of `timeout` through to remote runners when using the `montagu` package (VIMC-2517)

# 0.5.16

* The database schema now stores basic git information alongside report versions, if the report source archive uses git (VIMC-2615)
* The runner now recognises the `master_only` configuration option and can prevent running reports with references other than `master`.  This is now enforced entirely on the server (VIMC-2642)
* The `changelog` table includes the attributable *public* report version attributable to each changelog entry

# 0.5.15

* `orderly` now prompts to install missing packages and offers code to help with this (VIMC-2384)
* `orderly run` on the command line accepts message entries (VIMC-2363, VIMC-1797)
* `orderly` checks that the resources were not modified during running a report (VIMC-2137)

# 0.5.14

* README.md is automatically treated as reaource if present (VIMC-2230)

# 0.5.14

* Fix handling of remote environment variables (VIMC-2553)

# 0.5.13

* Removes support for shiny apps as this was poorly tested and not used by us (VIMC-2544, VIMC-2538).

# 0.5.12

* The `montagu` package is no longer used directly by `orderly` (VIMC-2453) - instead we use a similar "driver" approach to the database connections.  This paves the way for an eventual CRAN release (VIMC-2421).

# 0.5.11

* Improvements to archive migration when local archive directories contain invalid files (VIMC-2449). `orderly` now indicates the version that failed to run, and can move failed versions aside to complete migration of the rest of an archive.

# 0.5.10

* Requires `vaultr` 0.2.0, which is a major overhaul of that package.

# 0.5.8

* Better error messages when unexpected files are found in the orderly archive (VIMC-1761)
* Include parameters used in the orderly database (VIMC-2397)
* Better error message when migration is needed (VIMC-2392)
* Orderly works with report repository clones that lack an `archive` directory (VIMC-2383)

# 0.5.7

* Support for changelogs (VIMC-1503)

# 0.5.6

* The `displayname` and `description` fields are correctly copied into the new orderly database.

# 0.5.5

* New `connection` column in the `report_version` table in orderly's database (VIMC-2327)

# 0.5.4

* New `latest` column in the `report` table in orderly's database (VIMC-2298)

# 0.5.3

* Null entries are allowed in `orderly_envir.yml`

# 0.5.2

* Introduces "global resources" support for assets used in multiple reports that are not themselves dependencies.
* The database schema includes a `published` field (VIMC-2251)
* New option `if_schema_changed` to rebuild orderly database (`orderly_rebuild`) only on a schema change (VIMC-2280)

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
