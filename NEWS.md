# orderly 1.0.1

* In orderly to comply with CRAN policies, the functionality of `orderly::orderly_test_start` and related functions has been severely reduced.  Functions `orderly::orderly_test_end` and `orderly::orderly_test_restart` have been removed and `orderly::orderly_test_start` no longer directly provides a useable environment for testing reports (VIMC-3178).

# orderly 1.0.0

* Initial release to CRAN

# orderly 0.8.1

* Parameters now accept defaults in `orderly.yml`, and the format has altered slightly (VIMC-2413, #8).

# orderly 0.8.0

* Deprecated functions `pull_archive`, `pull_dependencies`, `set_default_remote` have been removed (VIMC-2946, deprecated in VIMC-2944 / 0.7.2).

# orderly 0.7.16

* `orderly_migrate` can now clean up old migration backup files.  These should generally be quite safe to delete, but it would be better to do this directly from `orderly` (VIMC-3157)

# orderly 0.7.15

* Global resources can now be renamed on copy, allowing use of subdirectories to structure them.  Using global resources as a set of strings is now deprecated (VIMC-2961).

# orderly 0.7.14

* The command line interface has been overhauled, in particular the parameter passing interface (VIMC-2469) which now accepts key-value pairs and not json

# orderly 0.7.13

* It is now possible to deduplicate an `orderly` archive using `orderly::orderly_deduplicate` (VIMC-731).

# orderly 0.7.12

* `orderly_run` now checks for sink imbalance, in the same way that device imbalances are currently checked for, preventing odd errors when sinks are left open or too many are closed (VIMC-3129)

# orderly 0.7.9

* All README files are copied into destination directory, not just top level (VIMC-3065)

# orderly 0.7.7

* Pulling from remotes now imports the report into the local `orderly` db, and also pulls all dependent reports (VIMC-2855, VIMC-3054).

# orderly 0.7.6

* Detection of modified dependencies has been improved (VIMC-2997).

# orderly 0.7.5

* Fixes notifications to slack on report completion (VIMC-2992).

# orderly 0.7.4

* The database configuration in `orderly_config.yml` now has an "args" section, rather than guessing arguments.  Old configurations are valid, with a warning to update (VIMC-1986).

# orderly 0.7.3

* Function `orderly_unzip_archive` / `unzip_archive` has been removed in preparation for release.
* Function `push_archive` (previously marked as experimental and not implemented for anything other than file remotes) has been removed.  We may reconsider this again in the future.

# orderly 0.7.2

* Functions `pull_archive` and `pull_dependencies` have been deprecated in favour of `orderly_pull_archive` and `orderly_pull_dependencies` (respectively).  Similarly, `set_default_remote` becomes `orderly_default_remote_set` and `orderly_default_remote_get` is exported.  `unzip_archive` becomes `orderly_unzip_archive` (VIMC-2944).

# orderly 0.7.1

* Remove arguments `open` and `extended_output` from `orderly_run` as these were not widely used (VIMC-2945)

# orderly 0.7.0

* The concept of "publishing" a report has been removed from `orderly` in preparation for it to be implemented entirely in [`OrderlyWeb`](https://github.com/vimc/orderly-web) (in the meantime it will be provided by [`orderly.server`](https://github.com/vimc/orderly.server) (VIMC-2941)

# orderly 0.6.6

* The database schema now represents the "custom" fields in a way that makes it more obvious which fields are in fact custom, using new tables `custom_fields` (holding metadata about the fields) and `report_version_custom_fields` (linking these to the report versions) (VIMC-2768)

# orderly 0.6.5

* The `orderly` "runner" (`orderly_runner`) will now periodically backup the destination database, which will be useful in cases where other applications store information in it (VIMC-2882)

# orderly 0.6.4

* The `config` argument to exported functions has been renamed to `root` to better reflect what is expected to be passed in (VIMC-2919)

# orderly 0.6.3

* The `orderly_run.yml` script has now been removed and `orderly_run.rds` is the sole source of truth for the `orderly` run metadata (VIMC-2873).

# orderly 0.6.0

* Allow multiple databases (or no database) to be be used in the `orderly` configuration (VIMC-2795)

# orderly 0.5.18

* Remove old db schema (VIMC-2769)

# orderly 0.5.17

* `README.md` files are automatically copied as resources from source folder. Additionally it is an error to include `README.md` as an artefact (VIMC-2230)
* Fix passing of `timeout` through to remote runners when using the `montagu` package (VIMC-2517)

# orderly 0.5.16

* The database schema now stores basic git information alongside report versions, if the report source archive uses git (VIMC-2615)
* The runner now recognises the `master_only` configuration option and can prevent running reports with references other than `master`.  This is now enforced entirely on the server (VIMC-2642)
* The `changelog` table includes the attributable *public* report version attributable to each changelog entry

# orderly 0.5.15

* `orderly` now prompts to install missing packages and offers code to help with this (VIMC-2384)
* `orderly run` on the command line accepts message entries (VIMC-2363, VIMC-1797)
* `orderly` checks that the resources were not modified during running a report (VIMC-2137)

# orderly 0.5.14

* `README.md` is automatically treated as resource if present (VIMC-2230)
* Fix handling of remote environment variables (VIMC-2553)

# orderly 0.5.13

* Removes support for shiny apps as this was poorly tested and not used by us (VIMC-2544, VIMC-2538).

# orderly 0.5.12

* The `montagu` package is no longer used directly by `orderly` (VIMC-2453) - instead we use a similar "driver" approach to the database connections.  This paves the way for an eventual CRAN release (VIMC-2421).

# orderly 0.5.11

* Improvements to archive migration when local archive directories contain invalid files (VIMC-2449). `orderly` now indicates the version that failed to run, and can move failed versions aside to complete migration of the rest of an archive.

# orderly 0.5.10

* Requires `vaultr` 0.2.0, which is a major overhaul of that package.

# orderly 0.5.8

* Better error messages when unexpected files are found in the `orderly` archive (VIMC-1761)
* Include parameters used in the `orderly` database (VIMC-2397)
* Better error message when migration is needed (VIMC-2392)
* `orderly` works with report repository clones that lack an `archive` directory (VIMC-2383)

# orderly 0.5.7

* Support for changelogs (VIMC-1503)

# orderly 0.5.6

* The `displayname` and `description` fields are correctly copied into the new `orderly` database.

# orderly 0.5.5

* New `connection` column in the `report_version` table in `orderly`'s database (VIMC-2327)

# orderly 0.5.4

* New `latest` column in the `report` table in `orderly`'s database (VIMC-2298)

# orderly 0.5.3

* Null entries are allowed in `orderly_envir.yml`

# orderly 0.5.2

* Introduces "global resources" support for assets used in multiple reports that are not themselves dependencies.
* The database schema includes a `published` field (VIMC-2251)
* New option `if_schema_changed` to rebuild `orderly` database (`orderly_rebuild`) only on a schema change (VIMC-2280)

# orderly 0.5.0

* Send a slack message after a report has been run from the commandline tool, and by extension the runner (VIMC-1931)
* New internal database structure (VIMC-1930)

# orderly 0.4.8

* Fix running reports on montagu servers (VIMC-1911)

# orderly 0.4.7

* Reset working directory if `orderly_test_start` fails on startup (VIMC-1870)
* Extend the templates to allow for multiple files and versions of templates (VIMC-1877)

# orderly 0.4.6

* Also output times with `orderly::orderly_run_info` (#24/VIMC-1818)

# orderly 0.4.5

* Copy a template report from the `orderly/template.yml` within the `orderly` root if available, then falling back on the version in the package.
* `push_archive`, the inverse of `pull_archive` for copying an archive report elsewhere (VIMC-1811).  Currently only supported for `remote` as a path.
* `orderly::orderly_run_info()` can return information about the current run, including information on dependencies.

# orderly 0.4.4

* Print total elapsed time taken to print report
* Runner can kill reports
* Git is reset to previous HEAD immediately after preparing workdir
* `orderly_run` gains a `--fetch` argument, used when `--ref` is specified to run `git fetch` before resolving `ref`
* The `orderly` runner looks up the `sha` for a `ref` at the point of queuing, rather than the point of running
* The cli tool gets a `--pull` and `--fetch` argument

# orderly 0.4.1

* Support for adding a message to a report

# orderly 0.4.0

* Support for downloading reports from locations other than montagu.  This includes a breaking change, where the `server` argument to `pull_dependencies` and `pull_report` changes to `remote`.  The structure here will probably change a little further in a future version.

# orderly 0.3.4

* Support for creating "shiny apps" as artefacts
