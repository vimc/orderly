# orderly 1.2.24

* Make `orderly_cleanup` much chattier (#265)

# orderly 1.2.20

* `orderly_pull_archive` is now more tolerant of trailing slashes (#260)

# orderly 1.2.19

* New functions `orderly_bundle_pack_remote` and `orderly_bundle_import_remote` which create a bundle from a remote and return the completed bundle back to the remote (VIMC-4457)

# orderly 1.2.18

* Allow `orderly_bundle_import` to accept a filename that has been renamed from `<id>.zip`. While this is not generally desireable, it may be needed for some workflows (VIMC-4382)

# orderly 1.2.17

* Fixes bug where `orderly_bundle_pack` failed when the orderly tree and temporary directory were on different filesystems (VIMC-4354)

# orderly 1.2.16

* Fixes bug where `orderly_bundle_run` failed when using a relative path for a working directory (VIMC-4337)

# orderly 1.2.15

* Fixes bug where default parameter values were not used in resolving query dependencies (VIMC-4135)

# orderly 1.2.14

* `orderly::orderly_new()` works in an orderly repo that does not (yet) have a `src/` directory (VIMC-4032)

# orderly 1.2.13

* Remove `orderly::orderly_runner` which has been moved to `vimc/orderly.server` (VIMC-4073)

# orderly 1.2.12

* Add new function `orderly::orderly_config` to return an object representing the config (VIMC-4048)

# orderly 1.2.11

* Added a new "bundle" concept, for collecting together all requirements for a report from orderly that can be run elsewhere (e.g., on a HPC) using new functions `orderly::orderly_bundle_pack`, `orderly::orderly_bundle_run` and `orderly::orderly_bundle_import`. This interface will be expanded and the interface may change somewhat (VIMC-3825)

# orderly 1.2.10

* Add function `get_report_parameters` to list parameters for a report at a particular commit id (VIMC-3953)

# orderly 1.2.9

* Add function `get_reports` to list reports available for a particular branch and commit from orderly_runner (VIMC-3945)

# orderly 1.2.8

* Add function `git_commits` to list commits for a particular branch from orderly_runner (VIMC-3941)

# orderly 1.2.7

* Add function `git_branches_no_merged` to get unmerged branches from orderly_runner (VIMC-3857)

# orderly 1.2.5

* Add ability to run workflow from CLI (VIMC-3873)

# orderly 1.2.4

* Error messages and stack traces are now preserved in `orderly.log` after a faied run; this primarily impacts the cli runner and then primarily when being run in parallel where the log is not printed to screen (VIMC-3841)

# orderly 1.2.3

* New function `orderly::orderly_workflow` allows users to run a "workflow" - a list of reports which to be run in order. Workflows are configured via a yml file in `workflows/` directory.

# orderly 1.2.0

* `orderly::orderly_data()` has been removed as do not believe anyone was using it, and it is superceeded by `orderly::orderly_develop_start()` (VIMC-3611)
* Some internally used arguments have been removed from `orderly::orderly_run` (`id_file`, `batch_id`, `ref` and `fetch`). These were not intended for direct use by users (VIMC-3539)

# orderly 1.1.35

* `orderly::orderly_pull_dependencies()` works where the dependencies use query ids, and `orderly::orderly_pull_archive()` accepts query ids (rather than just `latest`) as an argument (VIMC-3789)

# orderly 1.1.34

* `orderly::orderly_run_remote()` takes an instance argument to allow users to specify the source DB (VIMC-3698)

# orderly 1.1.32

* New function `orderly::orderly_push_archive()` which allows pushing of an archive to a remote.  The `orderly::orderly_remote_path` remote supports this, though care should be taken with what you push! (VIMC-3784)

# orderly 1.1.31

* `orderly::orderly_rebuild()` creates a dated backup of the database before running, allowing this potentially destructive operation to be recovered from (VIMC-3702)

# orderly 1.1.28

* `orderly::orderly_develop_clean()` no longer deletes artefacts that are re-exported from sources (VIMC-3671, reported by @cewalters)

# orderly 1.1.27

* Allow `orderly::orderly_develop_start()` to use environment variables declared in `orderly.yml` and defined in `orderly_envir.yml` (#214, VIMC-3669, reported by @sangeetabhatia03)

# orderly 1.1.26

* Add ability to post report commit notification to Microsoft Teams (VIMC-3640)

# orderly 1.1.24

* Batches of parameters can be supplied to `orderly batch` using a csv file via `--file` (VIMC-3569)

# orderly 1.1.23

* New function `orderly::orderly_remote()` for geting an "orderly remote" object, as declared in the `orderly_config.yml` (VIMC-3655)

# orderly 1.1.20

* Add support for running batches of orderly reports that differ in their parameters, available via `orderly::orderly_batch` and with the CLI as `orderly batch` (VIMC-3603)

# orderly 1.1.15

* Fix bug where report downloading did not work for some pathalogical windows paths (VIMC-3595)

# orderly 1.1.14

* Fix bug in instance selection, probably introduced in 1.0.6 (VIMC-3589)

# orderly 1.1.13

* Enable implicit report name for `orderly::orderly_run` and `orderly::orderly_pull_dependenices` (VIMC-3512, #170)

# orderly 1.1.12

* Introduces a basic query interface for searching for reports that match criteria based on parameters and tags, which can be used directly `orderly::orderly_search` or when declaring dependencies (VIMC-3538)

# orderly 1.1.11

* Environment variables can be used in orderly reports by using the `environment:` field in `orderly.yml` (VIMC-3558)

# orderly 1.1.10

* Secrets can be read from the vault and used in orderly reports (VIMC-3536)

# orderly 1.1.9

* Better parsing of parameters passed on the command line, allowing more parameters to be passed through, and coping better with shell quoting (VIMC-3550)

# orderly 1.1.6

* Environment variables in `orderly_envir.yml` are available during report run (#180, VIMC-3530)

# orderly 1.1.5

* Introduce the concept of "tags"; these are immutable and exist at the level of a report version.  Currently there is nothing that can be done with tags, but these will become useful in conjunction with [OrderlyWeb](https://github.com/vimc/orderly-web) (VIMC-3514)

# orderly 1.1.4

* Automatic creation of `.gitignore` files with `orderly::orderly_use_gitignore` (VIMC-3513, reported by @jeffeaton)

# orderly 1.1.3

* The `orderly::orderly_develop_*` functions are now permissive and allow some invalid `orderly.yml` to continue - in particular, the partially complete template generated by `orderly::orderly_new` can be directly used (VIMC-3510)

# orderly 1.1.2

* Improved error message with misformatted artefacts (VIMC-3511, reported by @jeffeaton)
* The first of `usethis` inspired functions `orderly::orderly_use_resource()`, `orderly::orderly_use_source()` and `orderly::orderly_use_package()`which can add a resource, source or packages into the orderly.yml (VIMC-3503)

# orderly 1.1.1

* Dependencies can be resolved as if they were to be run on a remote (including appropriate selection of "latest" dependencies).  This is to support future decentralised workflows (VIMC-3473)

# orderly 1.1.0

* New report development mode, via `orderly_develop_start`, `orderly_develop_status` and `orderly_develop_clean`.  These largely supersede `orderly_test_start`, though the latter remains for now (VIMC-3404)

# orderly 1.0.15

* Depending on draft reports, including with `use_draft = "newer"`, no longer pulls in failed drafts (VIMC-3467, reported by @sangeetabhatia03)

# orderly 1.0.14

* Functionality for creating dependency graphs for reports using orderly::orderly_graph (VIMC-2174)

# orderly 1.0.13

* The orderly CLI runner gets a `pull` sub command for pulling dependencies from remote orderly servers (VIMC-3466)

# orderly 1.0.12

* The orderly CLI runner gets an `--instance` argument (VIMC-3460)
* The instance information is saved into the `orderly_run.rds`.  This is not yet reflected in the database and might be tweaked before being added.

# orderly 1.0.11

* More informative error messages when orderly fails to resolve an environment variable, particularly when loading remote configuration (VIMC-3386)

# orderly 1.0.10

* Fix regression running reports - vault autocompletes to vault_server

# orderly 1.0.9

* Enforce parameter types before model run, rather than on commit, and with better messages (VIMC-3411)

# orderly 1.0.8

* Expand how the vault server is defined to allow additional arguments to be directly specified (VIMC-3372)

# orderly 1.0.7

* More flexible control of use of draft reports, using the argument `use_draft` to `orderly_run` and `orderly_test_start`.  This will replace the use of explicitly specifying `draft: true` in the depends section of `orderly.yml` (VIMC-3377).

# orderly 1.0.6

* Database configurations now support the concept of "instances" to allow switching between different versions of a database (e.g., production and staging) without manually altering the configuration or environment variables. Functions `orderly::orderly_run`, `orderly::orderly_db`, `orderly::orderly_test_start` and `orderly::orderly_data` all get an `instance` argument to support this (VIMC-3302).

# orderly 1.0.5

* The metadata now includes the state of `.Random.seed`, if present (VIMC-3375)

# orderly 1.0.3

* `orderly::orderly_test_start` prints instructions that are paste-able on windows -- previously they may have contained backslashes (VIMC-3251).
* `orderly::orderly_run` now strips a leading `src/` if provided, allowing easier tab-completion of report names (VIMC-3226).

# orderly 1.0.2 (CRAN)

* `orderly::orderly_test_check` is no longer case sensitive with paths, preventing issues when used from directories that do not have canonical casing (VIMC-3205)

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
