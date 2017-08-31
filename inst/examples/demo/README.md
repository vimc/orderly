# orderly demo

This is a demo orderly repo.  The yml file `demo.yml` controls how it is built.  The idea here is to generate test cases for use for testing the reporting api.  We'll continue adding reports in here until we have enough test cases to illustrate possible outcomes.

To add new examples, create a new orderly report within `src`.  Then edit `demo.yml` to arrange running the the report.  In simple cases it is sufficient to write

```yaml
- name: my-report-name
```

If the report takes parameters, there is a parameters field, e.g.:

```yaml
- name: other
  parameters:
    nmin: 0
```

A `publish` field, set to `true` will make the report "published", e.g.:

```yaml
- name: minimal
  publish: true
```

All reports listed here will be at least commited (moved to `archive/` and added to the orderly sqlite database)

In order to change the *source* contents of a script, there is a `before` field that can list a function to be run before running the report.  For example

```yaml
- name: other
  publish: true
  before: other_change_script
  parameters:
    nmin: 0
```

will run the function `other_change_script()` (with no arguments) just before running the report `other`.  The functions must be listed in the file `before.R`; the `other_change_script` function currently contains

```r
other_change_script <- function() {
  txt <- readLines("src/other/script.R")
  writeLines(c("extract$number <- extract$number * 1.2", txt),
             "src/other/script.R")
}
```

To build the demo repo, run

```r
path <- orderly:::create_orderly_demo()
```

which will create a new directory with all the output in it.  This is also run on teamcity to create an artefact `demo`, and also run via docker as

```sh
docker run --entrypoint create_orderly_demo.sh \
  -v $projectDir:/orderly \
  -w /orderly docker.montagu.dide.ic.ac.uk:5000/orderly:master \
  demo
```
