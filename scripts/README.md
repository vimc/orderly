## Building the website

The website is stored in `docs` as a git orphan branch that will be force-pushed to `gh-branches` to render at https://vimc.github.io/orderly/

The docs will be tested by teamcity on every push, but are only pushed to update the website when changes are made to `master`.

The steps are a little ugly:

1. create a new container that includes even more dependencies than orderly itself in order to use `pkgdown`
2. set up a postgres db to accept the new db schema
3. use the new dev container to build the schema in the db container
4. run `pkgdown` to generate the bulk of the website
5. run `schemaspy` (from its own container) to generate the schema docs
