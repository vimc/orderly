# Notes that might be useful when building an API

## Basic query api

* `GET report` - array of report names
* `GET report/:name/` - array of report versions
* `GET report/:name/:version` - dict of report metadata
* `GET report/:name/:version/data/` - dict of data names/hash
* `GET report/:name/:version/data/:data` - download an data
* `GET report/:name/:version/artefact/` - dict of artefacts/hash
* `GET report/:name/:version/artefact/:artefact` - download an artefact
* `GET report/:name/:version/resource/` - dict of resources/hash
* `GET report/:name/:version/resource/:resource` - download an resource
* `GET report/:name/:version/all` - zip file of everything (incl data)
* `GET report/:name/:version/sessioninfo` - rds session info
* `GET data/csv/:id` - get a data set
* `GET data/rds/:id` - get a data set

## Decentralised store

Will we be running everything on one central point, or allowing people to upload reports?  When shipping out to a centralised database, we can actually move only the relevant directories.  But to do that we need to know what data was used so it will look like:

  - A->B: I have reports `x`, `y`, `z` to give you
  - B->A: I already have `y`
  - A->B: Reports `x` and `z` use datasets `h1`, `h2` and `h3`
  - B->A: I already have `h1`
  - A: Send a zip containing the required subset of directories (do we send the source?  Probably not because that changes through time)
  - B: After recieving the zip unpack it and rebuild the index

It would probably be simplest for the upload to be a single instance at a time

It might be that we do something where we make a query where we ask what is needed and have the api return a url that we should post to with the appropriate zip file.  Then we push to that.  Something like:

* `POST report/` {name: name, id: id, data: [h1, h2]}
  -> {submit: bool, data: [h1], url: upload/:id}
* `POST upload/:id` (zip file) -> check and report
