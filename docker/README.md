# orderly docker image

Create a demo orderly store (change `demo` to put it elsewhere - `"."` should be fine if the current directory is empty)

```
docker run --rm --entrypoint create_orderly_demo.sh \
    -u ${UID} \
    -v ${PWD}:/orderly \
    -w /orderly \
    vimc/orderly:master \
    "demo"
```

Run a report - note `demo` on the `-v` line.  `minimal` is a report name:

```
docker run --rm \
    -u ${UID} \
    -v ${PWD}/demo:/orderly \
    -w /orderly \
    vimc/orderly:master \
    "minimal"
```

`other` is also allowed but you have to pass in parameters:

```
docker run --rm \
    -u ${UID} \
    -v ${PWD}/demo:/orderly \
    -w /orderly \
    vimc/orderly:master \
    --parameters '{"nmin": 0.5}' "other"
```
