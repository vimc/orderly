devtools::load_all()

stopifnot(packageVersion("orderly") == "0.4.8")
unlink("reference/0.4.8", recursive = TRUE)
unlink("reference/0.3.2", recursive = TRUE)
unlink("reference/0.3.2.zip")
unlink("reference/0.4.8.zip")

prepare_orderly_example("depends", "reference/0.4.8")
file_copy("reference/0.4.8.yml", "reference/0.4.8/demo.yml")
file_copy("reference/0.4.8.R", "reference/0.4.8/before.R")
file.remove("reference/0.4.8/orderly_version")

run_orderly_demo("reference/0.4.8")
withr::with_dir("reference", zip("0.4.8.zip", "0.4.8"))

file.rename("reference/0.4.8", "reference/0.3.2")
local({
  d <- orderly_list_archive("reference/0.3.2")
  p <- path_orderly_run_rds(file.path("reference/0.3.2/archive", d$name, d$id))
  for (i in p) {
    d <- readRDS(i)
    d$meta <- NULL
    saveRDS(d, i)
  }
})
withr::with_dir("reference", zip("0.3.2.zip", "0.3.2"))

unlink("reference/0.3.2", recursive = TRUE)


## 0.5.1
unlink("reference/0.5.1", recursive = TRUE)
unlink("reference/0.5.1.zip")
prepare_orderly_example("demo", "reference/0.5.1")
run_orderly_demo("reference/0.5.1")
withr::with_dir("reference", zip("0.5.1.zip", "0.5.1"))
unlink("reference/0.5.1", recursive = TRUE)


## 0.5.4
unlink("reference/0.5.4", recursive = TRUE)
unlink("reference/0.5.4.zip")
prepare_orderly_example("demo", "reference/0.5.4")
run_orderly_demo("reference/0.5.4")
withr::with_dir("reference", zip("0.5.4.zip", "0.5.4"))
unlink("reference/0.5.4", recursive = TRUE)

## 0.5.17 (3c794df)
unlink("reference/0.5.17", recursive = TRUE)
unlink("reference/0.5.17.zip")
prepare_orderly_example("demo", "reference/0.5.17")
run_orderly_demo("reference/0.5.17")
withr::with_dir("reference", zip("0.5.17.zip", "0.5.17"))
unlink("reference/0.5.17", recursive = TRUE)

## 0.5.18 (cf327ef)
unlink("reference/0.5.18", recursive = TRUE)
unlink("reference/0.5.18.zip")
prepare_orderly_example("demo", "reference/0.5.18")
run_orderly_demo("reference/0.5.18")
withr::with_dir("reference", zip("0.5.18.zip", "0.5.18"))
unlink("reference/0.5.18", recursive = TRUE)

## 0.6.0 (604dbc4)
unlink("reference/0.6.0", recursive = TRUE)
unlink("reference/0.6.0.zip")
prepare_orderly_example("demo", "reference/0.6.0")
run_orderly_demo("reference/0.6.0")
## Edit the output to make it reflect real-world data with the bug:
i <- dir("reference/0.6.0/archive/use_resource", full.names = TRUE)
p <- file.path(i, "orderly_run.rds")
d <- readRDS(p)
d$meta$hash_readme <- NULL
saveRDS(d, p)
withr::with_dir("reference", zip("0.6.0.zip", "0.6.0"))
unlink("reference/0.6.0", recursive = TRUE)
