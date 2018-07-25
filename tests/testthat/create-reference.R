stopifnot(packageVersion("orderly") == "0.4.8")
unlink("reference/0.4.8", recursive = TRUE)
unlink("reference/0.3.2", recursive = TRUE)

orderly:::prepare_orderly_example("depends", "reference/0.4.8")
file_copy("reference/0.4.8.yml", "reference/0.4.8/demo.yml")
file_copy("reference/0.4.8.R", "reference/0.4.8/before.R")
run_orderly_demo("reference/0.4.8")
withr::with_dir("reference", zip("0.4.8.zip", "0.4.8"))

file.rename("reference/0.4.8", "reference/0.3.2")
local({
  d <- orderly_list_archive("reference/0.3.2")
  p <- path_orderly_run_rds(file.path("reference/0.3.2/archive", d$name, d$id))
  for (i in paths) {
    d <- readRDS(i)
    d$meta <- NULL
    saveRDS(d, i)
  }
})
withr::with_dir("reference", zip("0.3.2.zip", "0.3.2"))

unlink("reference/0.3.2", recursive = TRUE)
