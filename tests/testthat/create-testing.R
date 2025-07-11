#!/usr/bin/env Rscript
if (file.exists("demo")) {
  unlink("demo", recursive = TRUE)
}
orderly1:::create_orderly_demo("demo")
