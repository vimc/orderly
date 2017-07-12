#!/usr/bin/env Rscript
if (file.exists("demo")) {
  unlink("demo", recursive = TRUE)
}
orderly:::create_orderly_demo("demo")
