dat1 <- readRDS("previous1.rds")
dat2 <- readRDS("previous2.rds")

saveRDS(list(dat1, dat2), "results.rds")
saveRDS(orderly1::orderly_run_info(), "output.rds")
