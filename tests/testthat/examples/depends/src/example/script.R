dat$number <- dat$number + rnorm(nrow(dat))
saveRDS(dat, "data.rds")
