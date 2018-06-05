dat <- readRDS('previous.rds')
png('mygraph.png')
par(mar = c(15, 4, .5, .5))
barplot(setNames(dat$number, dat$name), las = 2)
dev.off()

saveRDS(orderly::orderly_run_info(), "output.rds")
