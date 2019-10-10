d <- read.csv("incoming.csv", stringsAsFactors = FALSE)

png("graph.png")
par(mar = c(15, 4, .5, .5))
barplot(setNames(d$number, d$name), las = 2)
dev.off()

info <- orderly::orderly_run_info()
saveRDS(info, "info.rds")
