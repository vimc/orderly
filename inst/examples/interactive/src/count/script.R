end <- Sys.time() + time
while (Sys.time() < end) {
  cat(sprintf("%s waiting...\n", runif(1)))
  Sys.sleep(poll)
}

png("mygraph.png")
par(mar = c(15, 4, .5, .5))
barplot(setNames(dat$number, dat$name), las = 2)
dev.off()
