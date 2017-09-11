while (!file.exists("resume")) {
  Sys.sleep(0.05)
  writeLines("started", "started")
}

res <- readLines("resume")

if (res == "error") {
  stop("there was an error running the report")
}

png("mygraph.png")
par(mar = c(15, 4, .5, .5))
barplot(setNames(dat$number, dat$name), las = 2)
dev.off()
