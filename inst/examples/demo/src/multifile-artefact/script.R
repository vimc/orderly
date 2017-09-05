plt <- function() {
  par(mar = c(15, 4, .5, .5))
  barplot(setNames(dat$number, dat$name), las = 2)
}

png("mygraph.png")
plt()
dev.off()

pdf("mygraph.pdf")
plt()
dev.off()
