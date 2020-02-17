dat <- data.frame(x = 1:10, y = runif(10))
write.csv(dat, "mydata.csv", row.names = FALSE)

png("mygraph.png")
plot(dat)
dev.off()
