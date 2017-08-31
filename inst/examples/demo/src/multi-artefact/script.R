png("all.png")
par(mar = c(15, 4, .5, .5))
barplot(setNames(dat$number, dat$name), las = 2)
dev.off()

write.csv(dat, "all.csv", row.names = FALSE)

sub <- head(dat, 3)

png("subset.png")
par(mar = c(15, 4, .5, .5))
barplot(setNames(sub$number, sub$name), las = 2)
dev.off()

write.csv(sub, "subset.csv", row.names = FALSE)
