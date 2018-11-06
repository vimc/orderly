sql <- "SELECT name, number FROM thing"
dat <- DBI::dbGetQuery(con, sql)
png("mygraph.png")
par(mar = c(15, 4, .5, .5))
barplot(setNames(dat$number, dat$name), las = 2)
dev.off()
