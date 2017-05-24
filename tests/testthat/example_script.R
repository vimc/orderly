## This is just an example script; it makes use of 'dat' which is in
## the data.
blues <- c("#F7FBFF", "#DEEBF7", "#C6DBEF", "#9ECAE1", "#6BAED6",
           "#4292C6", "#2171B5", "#08519C", "#08306B")
things <- unique(dat$thing)
cols <- setNames(colorRampPalette(blues)(length(things)), things)
png("mygraph.png", width = 800, height = 600)
plot(value ~ number, dat, col = cols[thing], pch = 19, las = 1)
dev.off()
