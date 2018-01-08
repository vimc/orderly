dat$am <- factor(dat$am, labels = c("Automatic", "Manual"))
save(list = "dat", file = "shiny/data.RData")
