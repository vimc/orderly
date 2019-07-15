dat1 <- DBI::dbReadTable(con1, "mtcars")
dat2 <- DBI::dbReadTable(con2, "iris")

png("mygraph.png")
par(mfrow = c(2, 1))
boxplot(mpg ~ cyl, dat1)
boxplot(Sepal.Length ~ Species, dat2)
dev.off()
