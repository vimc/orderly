knitr::knit2html("report.Rmd", "report.html", quiet = TRUE)
unlink("report.md")
