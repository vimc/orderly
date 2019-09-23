knitr::knit2html("myreport.Rmd", "myreport.html", quiet = TRUE)
unlink("myreport.md")
