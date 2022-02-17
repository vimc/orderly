if (!is.numeric(b)) {
  stop("b must be numeric")
}

saveRDS(list(a = a, b = b, c = c),
        "results.rds")
