if (b == 2) {
  stop("b cannot be 2")
}

saveRDS(list(a = a, b = b, c = c),
        "results.rds")
