resolve_secrets <- function(x) {
  re <- "^VAULT:(.+):(.+)"
  if (is.list(x)) {
    i <- vlapply(x, function(el) is.character(el) && grepl(re, el))
    if (any(i)) {
      x[i] <- resolve_secrets(vcapply(x[i], identity))
    }
  } else {
    i <- grepl(re, x)
    if (any(i)) {
      loadNamespace("vaultr")
      vault <- vaultr::vault_client()
      key <- unname(sub(re, "\\1", x[i]))
      field <- unname(sub(re, "\\2", x[i]))
      x[i] <- unname(Map(vault$read, key, field))
    }
  }
  x
}
