resolve_secrets <- function(x, vault_server) {
  re <- "^VAULT:(.+):(.+)"
  if (is.list(x)) {
    i <- vlapply(x, function(el) is.character(el) && grepl(re, el))
    if (any(i)) {
      x[i] <- resolve_secrets(vcapply(x[i], identity), vault_server)
    }
  } else {
    i <- grepl(re, x)
    if (any(i)) {
      loadNamespace("vaultr")
      vault <- vaultr::vault_client(addr = vault_server)
      key <- unname(sub(re, "\\1", x[i]))
      field <- unname(sub(re, "\\2", x[i]))
      x[i] <- unname(Map(vault$read, key, field))
    }
  }
  x
}
