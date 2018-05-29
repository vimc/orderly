resolve_secrets <- function(x, config) {
  re <- "^VAULT:(.+):(.+)"
  if (is.list(x)) {
    i <- vlapply(x, function(el) is.character(el) && grepl(re, el))
    if (any(i)) {
      x[i] <- resolve_secrets(vcapply(x[i], identity), config)
    }
  } else {
    i <- grepl(re, x)
    if (any(i)) {
      loadNamespace("vaultr")
      vault <- withr::with_envvar(
        orderly_envir_read(config$path),
        vaultr::vault_client(addr = config$vault_server))
      key <- unname(sub(re, "\\1", x[i]))
      field <- unname(sub(re, "\\2", x[i]))
      x[i] <- unname(Map(vault$read, key, field))
    }
  }
  x
}
