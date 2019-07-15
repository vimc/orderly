resolve_secrets <- function(x, config) {
  if (is.null(config$vault_server)) {
    return(x)
  }
  loadNamespace("vaultr")
  withr::with_envvar(
    orderly_envir_read(config$root),
    vaultr::vault_resolve_secrets(x, addr = config$vault_server))
}
