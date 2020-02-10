resolve_secrets <- function(x, config) {
  if (is.null(config$vault)) {
    return(x)
  }
  loadNamespace("vaultr")
  withr::with_envvar(
    orderly_envir_read(config$root),
    vaultr::vault_resolve_secrets(x, vault_args = resolve_env(config$vault)))
}
