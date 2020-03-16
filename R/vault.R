resolve_secrets <- function(x, config) {
  if (is.null(config[["vault"]])) {
    return(x)
  }
  loadNamespace("vaultr")
  withr::with_envvar(orderly_envir_read(config$root), {
    vault_args <- resolve_env(config[["vault"]], "orderly_config.yml:vault")
    vaultr::vault_resolve_secrets(x, vault_args = vault_args)
  })
}
