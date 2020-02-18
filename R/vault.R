resolve_secrets <- function(x, config) {
  if (is.null(config[['vault']])) {
    return(x)
  }
  loadNamespace("vaultr")
  name <- "orderly_config.yml:vault"
  withr::with_envvar(orderly_envir_read(config$root), {
    vault_args <- resolve_env(config[["vault"]], name = name)
    vaultr::vault_resolve_secrets(x, vault_args = vault_args)
  })
}
