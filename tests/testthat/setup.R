withr::local_envvar(
  c(RETICULATE_USE_MANAGED_VENV = "yes"),
  .local_envir = teardown_env()
)
