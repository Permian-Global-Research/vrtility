withr::local_envvar(
  c(RETICULATE_USE_MANAGED_VENV = "yes"),
  .local_envir = teardown_env()
)

withr::local_options(
  list(
    vrt.pause.base = 1,
    vrt.pause.cap = 1,
    vrt.max.times = 1,
    vrtility.use_muparser = FALSE
  ),
  .local_envir = teardown_env()
)
