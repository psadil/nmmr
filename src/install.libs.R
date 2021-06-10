
message(glue::glue("R_PACKAGE_SOURCE: {R_PACKAGE_SOURCE}"))
message(glue::glue("R_PACKAGE_DIR: {R_PACKAGE_DIR}"))


sapply(c("deming"), function(model_name) {
  # create C++ code for stan model
  # the two instances are for when the package is in development vs. installed
  stan_dir <- if (dir.exists(file.path(R_PACKAGE_SOURCE, "stan"))) "stan" else file.path(R_PACKAGE_SOURCE, "inst", "stan")
  stan_file <- file.path(stan_dir, paste0(model_name, ".stan"))

  dest <- file.path(R_PACKAGE_SOURCE, 'exec')
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  # file.copy(execs, dest, overwrite = TRUE)

  cmdstanr::cmdstan_model(
    stan_file,
    dir = dest,
    include_paths = stan_dir
  )
})


