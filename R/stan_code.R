
stan_code <- sapply(c("vtf", "deming"), function(model_name) {
  # create C++ code for stan model
  # the two instances are for when the package is in development vs. installed
  stan_dir <- if (dir.exists("stan")) "stan" else file.path("inst", "stan")
  stan_file <- file.path(stan_dir, paste0(model_name, ".stan"))
  readLines(stan_file)
})
