# names of stan models
stanmodels <- c("vtf")

# instantiate each stanmodel object
stanmodels <- sapply(stanmodels, function(model_name) {
  # create C++ code for stan model
  stan_file <- if(dir.exists("stan")) "stan" else file.path("inst", "stan")
  stan_file <- file.path(stan_file, paste0(model_name, ".stan"))

  cmdstanr::cmdstan_model(stan_file, include_paths = file.path("inst", "stan"))
})
