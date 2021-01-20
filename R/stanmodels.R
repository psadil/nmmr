# names of stan models
stanmodels <- c("vtf", "orthogonal")

# instantiate each stanmodel object
stanmodels <- sapply(stanmodels, function(model_name) {
  # create C++ code for stan model
  stan_file <- file.path("inst", "stan", paste0(model_name, ".stan"))

  cmdstanr::cmdstan_model(stan_file, include_paths = file.path("inst", "stan"))
})
