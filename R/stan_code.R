
stan_code <- sapply(c("vtf", "deming"), function(model_name) {
  # create C++ code for stan model
  # the two instances are for when the package is in development vs. installed
  stan_dir <- if (dir.exists("stan")) "stan" else file.path("inst", "stan")
  stan_file <- file.path(stan_dir, paste0(model_name, ".stan"))
  readLines(stan_file)
})


stanmodels <- function(model_name){
  suppressMessages(
    model <- cmdstanr::cmdstan_model(
      file.path(system.file("stan", package = "nmmr"), "deming.stan"),
      dir = system.file("exec", package = "nmmr")) )
  # x$exe_file(system.file("exec", model_name, package = "nmmr"))
  model
}

# stanmodels <- sapply(c("deming"), function(model_name) {
#   # create C++ code for stan model
#   # the two instances are for when the package is in development vs. installed
#   # stan_dir <- if (dir.exists("stan")) "stan" else file.path("inst", "stan")
#   # stan_dir <- file.path("inst", "stan")
#   # stan_dir <- file.path(inst_dir(), "stan")
#   # dest <- file.path(stan_dir, "exec")
#   # if (!dir.exists(dest)) dir.create(dest)
#   # stan_file <- file.path(stan_dir, paste0(model_name, ".stan"))
#
#   # cmdstanr::cmdstan_model(stan_file, dir = dest)
#
#   function() {
#     suppressMessages(
#       x <- cmdstanr::cmdstan_model(
#         .make_name(),
#         dir = system.file("exec", package = "nmmr")) )
#     # x$exe_file(system.file("exec", model_name, package = "nmmr"))
#     x
#   }
# })
