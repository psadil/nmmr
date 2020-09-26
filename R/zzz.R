.onLoad <- function(libname, pkgname){

  if(!stanmodels$vtf$cmdstan_version == cmdstanr::cmdstan_version())
    warning(
      "The currently install CmdStan version does not match
      the version this package was build against. This can
      cause subtle bugs! Please reinstall nmmr.")

  if(!identical(stanmodels$vtf$cmdstanr_version, packageVersion("cmdstanr")))
    warning(
      "The currently install CmdStanR version does not match
      the version this package was build against. This can
      cause subtle bugs! Please reinstall nmmr.")

}
