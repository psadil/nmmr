.onLoad <- function(libname, pkgname){

  if(!identical(Model$public_fields$cmdstanr_version, utils::packageVersion("cmdstanr")))
    warning(
      "The currently install CmdStanR version does not match
      the version this package was build against. This can
      cause subtle bugs! Please reinstall nmmr.")

}
