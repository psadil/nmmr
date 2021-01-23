
# nmmr

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/psadil/nmmr/branch/master/graph/badge.svg)](https://codecov.io/gh/psadil/nmmr?branch=master)
[![R-CMD-check](https://github.com/psadil/nmmr/workflows/R-CMD-check/badge.svg)](https://github.com/psadil/nmmr/actions)
<!-- badges: end -->

## Overview

Experimental package for NeuroModulation Modeling in `R`. This package
under active development and not yet released.

## Installation

### Windows

If you are on Windows, you will first need ensure that
[RTools](https://cran.r-project.org/bin/windows/Rtools/) is installed
and correctly setup.

### Stan

`nmmr` relies on the [CmdStanR](https://mc-stan.org/cmdstanr/) interface
to [Stan](https://mc-stan.org). To install `nmmr`, first [follow
instructions for setting up Stan and
cmdstanR](https://mc-stan.org/cmdstanr/articles/cmdstanr.html). It is
important that `CmdStanR` and `CmdStan` be installed *before* installing
`nmmr`. Note that there are two steps to this process.

1.  Install the `CmdStanR` package

``` r
# If you don't have the remotes package, install it with 
# install.packages(remotes)
# then, run the following to install cmdstanr
remotes::install_github("stan-dev/cmdstanr")
```

At this point, you can check that you will be able to compile `CmdStan`
and the `nmmr` models.

``` r
cmdstanr::check_cmdstan_toolchain()
```

1.  Use `CmdStanR` to install `CmdStan`

``` r
# if you have the resources, you can set a 'cores' option to speed this up
cmdstanr::install_cmdstan()
```

### The `nmmr` package

After installing `CmdStanR`, and `CmdStan`, `nmmr` can be installed with
the `remotes` package.

``` r
# If you don't have the remotes package, install it with 
# install.packages(remotes)
# then, run the following to install nmmr
remotes::install_github("psadil/nmmr")
```

# Next Steps

NeuroModulation Modeling comprises two kinds of analyses, a
rough-and-ready check and a Bayesian model. The functions for performing
these analyses are detailed in two vignettes.

-   Rough-and-ready, [orthogonal
    regression](https://psadil.github.io/nmmr/articles/orthogonal.html)
-   Full Model, (vignette to come)

## Code of Conduct

Please note that the `nmmr` project is released with a [Contributor Code
of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
