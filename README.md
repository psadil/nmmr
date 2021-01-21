
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nmmr

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/psadil/nmmr/branch/master/graph/badge.svg)](https://codecov.io/gh/psadil/nmmr?branch=master)
<!-- badges: end -->

Experimental package for NeuroModulation Modeling in R. This package
under active development and not yet released.

## Installation

`nmmr` relies on the [cmdstanR](https://mc-stan.org/cmdstanr/) interface
to [Stan](https://mc-stan.org). To install `nmmr`, first [follow
instructions for setting up Stan and
cmdstanR](https://mc-stan.org/cmdstanr/articles/cmdstanr.html).

After installing `cmdstanR`, `nmmr` can be installed with the `remotes`
package.

``` r
# install.packages(remotes)
library(remotes)
remotes::install_github("psadil/nmmr")
```

# Example Analysis

## Data

The package is bundled with a sample dataset. The dataset contains the
beta values for a single participant and shows the format expected by
the functions of this package. The dataset can be loaded with the
following command.

``` r
# library(nmmr)
data("sub02")
knitr::kable(head(sub02))
```

| sub | run | voxel  | contrast | orientation |         y | ses |
|:----|:----|:-------|:---------|------------:|----------:|:----|
| 2   | 15  | 191852 | low      |   0.7853982 |  3.359860 | 3   |
| 2   | 15  | 197706 | low      |   0.7853982 | -2.839522 | 3   |
| 2   | 15  | 197769 | low      |   0.7853982 | -2.027267 | 3   |
| 2   | 15  | 197842 | low      |   0.7853982 |  2.234859 | 3   |
| 2   | 15  | 197906 | low      |   0.7853982 |  2.858387 | 3   |
| 2   | 15  | 197907 | low      |   0.7853982 |  1.506754 | 3   |

For extra info on the dataset, see the help page for sub02 (?sub02).

## Code of Conduct

Please note that the `nmmr` project is released with a [Contributor Code
of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
