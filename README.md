
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nmmr

Experimental package for NeuroModulation Modeling in R. This package
under active development and not yet released.

## Installation

`nmmr` relies on the [RStan](https://github.com/stan-dev/rstan)
interface to [Stan](https://mc-stan.org). To install `nmmr`, first
[follow instructions for setting up
RStan](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started).

After installing `RStan`, `nmmr` can be installed with the `remotes`
package.

``` r
# install.packages(remotes)
library(remotes)
devtools::install_github("psadil/nmmr")
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

| sub | run | voxel  | contrast | orientation |          y | ses |
| :-- | :-- | :----- | :------- | ----------: | ---------: | :-- |
| 2   | 15  | 191852 | low      |   0.7853982 |   3.359860 | 3   |
| 2   | 15  | 197706 | low      |   0.7853982 | \-2.839522 | 3   |
| 2   | 15  | 197769 | low      |   0.7853982 | \-2.027267 | 3   |
| 2   | 15  | 197842 | low      |   0.7853982 |   2.234859 | 3   |
| 2   | 15  | 197906 | low      |   0.7853982 |   2.858387 | 3   |
| 2   | 15  | 197907 | low      |   0.7853982 |   1.506754 | 3   |

For extra info on the dataset, see the help page for sub02 (?sub02).
