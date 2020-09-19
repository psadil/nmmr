
<!-- README.md is generated from README.Rmd. Please edit that file -->

# vtuner

## Installation

vtuner relies on the rstan interface to [Stan](https://mc-stan.org). To
install vtuner, first [follow instructions for setting up
rstan](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started).

After successfully installing rstan, vtuner can be installed with
devtools

``` r
# install.packages(devtools)
library(devtools)
devtools::install_gitlab("psadil/vtuner")
```

# Example Analysis

## Data

A sample dataset is provided with this package. The dataset contains the
beta values for a single participant, and shows the format expected by
the functions of this package. The dataset can be loaded with the
following command.

``` r
# library(vtuner)
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

For extra info on the dataset, see the help page for betas (?betas).

## Run Stan

The technique works by comparing separate models, each of which allows
just a single kind of modulation to the neural tuning functions. The two
kinds of neuromodulation currently implemented are *Additive* and
*Multiplicative*. Source for the models can be found on this [package’s
repository](https://gitlab.com/psadil/vtuner/tree/master/src/stan_files).
The three models are largely the same, differing only slightly in the
NTFs for the high contrast.

### Define Stan options

In this simple example, most voxel-wise parameters are not saved (e.g.,
the weights for each channel in each voxel, the value of the modulation
parameter for each voxel). Excluding these parameters drastically
reduces the size of the output and speeds up post-processing. The
parameter *mu*, which is the distribution of the beta values for each
trial, might also be worth dropping. *mu* is kept here because it is
required for model comparison.

A few additional parameters are used to control Stan’s sampling
behavior. See the help page for rstan::stan. Running one chain may
require a few hours.
