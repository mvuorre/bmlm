# bmlm: An R package for Bayesian MultiLevel Mediation models

__bmlm__ is an R package providing convenient methods for Bayesian estimation of multilevel mediation models using [Stan](http://mc-stan.org/).

For an example, please see [a short introduction to bmlm](https://mvuorre.github.io/bmlm/).

# Install

### Pre-requisites 

Please ensure you have the latest version of R installed.

This package depends on [Stan](http://mc-stan.org/). Please see [here](http://mc-stan.org/interfaces/rstan.html) for how to install the R Stan interface.

### Installing bmlm

Currently, bmlm is only available on GitHub, and therefore requires [devtools](https://cran.r-project.org/package=devtools) for installation. If you don't have the devtools package installed in R, first run this line:

```r
install.packages(devtools)
```

Then proceed to install bmlm:

```r
devtools::install_github("mvuorre/bmlm")
```

## Example

bmlm ships with an example data set from Intensive Longitudinal Methods: An Introduction to Diary and Experience Sampling Research ([Bolger & Laurenceau, 2013, chapter 9](http://www.intensivelongitudinal.com/)). To estimate the multilevel mediation model presented in that chapter, run:

```r
library(bmlm)
data(BLch9)
fit <- mlm(BLch9)
```

After a while, you will have a joint posterior distribution of plausible parameter values from the model applied to this data. Inspect the model:

```r
mlm_summary(fit)
```

# Information

bmlm is in active development, please contact the author of the package for questions and suggestions.

