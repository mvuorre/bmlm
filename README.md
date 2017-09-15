bmlm: An R package for Bayesian MultiLevel Mediation models
================

<!-- README.md is generated from README.Rmd. Please edit that file -->
[![CRAN version](http://www.r-pkg.org/badges/version/bmlm)](http://www.r-pkg.org/pkg/bmlm) [![download-badge](http://cranlogs.r-pkg.org/badges/bmlm)](https://cran.r-project.org/package=bmlm)

**bmlm** is an R package providing convenient methods for Bayesian estimation of multilevel mediation models using [Stan](http://mc-stan.org/).

For more details, visit the [package's website](https://mvuorre.github.io/bmlm/). A longer tutorial example is provided at <https://mvuorre.github.io/bmlm/articles/bmlm/bmlm.html>.

Install
=======

To install the latest stable version of bmlm from CRAN, run

``` r
install.packages("bmlm")
```

Install from GitHub
-------------------

Sometimes the stable version on CRAN is not the latest version of bmlm. bmlm is developed on GitHub, and users may obtain the latest (development) version from GitHub directly.

The latest development version of bmlm requires [devtools](https://cran.r-project.org/package=devtools) for installation. If you don't have the devtools package installed in R, first run this line:

``` r
install.packages("devtools")
```

Then proceed to install bmlm from GitHub:

``` r
devtools::install_github("mvuorre/bmlm", args = "--preclean")
```

Requirements
------------

Please ensure you have the latest version of R installed. Windows users may need to install RTools (more information on the [RStan website](https://github.com/stan-dev/rstan/wiki/Installing-RStan-on-Windows)), OS X users may need to install XCode ([more information](https://github.com/stan-dev/rstan/wiki/Installing-RStan-on-Mac-or-Linux)).

Example
-------

bmlm ships with an example data set from Intensive Longitudinal Methods: An Introduction to Diary and Experience Sampling Research ([Bolger & Laurenceau, 2013, chapter 9](http://www.intensivelongitudinal.com/)). To estimate the multilevel mediation model presented in that chapter, run:

``` r
library(bmlm)
data(BLch9)
fit <- mlm(BLch9)
```

After a while, you will have a joint posterior distribution of plausible parameter values from the model applied to this data. Inspect the model:

``` r
mlm_summary(fit)
```

Information
===========

Please contact the author of the package for questions and suggestions. I recommend creating a new issue on GitHub.

Citation
========

If you use this software, please cite it:

``` r
citation("bmlm")
#> 
#> To cite package bmlm in publications, please use:
#> 
#>   Vuorre, M., (2017). bmlm: Bayesian Multilevel Mediation. R
#>   package version 1.3.0. https://cran.r-project.org/package=bmlm
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {bmlm: Bayesian Multilevel Mediation},
#>     author = {Matti Vuorre},
#>     year = {2017},
#>     url = {https://cran.r-project.org/package=bmlm},
#>   }
```
