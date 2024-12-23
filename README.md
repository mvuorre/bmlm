bmlm: R package for Bayesian multilevel mediation
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

# bmlm

[![CRAN
version](https://www.r-pkg.org/badges/version/bmlm)](https://www.r-pkg.org/pkg/bmlm)
[![download-badge](https://cranlogs.r-pkg.org/badges/bmlm)](https://cran.r-project.org/package=bmlm)

**bmlm** is an R package providing convenient methods for Bayesian
estimation of multilevel mediation models using
[Stan](https://mc-stan.org/).

The package’s source code is hosted on
[GitHub](https://github.com/mvuorre/bmlm/). More information can be
found on the [bmlm’s website](https://mvuorre.github.io/bmlm/).

# Install

## Requirements

Please ensure you have the latest version of R installed. Windows users
may need to install RTools, and OS X users may need to install XCode
(see <https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started>.

## Install from CRAN

To install the latest stable version of bmlm from CRAN, run

``` r
install.packages("bmlm")
```

## Install from GitHub

Sometimes the stable version on CRAN is not the latest version of bmlm.
bmlm is developed on GitHub, and users may obtain the latest
(development) version from GitHub directly.

The latest development version of bmlm requires
[remotes](https://cran.r-project.org/package=remotes) for installation.
If you don’t have the remotes package installed in R, first run this
line:

``` r
install.packages("remotes")
```

Then proceed to install bmlm from GitHub:

``` r
remotes::install_github("mvuorre/bmlm")
```

# Information

Please contact the author of the package for questions and suggestions.
I recommend creating a new issue on GitHub.

# Citation

If you use this software, please cite it:

``` r
citation("bmlm")
#> To cite package 'bmlm' in publications use:
#> 
#>   Vuorre M, Bolger N (2018). "Within-subject mediation analysis for
#>   experimental data in cognitive psychology and neuroscience."
#>   _Behavior Research Methods_. doi:10.3758/s13428-017-0980-9
#>   <https://doi.org/10.3758/s13428-017-0980-9>.
#> 
#>   Vuorre M (2023). _bmlm: Bayesian Multilevel Mediation_. R package
#>   version 1.3.15, <https://CRAN.R-project.org/package=bmlm>.
#> 
#> To see these entries in BibTeX format, use 'print(<citation>,
#> bibtex=TRUE)', 'toBibtex(.)', or set
#> 'options(citation.bibtex.max=999)'.
```
