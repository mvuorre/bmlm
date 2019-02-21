# bmlm 1.3.11

Fix package for staged installation

# bmlm 1.3.9

* Fix NOTE about methods package

# bmlm 1.3.8

* Update to C++14, thanks to Ben Goodrich.

# bmlm 1.3.7

* Removed `tab2doc()`, package no longer needs archived ReporteRs package.

# bmlm 1.3.6

* Deprecate `tab2doc()` because required package ReporteRs is archived.

# bmlm 1.3.5

* Fix (harmless) constructor error message

# bmlm 1.3.4

* Minor cleaning of Stan code
* Fix typos in documentation

# bmlm 1.3.3

* Change the label '%me' to 'pme' (for proportion mediated effect) in output of mlm_path_plot(..., text = TRUE).

# bmlm 1.3.2

* Add options to `mlm_spaghetti_plot()` to allow jittering and adjusting size of the error bars.

# bmlm 1.3.1

* `mlm_spaghetti_plot()` now has argument `mx` which can be set to `mx = "data"` to plot the spaghetti plot of the M - Y relationship (b path) such that the X values are from data, and not fitted values from the X - M model (a path). The argument defaults to `mx = "fitted"`, such that the X axis values of the M - Y spaghetti plot are fitted values.

# bmlm 1.3.0

* New function `mlm_spaghetti_plot()` for visualizing model-fitted values for paths a (X->M regression) and b (M->Y regression)

# bmlm 1.2.10

* Default priors are now $Normal(0, 1000)$ for regression coefficients, and $Cauchy(0, 50)$ for group-level SDs
* `mlm_summary()` now gives only population level parameters by default, and group-level parameters when `pars = "random"`
* Renamed the mediated effect parameter to *me* to distinguish it from the product of *a* and *b* (similarly for group-level *u_me*)
* `mlm_path_plot()` now draws a template if no model is entered (i.e. `template` argument is deprecated)
* `mlm_path_plot()` now by default also shows SDs of group-level effects. This behavior can be turned off by specifying `random = FALSE`
* The fitted model object doesn't contain the whole covariance matrix anymore, but now contains the group-level intercepts
* New example data set included in package: `MEC2010`
* Posterior standard deviation is now referred to as SE in `mlm_summary()`



# bmlm 1.2.9

Removed sigma_y from being modeled when binary_y = TRUE. 

# bmlm 1.2.1

Removed posterior probabilities from default outputs.

Added type = "violin" as option for plotting coefficients with mlm_pars_plot().

# bmlm 1.2.0

Users may now change each individual regression parameter's prior, instead of classes of priors.

Users may now change the shape parameter of the LKJ prior.

# bmlm 1.1.1

Coefficient plots now reorder parameter estimates, if user has requested varying effects.

Path plot now by default does not scale the edges.

# bmlm 1.1.0

## Major update

bmlm now uses pre-compiled C++ code for the Stan models, which eliminates the need to compile a model each time `mlm()` is run. This significantly speeds up model estimation.

## Minor update

The Stan code used by `mlm()` is now built from separate chunks, allowing more flexible and robust model development.

# bmlm 1.0.0

Initial release to CRAN.
