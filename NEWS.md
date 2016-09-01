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
