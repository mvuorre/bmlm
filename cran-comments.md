## Test environments

- Local MacOS Ventura, R 4.3.1
- Remote Fedora Linux, R-devel, clang, gfortran
- winbuilder

## R CMD check results

❯ checking dependencies in R code ... NOTE
  Namespaces in Imports field not imported from:
    'RcppParallel' 'rstantools'
    All declared Imports should be used.

- Required by dependency rstan.

❯ checking for GNU extensions in Makefiles ... NOTE
  GNU make is a SystemRequirements.

- Required by dependency rstan.

# Reverse dependencies

N/A
