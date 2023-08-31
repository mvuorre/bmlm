## Test environments

- Local Debian Linux, R-devel, clang, ISO-8859-15 locale (`rhub::local_check_linux(image = "rhub/debian-clang-devel")`)
- Local MacOS Ventura 13.4.1c (latest); R 4.3.1 (`devtools::check(document = FALSE, cran = FALSE)`)
- R-hub Windows Server 2022, R-release, 32/64 bit (`rhub::check_on_windows()`)

## R CMD check results

4 notes.

❯ checking C++ specification ... NOTE
    Specified C++14: please drop specification unless essential

- Required by dependency rstan.

❯ checking installed package size ... NOTE
    installed size is 10.7Mb
    sub-directories of 1Mb or more:
      libs  10.3Mb

- Only occurred with Debian Linux build.

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
