## Test environments
* local OS X install, R 3.3.1
* win-builder (devel and release)

## R CMD check results

0 errors | 1 warning  | 0 notes

Status: 1 WARNING
checking compiled code ... WARNING
File ‘bmlm/libs/bmlm.so’:
  Found ‘___assert_rtn’, possibly from ‘assert’ (C)
    Object: ‘Modules.o’

Compiled code should not call entry points which might terminate R nor
write to stdout/stderr instead of to the console, nor the system RNG.

## Win-builder results

Installation time in seconds: 197
Check time in seconds: 108
Status: 1 WARNING

* checking whether package 'bmlm' can be installed ... WARNING
Found the following significant warnings:
  d:/RCompile/CRANpkg/lib/3.4/BH/include/boost/math/special_functions/detail/bernoulli_details.hpp:127:36: warning: ISO C++ 1998 does not support 'long long' [-Wlong-long]
  d:/RCompile/CRANpkg/lib/3.4/BH/include/boost/math/special_functions/detail/bernoulli_details.hpp:151:9: warning: ISO C++ 1998 does not support 'long long' [-Wlong-long]
See 'd:/RCompile/CRANguest/R-devel/bmlm.Rcheck/00install.out' for details.

RStanarm is on CRAN with these warnings, so might not be bad.

## Reverse dependencies

N/A


