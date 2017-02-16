## Resubmission

Resubmission; previous CRAN submission rejected because of NOTE:

* checking compiled code ... NOTE
File ‘bmlm/libs/bmlm.so’:
  Found no calls to: ‘R_registerRoutines’, ‘R_useDynamicSymbols’
  
This NOTE cannot be removed because it results from compiled RStan code. (Currently, all CRAN packages that depend on compiled RStan code have the same NOTE in check results.)

## Test environments
* local OS X install, R 3.3.2
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warning  | 0 notes

## Reverse dependencies

N/A


