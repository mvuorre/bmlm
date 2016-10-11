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

Installation time in seconds: 126
Check time in seconds: 64
Status: 1 NOTE
R Under development (unstable) (2016-09-29 r71397)

* checking top-level files ... NOTE
Non-standard file/directory found at top level:
  'win-builder-notes'
  
(This directory is now removed so I expect a successful build)

## Reverse dependencies

N/A


