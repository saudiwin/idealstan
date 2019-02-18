## Test environments
* ubuntu 14.04 (on travis-ci), R devel and release
* mac os x sierra (on travis-ci), R release
* win-builder, R devel

## R CMD check results
There were no ERRORs or WARNINGs. 3 NOTEs:

* checking installed package size ... NOTE
  installed size is  6.2Mb
  sub-directories of 1Mb or more:
    libs   4.1Mb
    
Explanation: This package has a large installed library because it uses the Stan MCMC engine as a backend, and it comes with pre-compiled C++ modules that are loaded into Stan through the package `rstan`. As a result, the libraries are large, but the actual R code in the package is relatively small. 

* checking for GNU extensions in Makefiles ... NOTE
  GNU make is a SystemRequirements.
  
GNU Make is a requirement because of the C++ back-end used to compile and fit models using package `rstan`.

* checking line endings in C/C++/Fortran sources/headers ... NOTE
Found the following sources/headers with CR or CRLF line endings:
  inst/include/meta_header.hpp
Some Unix compilers require LF line endings.

I keep the primary code base on a Windows machine which is likely why there are some CR/CRLF line endings. However, this does not affect compile behavior as I tested this on Ubuntu with both R devel and release.


## Downstream dependencies
None.