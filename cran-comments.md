## Test environments
* ubuntu 14.04 (on travis-ci), R devel and release
* mac os x sierra (on travis-ci), R release
* local windows, R 3.5.1

## R CMD check results
There were no ERRORs or WARNINGs. 

* checking installed package size ... NOTE
  installed size is 10.0Mb
  sub-directories of 1Mb or more:
    data   3.5Mb
    libs   5.4Mb
    
Explanation: This package has a large installed library because it uses the Stan MCMC engine as a backend, and it comes with pre-compiled C++ modules that are loaded into Stan through the package `rstan`. As a result, the libraries are large, but the actual R code in the package is small. 


## Downstream dependencies
None.