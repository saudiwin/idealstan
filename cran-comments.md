## Test environments
* ubuntu 14.04 (on travis-ci), R devel and release
* mac os x sierra (on travis-ci), R release
* win-builder, R release

## R CMD check results
There were no ERRORs or WARNINGs. 

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Robert Kubinec <rmk7@nyu.edu>'

New maintainer:
  Robert Kubinec <rmk7@nyu.edu>
Old maintainer(s):
  Robert Kubinec <rmk7xy@virginia.edu>

Explanation: I am keeping my contact information up to date.

* checking installed package size ... NOTE
  installed size is  7.3Mb
  sub-directories of 1Mb or more:
    libs   5.9Mb
    
Explanation: This package has a large installed library because it uses the Stan MCMC engine as a backend, and it comes with pre-compiled C++ modules that are loaded into Stan through the package `rstan`. As a result, the libraries are large, but the actual R code in the package is relatively small. 

* checking for GNU extensions in Makefiles ... NOTE
  GNU make is a SystemRequirements.
  
GNU Make is a requirement because of the C++ back-end used to compile and fit models using package `rstan`.


## Downstream dependencies
None.