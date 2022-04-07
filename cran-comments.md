## Test environments
* ubuntu 14.04 (on travis-ci), R devel and release
* mac os x sierra (on travis-ci), R release
* win-builder, R devel and release

## R CMD check results
There were no ERRORs or WARNINGs. 2 NOTEs:

* checking for GNU extensions in Makefiles ... NOTE
  GNU make is a SystemRequirements.
  
GNU Make is a requirement because of the C++ back-end used to compile and fit models using package `rstan`.

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Robert Kubinec <rmk7@nyu.edu>'

New submission

Package was archived on CRAN

CRAN repository db overrides:
  X-CRAN-Comment: Archived on 2019-07-01 as check problems were not
    corrected in time.


## Downstream dependencies
None.