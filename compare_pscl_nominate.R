# Test file to show how package works

# Import latest senate data using pscl
require(pscl)
require(dplyr)
require(idealstan)
require(tidyr)
require(ggplot2)
require(bayesplot)
require(readr)

#this is out of date, use new CSV Files
newdata <- readKH(file='senate_114.ord')

my_bin_abs <- readRDS('full_104_absence.rds')
my_bin_noabs <- readRDS('no_inflate_fullid.rds')
# let's see what pscl/dwnominate do with the data

pscl_model <- ideal(newdata,store.item=TRUE,normalize = TRUE)

# we can compare models directly by forming an idealstan object out of pscl_model

pscl_pts <- data_frame(ideal_pts=apply(pscl_model$x,2,median),
                       high_pt=apply(pscl_model$x,2,quantile,probs=0.9),
                       low_pt=apply(pscl_model$x,2,quantile,probs=0.1),
                       model_type='pscl')
output <- rstan::extract(my_bin_noabs@stan_samples)
no_abs_pts <- data_frame(ideal_pts=apply(output$L_full,2,median),
                         high_pt=apply(output$L_full,2,quantile,probs=0.9),
                         low_pt=apply(output$L_full,2,quantile,probs=0.1),
                         model_type='stan_no_abs')