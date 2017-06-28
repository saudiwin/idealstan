# Test file to show how package works

# Import latest senate data using pscl
require(pscl)
require(dplyr)
require(idealstan)
require(tidyr)
require(ggplot2)
require(bayesplot)
require(readr)
require(wnominate)

#this is out of date, use new CSV Files
newdata <- readKH(file='senate_114.ord')

# let's see what pscl/dwnominate do with the data

pscl_model <- ideal(newdata,store.item=TRUE,normalize = TRUE)
wnominate_model <- wnominate(newdata,polarity='SASSE (R NE)',dims=1,trials=10)
# we can compare models directly by forming an idealstan object out of pscl_model

pscl_pts <- data_frame(ideal_pts=apply(pscl_model$x,2,median),
                       high_pt=apply(pscl_model$x,2,quantile,probs=0.95),
                       low_pt=apply(pscl_model$x,2,quantile,probs=0.05),
                       model_type='IRT 2-PL')
wnominate_pts <- data_frame(ideal_pts=wnominate_model$legislators$coord1D,
                            high_pt=wnominate_model$legislators$coord1D + 1.96*wnominate_model$legislators$se1D,
                            low_pt=wnominate_model$legislators$coord1D - 1.96*wnominate_model$legislators$se1D,
                            model_type='W-NOMINATE')

# now run idealstan

to_use <- newdata$votes[-1, ]
newdata$legis.data <- newdata$legis.data[-1, ]
to_use <- apply(to_use, 2, function(x) {
  y <- recode(
    x,
    `1` = 3L,
    `6` = 1L,
    `7` = 2L,
    `9` = 4L
  )
  return(y)
})

rownames(to_use) <- rownames(newdata$legis.data)

idealdata <-
  make_idealdata(
    vote_data = to_use,
    legis_data = newdata$legis.data,
    abs_vote = 4,
    yes_vote = 3,
    no_vote = 1,
    abst_vote = 2,
    ordinal = TRUE
  )

estimated_full <-
  estimate_ideal(idealdata = idealdata,
                 model_type = 4,
                 use_vb = F,
                 ncores=4,
                 nfix=1,
                 restrict_type='constrain_twoway',
                 restrict_params='legis',
                 restrict_ind_high = which(row.names(newdata$votes)=='SASSE (R NE)'),
                 restrict_ind_low = which(row.names(newdata$votes)=='SANDERS (Indep VT)'),
                 auto_id=F,
                 fixtype='constrained',
                 abs_discrim_sd = 2.5,
                 reg_discrim_sd = 2.5,
                 legis_sd = 1)
output <- rstan::extract(estimated_full@stan_samples)
abs_pts <- data_frame(ideal_pts=apply(output$L_full,2,median),
                         high_pt=apply(output$L_full,2,quantile,probs=0.95),
                         low_pt=apply(output$L_full,2,quantile,probs=0.05),
                         model_type='Absence-Inflated')

all_perf <- bind_rows(pscl_pts,wnominate_pts,abs_pts)