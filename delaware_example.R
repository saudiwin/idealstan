# Estimate Delaware over-time ideal points

# use this command to install development version of idealstan
# devtools::install_github('saudiwin/idealstan',ref='develop',local=F)

require(idealstan)
require(ggplot2)
require(dplyr)
require(stringr)

load('DE 2018.Rdata')

# see if we can make idealstan data that works
# convert st.rc vote.data to contain dates
# we also need the bill labels to be unique, which they currently aren't

vote_labels <- colnames(st.rc$votes)
years <- str_extract(vote_labels,'\\_[a-z][0-9]+') %>% 
  str_replace_all('\\_|[a-z]',"") 
# make years a date by adding month/day
years <- paste0(years,'-01-01')
years <- lubridate::ymd(years)
vote_labels <- as.character(1:ncol(st.rc$votes))
st.rc$vote.data <- data_frame(vote_labels=vote_labels,
                              years=years)
colnames(st.rc$votes) <- vote_labels

to_ideal <- id_make(st.rc,ordinal=F,inflate=T,include_pres=T,
                    item_id="vote_labels",
                    time_id='years')

# look at distribution of outcome

table(to_ideal@score_matrix$outcome)

# Values of 3 indicate missing (absent from vote)

# values coded as NA, though, indicate missing from legislature

sum(is.na(to_ideal@score_matrix$outcome))

# approximately half of the values are NA and will be dropped from the estimation (i.e. considered ignorable)


# now see if we can estimate something
# random walk prior

estimate_rw <- id_estimate(to_ideal,use_vb = T,model_type = 2,
                            use_groups = T,
                           restrict_sd=.01,restrict_var_high = .25,
                            time_sd=.1,fixtype = 'vb_partial',restrict_ind_high = 'R',
                           restrict_ind_low = 'D')

# we can get all estimated parameters with summary. The legislator ideal points will be
# L_tp1[t,n]

all_params <- summary(estimate_rw,pars='L_tp1')

# look at plot 

id_plot_legis_dyn(estimate_rw,text_size_label = 6)

# now try with an AR(1) (stationary) model

estimate_ar <- id_estimate(to_ideal,use_vb = T,
                           model_type = 2,
                           use_groups = T,
                           restrict_sd=.01,
                           time_sd=.5,fixtype = 'vb_partial',restrict_ind_high = 'R',
                           restrict_ind_low = 'D')

id_plot_legis_dyn(estimate_ar,text_size_label = 6)

# The AR(1) model doesn't fit this data very well because
# 1) very few time points
# 2) time points are far apart (years). Not enough data to identify the AR(1) parameters

# Having compared models, let's use the random walk model and
# do full Bayesian inference

estimate_rw_full <- id_estimate(to_ideal,use_vb = F,
                           model_type = 2,
                           use_groups = T,
                           restrict_sd = .01,
                           restrict_var_high = .1,
                           time_sd=1,fixtype = 'vb_partial',restrict_ind_high = 'R',
                           restrict_ind_low = 'D')

id_plot_legis_dyn(estimate_rw_full,text_size_label = 6)

# now we can also look at individual legislator ideal points
# this lets the function select which legislators to contrain 
# it will pick those two that have starting values farthest apart (i.e. at t=1)

estimate_rw_indiv <- id_estimate(to_ideal,use_vb = T,
                                model_type = 2,
                                use_groups = F,
                                restrict_sd = .01,
                                restrict_var_high = .15,
                                time_sd=1,
                                fixtype = 'vb_full')

# turn off CI plots (too many lines) and make text labels smaller

id_plot_legis_dyn(estimate_rw_indiv,text_size_label = 2,use_ci = F)

# not a lot of movement in the lines (which isn't necessarily surprising)
# we can increase the upper limit of variance a bit and re-fit

estimate_rw_indiv_highvar <- id_estimate(to_ideal,use_vb = T,
                                 model_type = 2,
                                 use_groups = F,
                                 restrict_sd = .01,
                                 restrict_var_high = .3,
                                 time_sd=1,
                                 fixtype = 'vb_full')

# increasing allows *some* greater movement, but not a ton
# however Democrats and Republicans now seem to be clustered more on the correct side

id_plot_legis_dyn(estimate_rw_indiv_highvar,text_size_label = 2,use_ci = F)

# let's increase variance even further

estimate_rw_indiv_veryhighvar <- id_estimate(to_ideal,use_vb = T,
                                         model_type = 2,
                                         use_groups = F,
                                         restrict_sd = .01,
                                         restrict_var_high = .6,
                                         time_sd=1,
                                         fixtype = 'vb_full')

# even more interesting variation with higher variance
# it looks like when the number of persons/groups is high, it's OK to unconstrain the variance parameter

id_plot_legis_dyn(estimate_rw_indiv_veryhighvar,text_size_label = 2,use_ci = F)

# let's try one more fit with high variance

# let's increase variance even further

estimate_rw_indiv_veryveryhighvar <- id_estimate(to_ideal,use_vb = T,
                                             model_type = 2,
                                             use_groups = F,
                                             restrict_sd = .01,
                                             restrict_var_high = 1,
                                             time_sd=1,
                                             fixtype = 'vb_full')

# We probably don't want to allow much more variance than this
# ideal points are on the whole still quite stable

id_plot_legis_dyn(estimate_rw_indiv_veryveryhighvar,text_size_label = 2,use_ci = F)

# let's try an AR(1) individual legislator model
# probably overkill with the small number of time points that are spread far apart
# we'll also use the prior fit for identification
# make sure to turn off the mean restriction (that will screw with the AR(1) model)

estimate_ar_indiv <- id_estimate(to_ideal,use_vb = T,
                                                 model_type = 2,
                                                 use_groups = F,
                                                 restrict_sd = .01,
                                                use_ar = T,
                                                 time_sd=1,
                                                restrict_mean=FALSE,
                                                 fixtype = 'prior_fit',
                                                  prior_fit=estimate_rw_indiv_veryveryhighvar)

id_plot_legis_dyn(estimate_ar_indiv,text_size_label = 2,use_ci = F)

# it would seem the AR(1) model is generally more constrained than the random-walk model,
# but doesn't fit the data as well.





