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

to_ideal <- id_make(st.rc,ordinal=F,inflate=T,
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
# set a hard limit on the over-time ideal point variance restrict_var_high=.5 to prevent too much bounciness
# a hard limit of 0.25 is still fairly high
# we use the "vb_partial" option that will figure out optimal values to pin the Rs and Ds to in the 
# first time point. We only need to specify which to constrain high or low, i.e. restrict_ind_high and 
# restrict_ind_low


estimate_rw <- id_estimate(to_ideal,use_vb = T,model_type = 2,
                            use_groups = T,
                           restrict_sd=.01,restrict_var_high = .2,
                            time_sd=1,fixtype = 'vb_partial',restrict_ind_high = 'R',
                           restrict_ind_low = 'D')

# we can get all estimated parameters with summary. The legislator ideal points will be
# L_tp1[t,n]

all_params <- summary(estimate_rw,pars='L_tp1')

# look at plot 

id_plot_legis_dyn(estimate_rw,text_size_label = 6) + scale_color_manual(values=c('X'='green',
                                                                                 'D'='blue',
                                                                                 'R'='red'))

# now try with an AR(1) (stationary) model
# we won't set a hard limit on the variance, just set a low prior (time_sd=.1)
# the stationary assumption prevents explosive behavior

estimate_ar <- id_estimate(to_ideal,use_vb = T,
                           use_ar=T,
                           model_type = 2,
                           use_groups = T,
                           restrict_sd=.01,
                           restrict_var = F,
                           time_sd=.1,fixtype = 'vb_partial',restrict_ind_high = 'R',
                           restrict_ind_low = 'D')

id_plot_legis_dyn(estimate_ar,text_size_label = 6) + scale_color_manual(values=c('X'='green',
                                                                                 'D'='blue',
                                                                                 'R'='red'))


# The AR(1) model doesn't fit this data as well because
# the Democrats are clearly not stationary over this time period, so that violates the model assumptions
# to fit the AR(1) model better to this data we need more dis-aggregated time points (by month, for example)
# the AR(1) does show more movement in the Repupblicans ideal points, 
# which makes sense because if the Republicans are stationary over this time period than the AR(1)
# model will pick up on time-varying shocks that the random walk model ignores

# Having compared models, let's use the random walk model and
# do full Bayesian inference
# considerable trouble identifying this model because of the low number of groups (i.e., 3 parties)
# to identify it I added a default option for random-walk models that also constrains the 
# over-time mean of one of the parties (Democrats), not just fixing an initial value
# even so, the variance has to be restricted to .1

estimate_rw_full <- id_estimate(to_ideal,use_vb = F,
                           model_type = 2,
                           use_groups = T,
                           restrict_sd = .01,
                           restrict_var_high = .1,
                           time_sd=1,fixtype = 'vb_partial',restrict_ind_high = 'R',
                           restrict_ind_low = 'D')

id_plot_legis_dyn(estimate_rw_full,text_size_label = 6) + scale_color_manual(values=c('X'='green',
                                                                                      'D'='blue',
                                                                                      'R'='red'))


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

id_plot_legis_dyn(estimate_rw_indiv,text_size_label = 2,use_ci = F) + scale_color_manual(values=c('X'='green',
                                                                                                  'D'='blue',
                                                                                                  'R'='red'))


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

id_plot_legis_dyn(estimate_rw_indiv_highvar,text_size_label = 2,use_ci = F) + scale_color_manual(values=c('X'='green',
                                                                                                          'D'='blue',
                                                                                                          'R'='red'))


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

id_plot_legis_dyn(estimate_rw_indiv_veryhighvar,text_size_label = 2,use_ci = F) + scale_color_manual(values=c('X'='green',
                                                                                                              'D'='blue',
                                                                                                              'R'='red'))


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

id_plot_legis_dyn(estimate_rw_indiv_veryveryhighvar,text_size_label = 2,use_ci = F) + scale_color_manual(values=c('X'='green',
                                                                                                                  'D'='blue',
                                                                                                                  'R'='red'))


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

id_plot_legis_dyn(estimate_ar_indiv,text_size_label = 2,use_ci = F) + scale_color_manual(values=c('X'='green',
                                                                                                  'D'='blue',
                                                                                                  'R'='red'))


# it would seem the AR(1) model is generally more constrained than the random-walk model,
# but doesn't fit the data as well.


# now try a covariate model with party as a legislator-varying covariate
# need to reformulate the data first
# because parties are also group identifiers, we call the covariate ~group_id 
# (usually we'd just use the name of the column in the data)

to_ideal <- id_make(st.rc,ordinal=F,inflate=T,include_pres=T,
                    person_cov = ~group_id,
                    item_id="vote_labels",
                    time_id='years')

estimate_rw <- id_estimate(to_ideal,use_vb = T,model_type = 2,
                           use_groups = F,
                           restrict_sd=.01,restrict_var_high = 1,
                           time_sd=1)

# plot the covariates

id_plot_cov(estimate_rw,cov_type = 'person_cov')

# plot combined ideal point distributions (party intercepts + time-varying parameters)

id_plot_legis_dyn(estimate_rw,text_size_label = 2,use_ci = F) + scale_color_manual(values=c('X'='green',
                                                                                                  'D'='blue',
                                                                                                  'R'='red'))

# it is interesting that the model estimates look somewhat different. 
# parties can still be distinguished, but they are much closer together
# this is an artifact of using the random-walk model (party intercepts add substantial drift)
