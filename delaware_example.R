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
                           restrict_sd=.01,
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

estimate_rw_full <- id_estimate(to_ideal,use_vb = T,
                           model_type = 2,
                           use_groups = T,
                           restrict_sd = .01,
                           restrict_var_high = .1,
                           time_sd=.05,fixtype = 'vb_partial',restrict_ind_high = 'R',
                           restrict_ind_low = 'D')

id_plot_legis_dyn(estimate_rw_full,text_size_label = 6)
