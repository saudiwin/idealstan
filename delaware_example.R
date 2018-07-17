# Estimate Delaware over-time ideal points

# use this command to install development version of idealstan
# devtools::install_github('saudiwin/idealstan',branch='develop')

require(idealstan)
require(ggplot2)

load('DE 2018.Rdata')

# see if we can make idealstan data that works

to_ideal <- id_make(st.rc,ordinal=F,inflate=T,time='separate')


# now see if we can estimate something

estimate_all <- id_estimate(to_ideal,use_vb = T,
                            use_groups = T,nfix = 1,restrict_type='constrain_oneway')

# we can get all estimated parameters with summary. The legislator ideal points will be
# L_full[t,n]

all_params <- summary(estimate_all,pars='L_tp1')

# however, it's not so helpful to look at the raw data, so let's plot the ideal points over time

id_plot_legis_dyn(estimate_all,person_labels = 'fullname',group_labels = 'party',plot_text=T,
                  person_ci_alpha = .3)

# it's a bit of a spaghetti plot. Let's try highlighting some people

id_plot_legis_dyn(estimate_all,person_labels = 'fullname',group_labels = 'party',plot_text=T,
                  person_ci_alpha = .3,
                  highlight=sample(unique(st.rc$legis.data$fullname),2))

ggsave('test_graphic.png')
