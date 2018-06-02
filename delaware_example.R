# Estimate Delaware over-time ideal points

require(idealstan)

load('DE 2018.Rdata')

# see if we can make idealstan data that works

to_ideal <- id_make(st.rc,ordinal=F,inflate=T,time='separate')


# now see if we can estimate something

estimate_all <- id_estimate(to_ideal,use_vb = T)

# we can get all estimated parameters with summary. The legislator ideal points will be
# L_full[t,n]

all_params <- summary(estimate_all,pars='L_full')

# however, it's not so helpful to look at the raw data, so let's plot the ideal points over time

id_plot_legis_dyn(estimate_all)
