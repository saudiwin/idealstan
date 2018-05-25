# Estimate Delaware over-time ideal points

require(idealstan)

load('DE 2018.Rdata')

# see if we can make idealstan data that works

to_ideal <- id_make(st.rc,ordinal=F,inflate=T,time='separate')


# now see if we can estimate something

estimate_all <- id_estimate(to_ideal,use_vb = T)
