# Estimate Delaware over-time ideal points

# use this command to install development version of idealstan
# devtools::install_github('saudiwin/idealstan',branch='develop')

require(idealstan)
require(ggplot2)
require(dplyr)

load('DE 2018.Rdata')

# see if we can make idealstan data that works

to_ideal <- id_make(st.rc,ordinal=F,inflate=T,time='separate',include_pres=T)


# now see if we can estimate something
# random walk prior

estimate_rw <- id_estimate(to_ideal,use_vb = T,model_type = 2,
                            use_groups = T,nfix = 1,restrict_type='constrain_twoway',
                            restrict_ind_high = 2,
                            restrict_ind_low=1,
                            time_sd=15,
                            fixtype='vb')

# we can get all estimated parameters with summary. The legislator ideal points will be
# L_tp1[t,n]

all_params <- summary(estimate_rw,pars='L_tp1')

# look at plot 

all_params <- all_params %>% mutate(param_id=stringr::str_extract(parameters,'[0-9]\\]'),
                      param_id=as.numeric(stringr::str_extract(param_id,'[0-9]')),
                      param_id=factor(param_id,labels=c('R','X','D')),
                      time=stringr::str_extract(parameters,'\\[[0-9]+'),
                      time=as.numeric(stringr::str_extract(time,'[0-9]+')))

all_params <- left_join(all_params,
                        data_frame(time=unique(to_ideal@time_vals),
                                   time_vals=unique(to_ideal@time)))


rw_plot <- all_params %>% 
  filter(param_id!='X') %>% 
  ggplot(aes(y=posterior_median,x=time)) +
  geom_line(aes(colour=param_id),size=1) +
  geom_ribbon(aes(ymin=Prob.025,
                  ymax=Prob.975,
                  colour=param_id),
              alpha=0.3)

# now try with an AR(1) (stationary) model

estimate_ar <- id_estimate(to_ideal,use_vb = T,
                            use_groups = T,nfix = 1,restrict_type='constrain_twoway',
                            time_sd=10,
                            fixtype='constrained',
                            restrict_ind_high = 2,
                            restrict_ind_low=1,
                            use_ar = T)

# we can get all estimated parameters with summary. The legislator ideal points will be
# L_tp1[t,n]

all_params <- summary(estimate_ar,pars='L_tp1')

# look at plot 

all_params <- all_params %>% mutate(param_id=stringr::str_extract(parameters,'[0-9]\\]'),
                                    param_id=as.numeric(stringr::str_extract(param_id,'[0-9]')),
                                    param_id=factor(param_id,labels=c('X','R','D')),
                                    time=stringr::str_extract(parameters,'\\[[0-9]+'),
                                    time=as.numeric(stringr::str_extract(time,'[0-9]+')))

all_params <- left_join(all_params,
                        data_frame(time=unique(to_ideal@time_vals),
                                   time_vals=unique(to_ideal@time)))

ar_plot <- all_params %>% 
  filter(param_id!='X') %>% 
  ggplot(aes(y=posterior_median,x=time_vals)) +
  geom_line(aes(colour=param_id),size=1) +
  geom_ribbon(aes(ymin=Prob.025,
                  ymax=Prob.975,
                  colour=param_id),
              alpha=0.3)

gridExtra::grid.arrange(ar_plot,rw_plot)
rw_plot
ggsave('ar_rw_comparison2.png')

