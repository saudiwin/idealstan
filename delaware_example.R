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

estimate_all <- id_estimate(to_ideal,use_vb = T,
                            use_groups = T,nfix = 1,restrict_type='constrain_oneway')

# we can get all estimated parameters with summary. The legislator ideal points will be
# L_full[t,n]

all_params <- summary(estimate_all,pars='L_tp1')

# look at plot 

all_params <- all_params %>% mutate(param_id=stringr::str_extract(parameters,'[0-9]\\]'),
                      param_id=as.numeric(stringr::str_extract(param_id,'[0-9]')),
                      param_id=factor(param_id,labels=unique(to_ideal@person_data$group)),
                      time=stringr::str_extract(parameters,'\\[[0-9]+'),
                      time=as.numeric(stringr::str_extract(time,'[0-9]+')))

all_params %>% 
  filter(param_id!='X') %>% 
  ggplot(aes(y=posterior_median,x=time)) +
  geom_line(aes(colour=param_id),size=1) +
  geom_ribbon(aes(ymin=Prob.025,
                  ymax=Prob.975,
                  colour=param_id),
              alpha=0.3)

# now try with an AR(1) (stationary) model

estimate_all <- id_estimate(to_ideal,use_vb = T,
                            use_groups = T,nfix = 1,restrict_type='constrain_oneway',
                            fixtype='constrained',
                            restrict_ind_high = 2,
                            use_ar = T)

# we can get all estimated parameters with summary. The legislator ideal points will be
# L_full[t,n]

all_params <- summary(estimate_all,pars='L_tp1')

# look at plot 

all_params <- all_params %>% mutate(param_id=stringr::str_extract(parameters,'[0-9]\\]'),
                                    param_id=as.numeric(stringr::str_extract(param_id,'[0-9]')),
                                    param_id=factor(param_id,labels=unique(to_ideal@person_data$group)),
                                    time=stringr::str_extract(parameters,'\\[[0-9]+'),
                                    time=as.numeric(stringr::str_extract(time,'[0-9]+')))

all_params %>% 
  filter(param_id!='X') %>% 
  ggplot(aes(y=posterior_median,x=time)) +
  geom_line(aes(colour=param_id),size=1) +
  geom_ribbon(aes(ymin=Prob.025,
                  ymax=Prob.975,
                  colour=param_id),
              alpha=0.3)

# however, it's not so helpful to look at the raw data, so let's plot the ideal points over time

id_plot_legis_dyn(estimate_all,person_labels = 'fullname',group_labels = 'party',plot_text=T,
                  person_ci_alpha = .3)

# it's a bit of a spaghetti plot. Let's try highlighting some people

id_plot_legis_dyn(estimate_all,person_labels = 'fullname',group_labels = 'party',plot_text=T,
                  person_ci_alpha = .3,
                  highlight=sample(unique(st.rc$legis.data$fullname),2))

ggsave('test_graphic.png')
