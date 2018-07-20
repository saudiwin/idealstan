# testing ar models

require(idealstan)
require(tidyr)
require(dplyr)
require(stringr)

rand_walk <- id_sim_gen(time_points = 10)

ar1 <- id_sim_gen(time_points = 10,time_process = 'AR')

# see if we can recover parameters

rand_walk_fit <- id_estimate(rand_walk,model_type=2,use_vb=F)

# compare true to estimated values 

require(bayesplot)

true_l <- rand_walk@simul_data$true_person
mcmc_recover_hist(as.matrix(rand_walk_fit@stan_samples,pars='L_tp1[2,7]'),
                      true = c(true_l[2,7]))


get_estimates <- as.data.frame(rand_walk_fit@stan_samples,pars='L_tp1') %>% 
  mutate(iter=1:n()) %>% 
  gather(key = par,value=estimate,-iter) %>% 
  mutate(time_pt=str_extract(par,pattern = '\\[[0-9]+'),
         time_pt=str_replace(time_pt,'\\[',''),
         time_pt=as.numeric(time_pt),
         person=str_extract(par,pattern = '[0-9]+\\]'),
         person=str_replace(person,'\\]',''),
         person=as.numeric(person)) %>% 
  group_by(time_pt,person) %>% 
  summarize(mean_est=mean(estimate),
            high_est=quantile(estimate,.9),
            low_est=quantile(estimate,.1))

# compare to true values

true_d <- true_l[-1,] %>% 
  as_data_frame
names(true_d) <- 1:length(true_d)
true_d <- mutate(true_d,time_pt=1:n()) %>% 
  gather(key=person,value=true_val,-time_pt) %>% 
  mutate(person=as.numeric(person))

# merge and check

get_estimates <- left_join(get_estimates,true_d,by=c('time_pt','person'))

require(ggplot2)
ggplot(get_estimates,aes(y=mean_est,x=true_val)) +
  geom_point() + 
  stat_smooth(method='lm')

ar_fit <- id_estimate(ar1,model_type=2,use_vb=F,use_ar = T,
                      restrict_type='constrain_oneway',
                      nfix = 1)

true_l <- ar1@simul_data$true_person
mcmc_recover_hist(as.matrix(ar_fit@stan_samples,pars='L_full'),
                  true = c(true_l[1,]))


get_estimates <- as.data.frame(ar_fit@stan_samples,pars='L_tp1') %>% 
  mutate(iter=1:n()) %>% 
  gather(key = par,value=estimate,-iter) %>% 
  mutate(time_pt=str_extract(par,pattern = '\\[[0-9]+'),
         time_pt=str_replace(time_pt,'\\[',''),
         time_pt=as.numeric(time_pt),
         person=str_extract(par,pattern = '[0-9]+\\]'),
         person=str_replace(person,'\\]',''),
         person=as.numeric(person)) %>% 
  group_by(time_pt,person) %>% 
  summarize(mean_est=mean(estimate),
            high_est=quantile(estimate,.9),
            low_est=quantile(estimate,.1))

# compare to true values

true_d <- true_l[-1,] %>% 
  as_data_frame
names(true_d) <- 1:length(true_d)
true_d <- mutate(true_d,time_pt=n():1) %>% 
  gather(key=person,value=true_val,-time_pt) %>% 
  mutate(person=as.numeric(person))

# merge and check

get_estimates <- left_join(get_estimates,true_d,by=c('time_pt','person'))

require(ggplot2)
ggplot(get_estimates,aes(y=mean_est,x=true_val)) +
  geom_point() + 
  stat_smooth(method='lm')
