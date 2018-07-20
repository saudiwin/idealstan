# testing ar models

require(rstan)
require(tidyr)
require(dplyr)
require(stringr)
require(bayesplot)

# do the simulation here

num_bills <- 100
num_person <- 25
time_points <- 25
absence_diff_mean <- 0
absence_discrim_sd <- 2
ideal_pts_sd <- 1
diff_sd <- 2
reg_discrim_sd <- 2

prior_func <- function(params) {
  
  output <- rnorm(n=params$N,mean=params$mean,sd=params$sd)
  
}

absence_diff <- prior_func(params=list(N=num_bills,mean=absence_diff_mean,sd=diff_sd)) 
absence_discrim <- prior_func(params=list(N=num_bills,mean=0,sd=absence_discrim_sd))

ideal_t1 <- prior_func(params=list(N=num_person,mean=0,sd=ideal_pts_sd))
# random AR parameters
ar_adj <- runif(n = num_person,min = -0.5,max=0.5)
# drift parameters
drift <- prior_func(params=list(N=num_person,mean=0,sd=ideal_pts_sd))

.gen_ts_data <- function(t,adj_in,alpha_int,sigma,init_sides) {
  current_val <- new.env()
  current_val$t1 <- 0
  
  out_vec <- lapply(1:t,function(t_1) {
    
    if(t_1==1) {
      t_11 <- init_sides
      current_val$t1 <- t_11
      return(data_frame(t_11))
    } else {
      t_11 <- alpha_int + adj_in*current_val$t1 + rnorm(n=1,sd=sigma/10)
    }
    current_val$t1 <- t_11
    return(data_frame(t_11))
  })  %>% bind_rows
  return(out_vec)
}

ideal_pts <- lapply(1:num_person, function(i) {
  this_person <- .gen_ts_data(t=time_points,
                              adj_in=ar_adj[i],
                              alpha_int=drift[i],
                              sigma=ideal_pts_sd/4,
                              init_sides=ideal_t1[i])
  return(this_person)
}) %>% bind_cols %>% as.matrix

person_points <- rep(1:num_person,times=num_bills)
bill_points <- rep(1:num_bills,each=num_person)

time_points <- rep(1:time_points,each=num_bills/time_points)
time_points <- time_points[bill_points]

pr_absence <- sapply(1:length(person_points),function(n) {
  ideal_pts[time_points[n],person_points[n]]*absence_discrim[bill_points[n]] - absence_diff[bill_points[n]]
}) %>% plogis()

reg_diff <- prior_func(params=list(N=num_bills,mean=0,sd=diff_sd))
reg_discrim <- prior_func(params=list(N=num_bills,mean=0,sd=reg_discrim_sd))

# this is the same for all DGPs

pr_vote <- sapply(1:length(person_points),function(n) {
  ideal_pts[time_points[n],person_points[n]]*reg_discrim[bill_points[n]] - reg_diff[bill_points[n]]
}) %>% plogis()

N <- length(pr_vote)
votes <- as.numeric(pr_vote>runif(N))

combined <- if_else(pr_absence<runif(N),votes,2)

combined <- matrix(combined,ncol=num_bills,nrow=num_person,byrow = F)

colnames(combined) <- paste0('Vote_',1:ncol(combined))
row.names(combined) <- paste0('person_',1:nrow(combined))

# need to select high, middle and low

highest <- sort.int(drift,index.return = T,decreasing=T)$ix
highest <- highest[c(1,c(length(highest)))]
diff <- drift[highest[1]] - drift[highest[2]]
lowest <- sort.int(drift,index.return = T,decreasing=F)$ix[1:3]

combined <- rbind(combined[-c(highest),],combined[c(highest),])

stan_data <- list(N=N,
                  T=max(time_points),
                  Y=c(combined),
                  use_ar=1,
                  num_legis=num_person,
                  num_bills=num_bills,
                  ll=person_points,
                  bb=bill_points,
                  diff=diff,
                  time=time_points,
                  num_fix_high=1,
                  num_fix_low=3,
                  pin_vals=array(1),
                  discrim_reg_sd=reg_discrim_sd,
                  discrim_abs_sd=absence_discrim_sd,
                  legis_sd=ideal_pts_sd,
                  diff_abs_sd=diff_sd,
                  diff_reg_sd=diff_sd,
                  restrict_sd=1,
                  restrict_low_bar=0,
                  restrict_high_bar=0,
                  restrict_alpha=0,
                  restrict_beta=0)

stan_code <- "
data {
int N;
int T;
int Y[N];
//int LX;
//int SRX;
//int SAX;
int use_ar;
int<lower=1> num_legis;
int<lower=1> num_bills;
int num_fix_high;
int num_fix_low;
real diff;
int ll[N];
int bb[N];
int time[N];
//matrix[num_legis,LX] legis_pred[T];
//matrix[num_bills,SRX] srx_pred;
//matrix[num_bills,SAX] sax_pred;
vector[num_fix_high] pin_vals;
real discrim_reg_sd;
real discrim_abs_sd;
real legis_sd;
real diff_abs_sd;
real diff_reg_sd;
real restrict_sd;
real restrict_low_bar;
real restrict_high_bar;
real restrict_alpha;
real restrict_beta;
}

transformed data {
int m;                         // # steps
int absence[N]; // need to create absence indicator

//binary models
m = 1;


for(n in 1:N) {
if(Y[n]>m || Y[n]==(-9998)) {
absence[n]=1;
} else {
absence[n]=0;
}

}
}
parameters {

vector[num_legis-2] L_free; // first T=1 params to constrain
vector[num_legis] L_tp1[T]; // all other params can float
vector[num_legis] L_AR1; // AR-1 parameters for AR-1 model

vector[num_bills] sigma_reg_free;
vector[num_bills] B_int_free;
vector[num_bills] A_int_free;
vector[num_bills] sigma_abs_free;

vector<lower=0>[1] restrict_high;
vector[num_fix_high] pinned_pars;
/*
vector[LX] legis_x;
vector[SRX] sigma_reg_x;
vector[SAX] sigma_abs_x;
vector[LX] legis_x_cons;
vector[SRX] sigma_reg_x_cons;
vector[SAX] sigma_abs_x_cons;

ordered[m-1] steps_votes;
ordered[m-1] steps_votes_grm[num_bills];
ordered[num_fix_low+num_fix_high] restrict_ord[T];
real exog_param;
*/

}

transformed parameters {

vector[num_legis] L_full;
vector[num_bills] sigma_abs_full;
vector[num_bills] sigma_reg_full;
vector[1] restrict_low;
vector[num_bills] B_int_full;
vector[num_bills] A_int_full;

B_int_full = B_int_free;
A_int_full = A_int_free;
sigma_abs_full = sigma_abs_free;
sigma_reg_full = sigma_reg_free;
restrict_low = restrict_high - diff; // set the width of the ideal points
L_full = append_row(L_free,append_row(restrict_high,restrict_low));
}

model {	
//vectors to hold model calculations
vector[N] pi1;
vector[N] pi2;

/*
legis_x ~ normal(0,5);
legis_x_cons ~ normal(0,5);
sigma_abs_x ~ normal(0,5);
sigma_reg_x ~ normal(0,5);
sigma_abs_x_cons ~ normal(0,5);
sigma_reg_x_cons ~ normal(0,5);
*/

L_AR1 ~ normal(0,1); // these parameters shouldn't get too big

B_int_free ~ normal(0,3);
A_int_free ~ normal(0,3);
L_free ~normal(0, legis_sd);
L_tp1[1] ~ normal(0, legis_sd);

restrict_high ~ exponential(.1);

sigma_reg_free ~ normal(0,3);
sigma_abs_free ~ normal(0,3);

//ar 1 prior

for(t in 2:T) {

    L_tp1[t] ~normal(L_full  +   L_AR1 .* L_tp1[t - 1],
    legis_sd/4);

}

//model 
for(n in 1:N) {


    pi1[n] = sigma_reg_full[bb[n]] *  L_tp1[time[n],ll[n]] - B_int_full[bb[n]];
    pi2[n] = sigma_abs_full[bb[n]] * L_tp1[time[n],ll[n]] - A_int_full[bb[n]]; 


  if(absence[n]==1) {
  	   1 ~ bernoulli_logit(pi2[n]);
  } else {
      0 ~ bernoulli_logit(pi2[n]);
      Y[n] ~ bernoulli_logit(pi1[n]);
  }
}

}
"

# compile model

to_stan <- stan_model(model_code=stan_code,model_name = 'Test AR1')

# run model

fit_model <- sampling(to_stan,data=stan_data,chains=4,cores=4,iter=2000,control=list(max_treedepth=15))
stan_rhat(fit_model)
# compare parameters

mcmc_recover_hist(as.array(fit_model,'restrict_high'),drift[highest[1]])
mcmc_recover_hist(as.array(fit_model,'restrict_low'),drift[highest[2]])

# figure out scaling loss
rhigh <- as.matrix(fit_model,'restrict_high')
rlow <- as.matrix(fit_model,'restrict_low')

scale_loss <- rhigh - drift[highest[1]]

ar1 <- as.array(fit_model,pars='L_AR1')

mcmc_recover_intervals(ar1,true=ar_adj)

ints <- as.array(fit_model,pars='L_full')

mcmc_recover_intervals(ints,true=drift)

persons <- as.array(fit_model, 'L_tp1')

mcmc_recover_intervals(persons,true=c(ideal_pts))

# correlation
require(stringr)
ar1 <- as.matrix(fit_model,pars='L_AR1') %>% apply(2,mean)
cor(ar1,ar_adj)
ints <- as.matrix(fit_model,pars='L_full') %>% apply(2,mean)
cor(ints + mean(scale_loss),drift)
persons <- as.data.frame(fit_model, 'L_tp1') %>% 
  mutate(iter=1:n()) %>% 
  gather(key=param,value=estimate,-iter) %>% 
  mutate(timepoint=str_extract(param,'\\[[0-9]+'),
         timepoint=as.numeric(str_extract(timepoint,'[0-9]+')),
         legispoint=str_extract(param,'[0-9]+\\]'),
         legispoint=as.numeric(str_extract(legispoint,'[0-9]+'))) %>% 
  select(-param) %>% 
  group_by(timepoint,legispoint) %>% 
  summarize(mean_est=mean(estimate),
            low_est=quantile(estimate,.1),
            high_est=quantile(estimate,.9))

ideal_pt_long <- as_data_frame(ideal_pts) %>% 
  mutate(timepoint=1:n()) %>% 
  gather(key=param,value=true_estimate,-timepoint) %>% 
  mutate(legispoint=as.numeric(factor(param,levels=colnames(ideal_pts)))) %>% 
  select(-param)

persons <- left_join(persons,ideal_pt_long,by=c('legispoint','timepoint'))

# plot real versus fake 

persons %>% 
  filter(legispoint %in% c(1:8)) %>% 
  ggplot(aes(y=mean_est,x=timepoint)) +
  geom_line() +
  geom_ribbon(aes(ymin=low_est,ymax=high_est)) +
  geom_line(aes(y=true_estimate,x=timepoint),colour='red',size=1) +
  facet_wrap(~legispoint)

# plot against real persons 



# plot persons as over-lapping lines will help visualize things



# # test just the ar component
# 
# stan_ar1_code <- "
# data {
# int T;
# int num_legis;
# vector[num_legis] L_tp1[T];
# }
# parameters {
# 
# vector[num_legis] L_free; // first T=1 params to constrain
# vector[num_legis] L_AR1; // AR-1 parameters for AR-1 model
# 
# }
# 
# model {	
# 
# L_AR1 ~ normal(0,1); // these parameters shouldn't get too big
# L_free ~normal(0, 1);
# 
# //ar 1 prior
# L_tp1[1] ~ normal(0,1);
# for(t in 2:T) {
#   L_tp1[t] ~normal(L_free + L_AR1 .* L_tp1[t - 1],1);
# }
# 
# }
# 
# "
# 
# stan_ar1_data <- list(T=max(time_points),
#                       num_legis=num_person,
#                       L_tp1=ideal_pts)
# 
# stan_ar1_model <- stan_model(model_code=stan_ar1_code,model_name = 'ar1_model')
# 
# ar1_fit <- sampling(stan_ar1_model,data=stan_ar1_data,chains=4,iter=1000,cores=4)
# 
# ar1 <- as.array(ar1_fit,pars='L_AR1')
# 
# mcmc_recover_intervals(ar1,true=ar_adj)
# 
# ints <- as.array(ar1_fit,pars='L_free')
# 
# mcmc_recover_intervals(ints,true=drift)
# 
# ar1 <- as.matrix(ar1_fit,pars='L_AR1') %>% apply(2,mean)
# cor(ar1,ar_adj)
# ints <- as.matrix(ar1_fit,pars='L_free') %>% apply(2,mean)
# cor(ints,drift)

# # test coding
# 
# num_bills <- 4
# num_person <- 5
# time_points <- 2
# 
# person_points <- rep(1:num_person,times=num_bills)
# bill_points <- rep(1:num_bills,each=num_person)
# 
# time_points <- rep(1:time_points,each=num_bills/time_points)
# time_points <- time_points[bill_points]
# 
# stan_test_code <- "
# data {
# int N;
# int T;
# int num_legis;
# vector[num_legis] L_tp1[T];
# int Y[N];
# int ll[N];
# int bb[N];
# int time[N];
# }
# parameters {
# }
# 
# model {	
# 
# 
# for(n in 1:N) {
#   print(ll[n]);
#   print(bb[n]);
#   print(time[n]);
#   print(L_tp1[time[n],ll[n]]);
#   print(Y[n]);
# }
# 
# }
# 
# "
# 
# test_model <- stan_model(model_name='test',model_code=stan_test_code)
# stan_data$L_tp1 <- ideal_pts
# sampling(test_model,chains=1,iter=1,data=stan_data,algorith='Fixed_param')
# 
