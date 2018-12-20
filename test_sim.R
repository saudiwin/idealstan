# simulate GP ideal and fit it

require(rstan)
require(dplyr)
require(ggplot2)
require(forcats)
# classic ARMA stan model:

num_person <- 10


num_bills <- 200
alpha_int <- rnorm(num_person)
sigma <- 0.1
adj_in <- runif(num_person,-.8,.8)

t <- 10

# simulate the GP

alpha_true <- 0.5
rho_true <- runif(n=num_person,2.5,5.5)
sigma_true <- 0.1

N_total = num_person
x_total <- 1:t

simu_data <- list(alpha=alpha_true, rho=rho_true, sigma=sigma_true,
                  N=N_total, x=x_total, T=t)

sim_gauss <- "data {
int<lower=1> N;
int<lower=1> T;
real x[T];

real<lower=0> rho[N];
real<lower=0> alpha;
real<lower=0> sigma;
}

transformed data {
//create one covariance matrix for each legislator
matrix[T, T] cov[N];
matrix[T, T] L_cov[N];
for(n in 1:N) {
cov[n] =   cov_exp_quad(x, alpha, rho[n])
+ diag_matrix(rep_vector(1e-10, T));
L_cov[n] = cholesky_decompose(cov[n]);
}


}

parameters {}
model {}

generated quantities {
matrix[N,T] f;
matrix[N,T] Y;
  for(n in 1:N) {
    f[n,] = multi_normal_cholesky_rng(rep_vector(0, T), L_cov[n])';
    for(t in 1:T) {
      Y[n,t] = normal_rng(f[n,t],sigma);
    }
    
  }  
}"

simu_fit <- stan(model_code = sim_gauss, data=simu_data, iter=1,
                 chains=1, seed=494838, algorithm="Fixed_param")

# more realistic

Y <- rstan::extract(simu_fit)$Y[1,,]

require(tidyr)
require(stringr)

y_plot <- as_data_frame(Y) %>% mutate(person=1:n(),
                                      alpha_true=alpha_true,
                                      rho_true=rho_true) %>% 
  gather(key=time,value=estimate,-person,-alpha_true,-rho_true) %>% 
  mutate(time=as.numeric(str_extract(time,'[0-9]+'))) %>% 
  group_by(person) %>% 
  mutate(overall_mean=max(estimate),
         time_pt=time[which(estimate==max(estimate))])

y_plot %>% 
  ggplot(aes(y=estimate,x=time)) +
  geom_line(aes(colour=alpha_true,group=person,size=alpha_true),alpha=0.5) +
  theme(panel.grid = element_blank(),
        panel.background = element_blank()) +
  geom_text(aes(y=overall_mean,x=time_pt,label=as.character(overall_mean)),colour='red')

y_plot %>% 
  ggplot(aes(y=estimate,x=time)) +
  geom_line(aes(colour=rho_true,group=person,size=rho_true),alpha=0.5) +
  theme(panel.grid = element_blank(),
        panel.background = element_blank())

# see how these variables correlate with mean and SD

y_plot %>% 
  group_by(person) %>% 
  summarize(mean_p=mean(estimate),
            sd_p=sd(estimate),
            rho_true=rho_true[1],
            alpha_true=alpha_true[1]) %>% 
  ggplot(aes(y=alpha_true,x=rho_true)) +
    geom_point(aes(colour=mean_p,size=sd_p),alpha=0.5) +
  theme(panel.grid = element_blank(),
        panel.background = element_blank())

# now need to estimate the original data

# plot data

#relevel to consrain

person_points <- rep(1:num_person,times=num_bills)
bill_points <- rep(1:num_bills,each=num_person)

restrict_high <- sort.int(alpha_int,index.return = T,decreasing = T)$ix[1]
restrict_low <- sort.int(alpha_int,index.return = T,decreasing = F)$ix[1]

# generate time points

time_points <- rep(1:t,each=num_bills/t)
time_points <- time_points[bill_points]

absence_discrim <- rnorm(num_bills)
absence_diff <- rnorm(num_bills)

reg_discrim <- rnorm(num_bills)
reg_diff <- rnorm(num_bills)

pr_absence <- sapply(1:length(person_points),function(n) {
  Y[person_points[n],time_points[n]]*absence_discrim[bill_points[n]] - absence_diff[bill_points[n]]
}) %>% plogis()

pr_vote <- sapply(1:length(person_points),function(n) {
  Y[person_points[n],time_points[n]]*reg_discrim[bill_points[n]] - reg_diff[bill_points[n]]
}) %>% plogis()

absent <- as.numeric(runif(length(person_points))<pr_absence)
present <- as.numeric(runif(length(person_points))<pr_vote)

#outcome <- ifelse(absent==0,present,3)
outcome <- as.numeric(runif(length(person_points))<pr_vote)

person_points <- factor(person_points)
person_points <- fct_relevel(person_points,as.character(restrict_high),
                             after=num_person)

# need to adjust for full gaussian process inference.
# only non-missing for now


# now fit a model to the observed series

stan_code <- "
functions {
  real jacob_mean(int N, real N_real) {
    vector[N] col_vec;
real num_div;
real density;

num_div = 1/N_real;

col_vec = rep_vector(num_div,N);

density = sum(log(col_vec));

return density;

}
}
data {
int N;
int L;
int B;
int T;
int ll[N];
int bb[N];
int tt[N];
int outcome[N];
real id_diff;
real id_diff_high;
//vector[L] rho;
//real m_sd;
//real sigma;
int restrict_mean_ind;
real restrict_mean_val;
} 
transformed data {
// need a time counter as input for the Gaussian process.
  real x[T];
real L_real;
L_real = L;
  for (t in 1:T) {
    x[t] = t;
  }
}
parameters {
real<lower=0> alpha;
matrix[T,L] Y;
vector<lower=0>[L] rho;
//vector<lower=0>[L] m_sd;
vector[B] discrim;
vector[B] diff;
real<lower=0> sigma;
//vector[1] high;
}
transformed parameters {
}
model {
matrix[T, T] cov[L];
matrix[T, T] L_cov[L];

  for(n in 1:L) {
    cov[n] =   cov_exp_quad(x, alpha, rho[n])
      + diag_matrix(rep_vector(square(sigma),L));
    L_cov[n] = cholesky_decompose(cov[n]);
  }

for(n in 1:L) {
  Y[,n] ~ multi_normal_cholesky(rep_vector(0, T), L_cov[n]);
}
diff ~ normal(0,3);
discrim ~ normal(0,3);
rho ~ inv_gamma(8.91924, 34.5805);
sigma ~ exponential(5);
//m_sd ~ normal(0, 2);
alpha ~ normal(0,1);

// constrain the over-time mean of one L
max(Y[,restrict_mean_ind]) ~ normal(restrict_mean_val,.01);
  //target += jacob_mean(L,L_real); // this is a constant as it only varies with the count of the parameters

for(n in 1:N) {
outcome[n] ~ bernoulli_logit(discrim[bb[n]] * (Y[tt[n],ll[n]]) - diff[bb[n]]);
}

}
"

to_stan <- stan_model(model_code = stan_code)

# find the item to constrain

all_means <- apply(Y,1,max)
restrict_mean_ind <- which(all_means==min(all_means))

run_ar1 <- vb(to_stan,data=list(N=length(outcome),

                                      L=nrow(Y),
                                      B=num_bills,
                                      restrict_mean_ind=restrict_mean_ind,
                                      restrict_mean_val=all_means[restrict_mean_ind],
                                      rho=rho_true,
                                      m_sd=alpha_true,
                                      sigma=sigma,
                                      T=t,
                                      ll=as.numeric(person_points),
                                      bb=bill_points,
                                      tt=time_points,
                                      outcome=outcome,
                                      id_diff=sort.int(alpha_int,index.return = T,decreasing = T)$x[1] -
                                        sort.int(alpha_int,index.return = T,decreasing = F)$x[1],
                                      id_diff_high=sort.int(alpha_int,index.return = T,decreasing = T)$x[1]))
              #iter=1000,chains=3,cores=3)



#print(run_ar1,pars='rho')

# get estimates for y 

y_est <- summary(run_ar1,pars='Y')$summary[,'mean']
y_est <- matrix(y_est,nrow=num_person,ncol=t)

cor(c(y_est),c(Y))

colnames(Y) <- paste0('time_',1:t)
colnames(y_est) <- paste0('time_',1:t)

y_long <- as_data_frame(Y) %>% 
  mutate(person=1:n()) %>% 
  gather(time,estimate,-person) %>% 
  mutate(time=as.numeric(str_extract(time,'[0-9]+')),
         type='true')

y_long_est <- as_data_frame(y_est) %>% 
  mutate(person=1:n()) %>% 
  gather(time,estimate,-person) %>% 
  mutate(time=as.numeric(str_extract(time,'[0-9]+')),
         type='estimated')

y_est_low <- summary(run_ar1,pars='Y')$summary[,'2.5%']
y_est_low <- matrix(y_est_low,nrow=num_person,ncol=t)
y_long_est_low <- as_data_frame(y_est_low) %>% 
  mutate(person=1:n()) %>% 
  gather(time,estimate_low,-person) %>% 
  mutate(time=as.numeric(str_extract(time,'[0-9]+')),
         type='estimated')

y_est_high <- summary(run_ar1,pars='Y')$summary[,'97.5%']
y_est_high <- matrix(y_est_high,nrow=num_person,ncol=t)
y_long_est_high <- as_data_frame(y_est_high) %>% 
  mutate(person=1:n()) %>% 
  gather(time,estimate_high,-person) %>% 
  mutate(time=as.numeric(str_extract(time,'[0-9]+')),
         type='estimated')

y_long_est <- left_join(y_long_est,
                        y_long_est_low,by=c('time','person','type')) %>% 
                left_join(y_long_est_high,by=c('time','person','type'))

combine_data <- bind_rows(y_long_est,y_long)

combine_data %>% 
  ggplot(aes(x=time)) +
  geom_ribbon(aes(ymax=estimate_high,
                  ymin=estimate_low,fill=type),alpha=0.5) +
  geom_line(aes(linetype=type,y=estimate),alpha=0.5) +
  facet_wrap(~person) +
  theme(panel.grid = element_blank(),
        panel.background = element_blank())
