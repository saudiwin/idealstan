# simulate GP ideal and fit it

require(rstan)
require(dplyr)
require(ggplot2)
require(forcats)
# classic ARMA stan model:

num_person <- 25
num_bills <- 100
alpha_int <- rnorm(num_person)
sigma <- 0.1
adj_in <- runif(num_person,-.8,.8)

t <- 20

# simulate the GP

alpha_true <- runif(n = num_person,2,3)
rho_true <- runif(n=num_person,2.5,5.5)
sigma_true <- runif(n=num_person,0.5,3)

N_total = num_person
x_total <- 1:t

simu_data <- list(alpha=alpha_true, rho=rho_true, sigma=sigma_true,
                  N=N_total, x=x_total, T=t)

sim_gauss <- "data {
int<lower=1> N;
int<lower=1> T;
real x[T];

real<lower=0> rho[N];
real<lower=0> alpha[N];
real<lower=0> sigma[N];
}

transformed data {
//create one covariance matrix for each legislator
matrix[T, T] cov[N];
matrix[T, T] L_cov[N];
for(n in 1:N) {
cov[n] =   cov_exp_quad(x, alpha[n], rho[n])
+ diag_matrix(rep_vector(1e-10, T));
L_cov[n] = cholesky_decompose(cov[n]);
}


}

parameters {}
model {}

generated quantities {
matrix[N,T] f;
for(n in 1:N) {
f[n,] = multi_normal_cholesky_rng(rep_vector(0, T), L_cov[n])';
}  
}"

simu_fit <- stan(model_code = sim_gauss, data=simu_data, iter=1,
                 chains=1, seed=494838, algorithm="Fixed_param")

# more realistic

Y <- rstan::extract(simu_fit)$f[1,,]

require(tidyr)
require(stringr)

y_plot <- as_data_frame(Y) %>% mutate(person=1:n(),
                                      alpha_true=alpha_true,
                                      rho_true=rho_true) %>% 
  gather(key=time,value=estimate,-person,-alpha_true,-rho_true) %>% 
  mutate(time=as.numeric(str_extract(time,'[0-9]+')))

y_plot %>% 
  ggplot(aes(y=estimate,x=time)) +
  geom_line(aes(colour=alpha_true,group=person,size=alpha_true),alpha=0.5) +
  theme(panel.grid = element_blank(),
        panel.background = element_blank())

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

stan_code <- '
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
} 
transformed data {
// need a time counter as input for the Gaussian process.
  real x[T]; 
  for (t in 1:T) {
    x[t] = t;
  }
}
parameters {
vector[L] alpha;
matrix[T,L] Y;
vector<lower=0>[L] rho;
vector<lower=0>[L] m_sd;
vector[B] discrim;
vector[B] diff;
real<lower=0> sigma;
vector[1] high;
}
transformed parameters {

  matrix[T, T] cov[L];
  matrix[T, T] L_cov[L];

  for(n in 1:L) {
    cov[n] =   cov_exp_quad(x, m_sd[n], rho[n])
      + diag_matrix(rep_vector(1e-10, T));
    L_cov[n] = cholesky_decompose(cov[n]);
  }
}
model {

for(n in 1:L) {
  Y[,n] ~ multi_normal_cholesky(rep_vector(0, T), L_cov[n]);
}
diff ~ normal(0,3);
discrim ~ normal(0,3);
rho ~ inv_gamma(8.91924, 34.5805);
m_sd ~ normal(0, 2);
alpha ~ normal(0,1);
for(n in 1:N) {
outcome[n] ~ bernoulli_logit(discrim[bb[n]] * (Y[tt[n],ll[n]]) - diff[bb[n]]);
}

}
'

to_stan <- stan_model(model_code = stan_code)

run_ar1 <- vb(to_stan,data=list(N=length(outcome),
                                      L=nrow(Y),
                                      B=num_bills,
                                      T=t,
                                      ll=as.numeric(person_points),
                                      bb=bill_points,
                                      tt=time_points,
                                      outcome=outcome,
                                      id_diff=sort.int(alpha_int,index.return = T,decreasing = T)$x[1] -
                                        sort.int(alpha_int,index.return = T,decreasing = F)$x[1],
                                      id_diff_high=sort.int(alpha_int,index.return = T,decreasing = T)$x[1]),
              tol_rel_obj=1e-4)



print(run_ar1,pars='m_sd')

# get estimates for y 

y_est <- summary(run_ar1,pars='Y')$summary[,'mean']
y_est <- matrix(y_est,nrow=25)

cor(c(y_est),c(Y))

plot_data <- data_frame(y_est=c(y_est),
                        y=c(Y),
                        time=rep(1:t,times=num_person),
                        person=rep(1:num_person,each=t)) %>% 
  gather(est_type,estimate,-time,-person)

plot_data %>% 
  ggplot(aes(y=estimate,x=time)) +
  geom_line(aes(colour=est_type,group=person),alpha=0.5) +
  facet_wrap(~est_type) +
  theme(panel.grid = element_blank(),
        panel.background = element_blank())
