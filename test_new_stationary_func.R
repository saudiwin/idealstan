require(rstan)
require(dplyr)
require(ggplot2)
require(forcats)
# classic ARMA stan model:

num_person <- 25
num_bills <- 200
alpha_int <- rnorm(num_person)
sigma <- 0.1
adj_in <- runif(num_person,-.8,.8)

t <- 100


.gen_ts_data <- function(t,adj_in,alpha_int,sigma,init_sides) {
  current_val <- new.env()
  current_val$t1 <- 0
  
  out_vec <- lapply(1:t,function(t_1) {
    
    if(t_1==1) {
      t_11 <- alpha_int
      current_val$t1 <- t_11
      return(data_frame(t_11))
    } else {
      t_11 <- alpha_int + adj_in*current_val$t1 + rnorm(n=1,sd=sigma)
    }
    current_val$t1 <- t_11
    return(data_frame(t_11))
  })  %>% bind_rows
  return(out_vec)
}

# more realistic

Y <- lapply(1:num_person, function(i) {
  this_person <- .gen_ts_data(t=t,
                              adj_in=adj_in[i],
                              alpha_int=alpha_int[i],
                              sigma=sigma,
                              init_sides=0)
  return(this_person)
}) %>% bind_cols %>% as.matrix
Y <- t(Y)
# gen ideal data

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

pr_absence <- sapply(1:length(person_points),function(n) {
  Y[person_points[n],time_points[n]]*absence_discrim[bill_points[n]] - absence_diff[bill_points[n]]
}) %>% plogis()

outcome <- as.numeric(runif(length(person_points))<pr_absence)

person_points <- factor(person_points)
person_points <- fct_relevel(person_points,as.character(restrict_high),
                             after=num_person)

# reg_diff <- prior_func(params=list(N=num_bills,mean=0,sd=diff_sd))
# reg_discrim <- prior_func(params=list(N=num_bills,mean=0,sd=reg_discrim_sd))
# 
# # this is the same for all DGPs
# 
# pr_vote <- sapply(1:length(person_points),function(n) {
#   ideal_pts[person_points[n],time_points[n]]*reg_discrim[bill_points[n]] - reg_diff[bill_points[n]]
# }) %>% plogis()


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
parameters {
vector[L-1] alpha_free;
vector<lower=-0.8,upper=0.8>[L-1] adj_in_free;
vector[L] Y_start;
vector[T-1] Y_var;
vector[B] discrim;
vector[B] diff;
real<lower=0> sigma;
vector[1] high;
vector<lower=-0.8,upper=0.8>[1] adj_high;
}
transformed parameters {
vector[L] alpha;
vector[L] adj_in;
vector[1] low;
vector[L] Y[T];

low = high - id_diff;

alpha=append_row(alpha_free,high);
adj_in=append_row(adj_in_free,adj_high);

for(t in 1:T) {
  if(t==1) {
    Y[1] = Y_start;
  } else {
    Y[t] = alpha + adj_in .* Y[t-1] + sigma*Y_var[t-1];
  }
}

}
model {
diff ~ normal(0,3);
discrim ~ normal(0,3);
alpha_free ~ normal(0,1);
adj_in_free ~ normal(0,2);
adj_high ~ normal(0,1);
sigma ~ exponential(1/.1);
high ~ normal(id_diff_high,.01);
Y_var ~ normal(0,1);
Y_start ~ normal(0,1);

for(n in 1:N) {
  outcome[n] ~ bernoulli_logit(discrim[bb[n]] * (Y[tt[n],ll[n]]) - diff[bb[n]]);
}

}
'

to_stan <- stan_model(model_code = stan_code)

run_ar1 <- sampling(to_stan,data=list(N=length(outcome),
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
                    chains=4,cores=4)



print(run_ar1)

all_res <- summary(run_ar1)

alpha <- all_res$summary[grepl(x=row.names(all_res$summary),
                               pattern='alpha\\['),
                         'mean']

alpha <- alpha[c(1:(restrict_high-1),num_person,(restrict_high):(num_person-1))]
cor(alpha,alpha_int)
adj_in_est <- all_res$summary[grepl(x=row.names(all_res$summary),
                               pattern='adj_in\\['),
                         'mean']
adj_in_est <- adj_in_est[c(1:(restrict_high-1),num_person,(restrict_high):(num_person-1))]
cor(adj_in_est,adj_in)
Y_est <- all_res$summary[grepl(x=row.names(all_res$summary),
                               pattern='Y\\['),
                         'mean']

cor(Y_est,c(Y[c(1:(restrict_high-1),num_person,(restrict_high):(num_person-1)),]))

# now try a random walk model

alpha_int <- -1.25
sigma <- 0.1
adj_in <- 1

t <- 20


Y <-  .gen_ts_data(t=t,
                   adj_in=adj_in,
                   alpha_int=alpha_int,
                   sigma=sigma,
                   init_sides=0)


to_stan <- stan_model(model_code = stan_code)

run_rw <- sampling(to_stan,data=list(N=length(Y$t_11),
                                      Y=Y$t_11))
