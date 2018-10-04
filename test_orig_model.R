require(rstan)
require(dplyr)
require(ggplot2)
require(forcats)
# classic ARMA stan model:

num_person <- 20
num_bills <- 100

# gen ideal data

#relevel to consrain

person_points <- rep(1:num_person,times=num_bills)
bill_points <- rep(1:num_bills,each=num_person)

ideal_pts <- rnorm(num_person)

restrict_high <- sort.int(ideal_pts,index.return = T,decreasing = T)$ix[1]
restrict_low <- sort.int(ideal_pts,index.return = T,decreasing = F)$ix[1]




# generate time points

absence_discrim <- rnorm(num_bills)
absence_diff <- rnorm(num_bills)

pr_absence <- sapply(1:length(person_points),function(n) {
  ideal_pts[person_points[n]]*absence_discrim[bill_points[n]] - absence_diff[bill_points[n]]
}) %>% plogis()

outcome <- as.numeric(runif(length(person_points))<pr_absence)

person_points <- factor(person_points)
person_points <- fct_relevel(person_points,as.character(restrict_low),as.character(restrict_high),
                             after=num_person)

# now fit a model to the observed series

stan_code <- '
data {
int N;
int L;
int B;

int ll[N];
int bb[N];

int outcome[N];
real id_diff;
real id_diff_high;
} 
parameters {
vector[L-2] alpha_free;
vector[L] Y;
vector[1] high;
vector[B] discrim;
vector[B] diff;
}
transformed parameters {
vector[L] alpha;

vector[1] low;


low = high - id_diff;

alpha=append_row(alpha_free,append_row(low,high));


}
model {
diff ~ normal(0,1);
discrim ~ normal(0,1);
alpha_free ~ normal(0,3);
high ~ normal(id_diff_high,.01);


for(n in 1:N) {
  outcome[n] ~ bernoulli_logit(discrim[bb[n]] * (alpha[ll[n]]) - diff[bb[n]]);
}

}
'

to_stan <- stan_model(model_code = stan_code)

run_ar1 <- sampling(to_stan,data=list(N=length(outcome),
                                      L=num_person,
                                      B=num_bills,
                                      ll=as.numeric(person_points),
                                      bb=bill_points,
                                      outcome=outcome,
                                      id_diff=sort.int(ideal_pts,index.return = T,decreasing = T)$x[1] -
                      sort.int(ideal_pts,index.return = T,decreasing = F)$x[1],
                    id_diff_high=sort.int(ideal_pts,index.return = T,decreasing = T)$x[1]),
                    chains=4,cores=4)



print(run_ar1,'alpha')

all_res <- summary(run_ar1)

alpha <- all_res$summary[grepl(x=row.names(all_res$summary),
                               pattern='alpha_free\\['),
                         'mean']

cor(alpha,ideal_pts[-c(restrict_high,restrict_low)])

discrim_est <- all_res$summary[grepl(x=row.names(all_res$summary),
                               pattern='discrim\\['),
                         'mean']
cor(discrim_est,absence_discrim)
