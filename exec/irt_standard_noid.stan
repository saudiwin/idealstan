
#include "license.stan"

data {
  int N;
  int T;
  int Y[N];
  
  /* Use this to set the type of IRT Model to run
  1= basic IRT 2 Pl (no inflation)
  2= basic IRT 2 PL (with inflation)
  3 = ratingscale IRT (no inflation)
  4= ratingscale IRT (with inflation)
  5 = grm IRT (no inflation)
  6= grm IRT (with inflation)
  7= ZIP IRT
  */
  int model_type;
  int LX;
  int SRX;
  int SAX;
  int use_ar;
  // int ar_lag;
  // int ma_lag;
  // int i_lag;
  int<lower=1> num_legis;
  int<lower=1> num_bills;
  int ll[N];
  int bb[N];
  int time[N];
  matrix[num_legis,LX] legis_pred[T];
  matrix[num_bills,SRX] srx_pred;
  matrix[num_bills,SAX] sax_pred;
  //vector[N] exog_data;
  real discrim_reg_sd;
  real discrim_abs_sd;
  real legis_sd;
  real diff_abs_sd;
  real diff_reg_sd;
  real restrict_sd;
  real time_sd;
}

transformed data {
	int m;                         // # steps
	int absence[N]; // need to create absence indicator
	int num_constrain_l;
	int num_constrain_sa;
	int num_constrain_sr;
	int Y_new[N];
	if(model_type==4||model_type==6) {
	  //count down one if model is inflated
	  m = max(Y) - 1;
	} else if(model_type==1||model_type==2) {
	  //binary models
	  m = 2;
	} else {
	  m= max(Y);
	}
	
  for(n in 1:N) {
      if(Y[n]>m || Y[n]==(-9998)) {
        absence[n]=1;
      } else {
        absence[n]=0;
      }
      if(model_type==1) {
        //need to change outcome for binomial models
        if(max(Y)==2) {
          Y_new[n] = Y[n] - 1;
        } else {
          Y_new[n] = Y[n];
        }
      } else if(model_type==2) {
         if(max(Y)==3) {
          Y_new[n] = Y[n] - 1;
        } else {
          Y_new[n] = Y[n];
        }
      }
  }
  


}

parameters {
  vector[num_bills] sigma_abs_free;
  vector[num_legis] L_free;
  vector[num_legis] L_tp1[T]; // all other params can float
  vector[num_legis] L_AR1; // AR-1 parameters for AR-1 model
  vector[num_bills] sigma_reg_free;
  vector[LX] legis_x;
  vector[SRX] sigma_reg_x;
  vector[SAX] sigma_abs_x;
  vector[LX] legis_x_cons;
  vector[SRX] sigma_reg_x_cons;
  vector[SAX] sigma_abs_x_cons;
  ordered[m-1] steps_votes;
  ordered[m-1] steps_votes_grm[num_bills];
  vector[num_bills] B_int_free;
  vector[num_bills] A_int_free;
  //real<lower=0> time_sd;
}

transformed parameters {
  
  vector[num_legis] L_full;
  vector[num_bills] sigma_abs_full;
  vector[num_bills] sigma_reg_full;
  vector[num_bills] B_int_full;
  vector[num_bills] A_int_full;
  
  L_full=L_free;
  sigma_abs_full=sigma_abs_free;
  sigma_reg_full=sigma_reg_free;
  
  B_int_full = B_int_free;
  //B_int_full[1] = 0.0;
  A_int_full=A_int_free;
  //B_int_full=B_int_free;
  
}

model {	
  //vectors to hold model calculations
  vector[N] pi1;
  vector[N] pi2;
  
  
  if(T==1) {
    L_free ~normal(legis_pred[1, 1:(num_legis - num_constrain_l), ] * legis_x, legis_sd);
  } else {
    L_free ~ normal(0,legis_sd);
  }
  L_tp1[1] ~ normal(legis_pred[1, 1:(num_legis), ] * legis_x,legis_sd);
  if(T>1) {
    if(use_ar==1) {
       #include l_hier_ar1_prior.stan
    } else {
      #include l_hier_prior.stan
    }
  }

  
  sigma_abs_free ~ normal(0,discrim_abs_sd);
  sigma_reg_free ~ normal(0,discrim_reg_sd);
  legis_x ~ normal(0,5);
  sigma_reg_x ~ normal(srx_pred[num_bills, ] * sigma_reg_x,5);
  sigma_abs_x ~ normal(sax_pred[num_bills, ] * sigma_abs_x,5);
  legis_x_cons ~ normal(0,5);;
  sigma_reg_x_cons ~ normal(0,5);
  sigma_abs_x_cons ~ normal(0,5);
  L_AR1 ~ normal(0,1); // these parameters shouldn't get too big
  //time_sd ~ lognormal(1.7,.3);
  if(model_type>2 && model_type<8) {
    for(i in 1:(m-2)) {
    steps_votes[i+1] - steps_votes[i] ~ normal(0,5); 
    }
  } else {
    steps_votes ~ normal(0,5);
  }
	
  B_int_free ~ normal(0,diff_reg_sd);
  A_int_free ~ normal(0,diff_abs_sd);
  //exog_param ~ normal(0,5);
  for(b in 1:num_bills) {
  steps_votes_grm[b] ~ normal(0,5);
  }
  //model

#include "model_types.stan"


  
}

