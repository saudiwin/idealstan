/* This file sets up the various types of IRT models that can be run in idealstan */

if(model_type==1) {
  //2 PL no inflation

      for(n in 1:N) {
        pi1[n] = sigma_reg_full[bb[n]] *  L_full[time[n],ll[n]] - B_yes[bb[n]];
      }
      Y_new ~ bernoulli_logit(pi1);

} else if(model_type==2) {
  //2 PL inflation

      for(n in 1:N) {
        pi1[n] = sigma_reg_full[bb[n]] *  L_full[time[n],ll[n]] - B_yes[bb[n]];
        pi2[n] = sigma_abs_full[bb[n]] * L_full[time[n],ll[n]] - 
                  B_abs[bb[n]] + avg_particip * particip[ll[n]];
        if(absence[n]==1) {
  	      1 ~ bernoulli_logit(pi2[n]);
        } else {
          0 ~ bernoulli_logit(pi2[n]);
          Y_new[n] ~ bernoulli_logit(pi1[n]);
          }
        }

} else if(model_type==3) {
  //ratingscale no inflation

    for(n in 1:N) {
      pi1[n] = sigma_reg_full[bb[n]] *  L_full[time[n],ll[n]] - B_yes[bb[n]];
        Y[n] ~ ordered_logistic(pi1[n],steps_votes);
      }

} else if(model_type==4) {
  //ratingscale inflation

    for(n in 1:N) {
      pi1[n] = sigma_reg_full[bb[n]] *  L_full[time[n],ll[n]] - B_yes[bb[n]];
      pi2[n] = sigma_abs_full[bb[n]] * L_full[time[n],ll[n]] - B_abs[bb[n]] + 
      avg_particip * particip[ll[n]];
      if(absence[n]==1) {
	      1 ~ bernoulli_logit(pi2[n]);
      } else {
        0 ~ bernoulli_logit(pi2[n]);
        Y[n] ~ ordered_logistic(pi1[n],steps_votes);
      }

  }
} else if(model_type==5) {
  //grm no inflation

    for(n in 1:N) {
      pi1[n] = sigma_reg_full[bb[n]] *  L_full[time[n],ll[n]] - B_yes[bb[n]];
        Y[n] ~ ordered_logistic(pi1[n],steps_votes_grm[bb[n]]);
      }

} else if(model_type==6) {
  //grm inflation

    for(n in 1:N) {
      pi1[n] = sigma_reg_full[bb[n]] *  L_full[time[n],ll[n]] - B_yes[bb[n]];
      pi2[n] = sigma_abs_full[bb[n]] * L_full[time[n],ll[n]] - B_abs[bb[n]] + 
      avg_particip * particip[ll[n]];
      if(absence[n]==1) {
	      1 ~ bernoulli_logit(pi2[n]);
      } else {
        0 ~ bernoulli_logit(pi2[n]);
        Y[n] ~ ordered_logistic(pi1[n],steps_votes_grm[bb[n]]);
      }
    }

} else if(model_type==7) {
  //hurdle poisson

    for(n in 1:N) {
      pi1[n] = sigma_reg_full[bb[n]] *  L_full[time[n],ll[n]] - B_yes[bb[n]];
      pi2[n] = sigma_abs_full[bb[n]] * L_full[time[n],ll[n]] - B_abs[bb[n]] + 
      avg_particip * particip[ll[n]];
      if(absence[n]==1) {
	      1 ~ bernoulli_logit(pi2[n]);
      } else {
        0 ~ bernoulli_logit(pi2[n]);
        Y[n] ~ poisson(pi1[n]);
      }
    }

}
