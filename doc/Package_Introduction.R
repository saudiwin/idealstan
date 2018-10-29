## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,warning=FALSE,fig.align = 'center',fig.width=6, fig.height=5)
require(idealstan)
require(dplyr)
require(ggplot2)

## ----sim_data------------------------------------------------------------

ord_ideal_sim <- id_sim_gen(model='ordinal_grm')
knitr::kable(as_data_frame(head(ord_ideal_sim@score_matrix)))

## ----constrain_sim-------------------------------------------------------
true_legis <- ord_ideal_sim@simul_data$true_person
high_leg <- sort(true_legis,decreasing = TRUE,index.return=TRUE)
low_leg <- sort(true_legis,index.return=TRUE)

ord_ideal_est <- id_estimate(idealdata=ord_ideal_sim,
                             model_type=6,
                             fixtype='constrained',
                             id_diff=high_leg$x[1]-low_leg$x[1],
                             id_diff_high=high_leg$x[1],
                             restrict_ind_high = as.character(high_leg$ix[1]),
                             restrict_ind_low=as.character(low_leg$ix[1]),
                             refresh=500,
                             ncores=2,
                             person_sd=1,
                           nchains=2)

## ----check_true----------------------------------------------------------
id_plot_legis(ord_ideal_est,show_true = TRUE)

## ----restrict_auto-------------------------------------------------------
ord_ideal_est <- id_estimate(idealdata=ord_ideal_sim,
                             model_type=6,
                             refresh=500,
                             ncores=2,
                           nchains=2,
                           person_sd=1)

## ----rhats---------------------------------------------------------------
id_plot_rhats(ord_ideal_est)

## ----check_data----------------------------------------------------------

data('senate114')

knitr::kable(select(head(senate114),1:8))

table(senate114$cast_code)


## ----long_to_ideal-------------------------------------------------------
senate_data <- id_make(senate114,outcome = 'cast_code',
                       person_id = 'bioname',
                       item_id = 'rollnumber',
                       group_id= 'party_code',
                       time_id='date',
                       miss_val='Absent')


## ----run_114_model,fig.height=7------------------------------------------
sen_est <- id_estimate(senate_data,
                model_type = 1,
                 use_vb = T,
                fixtype='vb_partial',
                 restrict_ind_high = "WARREN, Elizabeth",
                 restrict_ind_low="BARRASSO, John A.",
            seed=84520)
id_plot_legis(sen_est,person_ci_alpha=0.2) +
  scale_color_manual(values=c(D='blue',R='red',I='green')) +
  ggtitle('Ideal Points of the 114th Senate')

## ----item_plot,fig.height=8----------------------------------------------
id_plot_legis(sen_est,person_ci_alpha=0.1,item_plot='94') +
  scale_color_manual(values=c(D='blue',R='red',I='green')) +
  ggtitle('Ideal Points of the 114th Senate with Vote 94 Midpoint')

## ----abs_item_plot,fig.height=8------------------------------------------

id_plot(sen_est,person_ci_alpha=0.1,item_plot='225',
        item_plot_type='inflated') + 
  scale_color_manual(values=c(D='blue',R='red',I='green'))

## ----ideal_pts_sum-------------------------------------------------------
ideal_pts_sum <- summary(sen_est,pars='ideal_pts')
knitr::kable(head(ideal_pts_sum))

## ----stan_obj------------------------------------------------------------
stan_obj <- sen_est@stan_samples
# show the 
print(stan_obj,pars = c("L_full[1]",
                        'L_full[2]',
                        'L_full[3]'))

## ----stan_trace----------------------------------------------------------
stan_trace(sen_est,par='L_full[1]')

## ----ideal_pts_ind-------------------------------------------------------
item_all <- summary(sen_est,pars='items', aggregate=F)
knitr::kable(head(item_all))

## ----create_cov_data-----------------------------------------------------

senate114$age <- 2018 - senate114$born

senate_data <- id_make(senate114,outcome = 'cast_code',
                       person_id = 'bioname',
                       item_id = 'rollnumber',
                       group_id= 'party_code',
                       time_id='date',
                       person_cov = ~party_code*age,
                       miss_val='Absent')

sen_est_cov <- id_estimate(senate_data,
                model_type = 1,
                 use_vb = T,
                fixtype='vb_full',
                 restrict_ind_high = "BARRASSO, John A.",
                 restrict_ind_low="WARREN, Elizabeth",
            seed=84520)



## ----cov_plot------------------------------------------------------------
id_plot_cov(sen_est_cov,cov_type='person_cov')

## ----cov_plot_small------------------------------------------------------
id_plot_cov(sen_est_cov,cov_type='person_cov') + xlim(c(-0.25,0.25)) +
  ggtitle('Effect of Age by Party on Ideal Point Scores in 114th Senate')

## ----cov_plot_filter-----------------------------------------------------
id_plot_cov(sen_est_cov,cov_type='person_cov',
            filter_cov=c('age','party_codeR:age')) + 
  ggtitle('Effect of Age by Party on Ideal Point Scores in 114th Senate')

## ----cov_plot_filter_label-----------------------------------------------
id_plot_cov(sen_est_cov,cov_type='person_cov',
            filter_cov=c('age','party_codeR:age')) + 
  ggtitle('Effect of Age by Party on Ideal Point Scores in 114th Senate') +
  scale_y_discrete(breaks=c('age','party_codeR:age'),
                   labels=c('Democrats','Republicans'))

## ----cov_values----------------------------------------------------------
cov_sum <- summary(sen_est_cov,pars='person_cov')
knitr::kable(cov_sum)

