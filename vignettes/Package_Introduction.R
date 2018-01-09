## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,warning=FALSE,fig.align = 'center',fig.width = 7,fig.height=4)
require(idealstan)
require(dplyr)
require(ggplot2)

## ----sim_data------------------------------------------------------------

ord_ideal_sim <- id_sim_gen()
knitr::kable(as_data_frame(head(ord_ideal_sim@score_matrix)))

## ----constrain_sim-------------------------------------------------------
true_legis <- ord_ideal_sim@simul_data$true_person
high_leg <- sort(true_legis,decreasing = T,index.return=T)
low_leg <- sort(true_legis,index.return=T)

ord_ideal_est <- id_estimate(idealdata=ord_ideal_sim,
                             model_type=4,
                             fixtype='constrained',
                             restrict_params='person',
                             restrict_type='constrain_twoway',
                             restrict_ind_high = high_leg$ix[1:2],
                             restrict_ind_low=low_leg$ix[1:2],
                             refresh=500)

## ----check_true----------------------------------------------------------
id_plot_sims(ord_ideal_est,type='Residuals')

## ----use_senate----------------------------------------------------------

data('senate114')

senate_data <-
  id_make(score_data = senate114,
    ordinal = F,
    include_pres=F)

hist(senate_data@score_matrix)


## ----run_114_model-------------------------------------------------------
sen_est <- id_estimate(senate_data,
                model_type = 2,
                 use_vb = T,
                 ncores=4,
                 nfix=2,
                 restrict_type='constrain_oneway',
                 restrict_params='person',
                 restrict_ind_high = c(which(row.names(senate114$votes[-1,])=='SASSE (R NE)'),
                                                   which(row.names(senate114$votes[-1,])=='CRUZ (R TX)'),
                                                   which(row.names(senate114$votes[-1,])=='RUBIO (R FL)')),
                 restrict_ind_low=c(which(row.names(senate114$votes[-1,])=='SANDERS (Indep VT)'),
                                                which(row.names(senate114$votes[-1,])=='REID (D NV)'),
                                                which(row.names(senate114$votes[-1,])=='WARREN (D MA)')),
                 auto_id=F,
                 fixtype='constrained',
            seed=84520,
            refresh=500)
id_plot_legis(sen_est,person_ci_alpha=0.7) + scale_colour_brewer(type='qual')

## ----bill_plot-----------------------------------------------------------
id_plot_legis(sen_est,person_ci_alpha=0.1,item_plot=205,
        abs_and_reg='Non-inflated') + scale_colour_brewer(type='qual')

