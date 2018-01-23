## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,warning=TRUE,fig.align = 'center')
require(idealstan)
require(dplyr)
require(ggplot2)
require(loo)
require(bayesplot)

## ----sim_irt_2pl---------------------------------------------------------
irt_2pl <- id_sim_gen(ordinal=FALSE,absence=TRUE)

## ----fit_irt_2pl---------------------------------------------------------

irt_2pl_correct <- id_estimate(idealdata=irt_2pl,
                               model_type=2,
                               restrict_ind_high = sort(irt_2pl@simul_data$true_reg_discrim,
                                                        decreasing=TRUE,
                                                        index=TRUE)$ix[1:3],
                              restrict_ind_low = sort(irt_2pl@simul_data$true_reg_discrim,
                                                      decreasing=FALSE,
                                                        index=TRUE)$ix[1:3],
                           restrict_params = 'discrim_reg',
                               restrict_type = 'constrain_twoway',
                               fixtype='constrained')

irt_2pl_incorrect <- id_estimate(idealdata=irt_2pl,
                               model_type=2,
                               restrict_ind_high = c(1,2),
                           restrict_params = 'person',
                           pin_vals=c(-1,1.5),
                               fixtype='pinned')


## ----rhats_correct-------------------------------------------------------
id_plot_rhats(irt_2pl_correct)

## ----rhats_incorrect-----------------------------------------------------
id_plot_rhats(irt_2pl_incorrect)

## ----post_pred-----------------------------------------------------------
post_correct <- id_post_pred(irt_2pl_correct)
post_incorrect <- id_post_pred(irt_2pl_incorrect)

## ----post_pred_graph-----------------------------------------------------

id_plot_ppc(irt_2pl_correct,ppc_pred=post_correct)
id_plot_ppc(irt_2pl_incorrect,ppc_pred=post_incorrect)


## ----post_pred_ind-------------------------------------------------------
id_plot_ppc(irt_2pl_incorrect,ppc_pred=post_incorrect,person=c(1,2))

## ----log_lik-------------------------------------------------------------
log_lik_irt_2pl_correct <- id_log_lik(irt_2pl_correct)
log_like_irt_2pl_incorrect <- id_log_lik(irt_2pl_incorrect)

## ----loo_show------------------------------------------------------------
loo(log_lik_irt_2pl_correct)
loo(log_like_irt_2pl_incorrect)

## ----loo_compare---------------------------------------------------------
compare(loo(log_lik_irt_2pl_correct),
        loo(log_like_irt_2pl_incorrect))

