## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,warning=TRUE,fig.align = 'center',fig.width=6, fig.height=5)
require(idealstan)
require(dplyr)
require(ggplot2)
require(loo)
require(bayesplot)
options(mc.cores=2)

## ----sim_irt_2pl---------------------------------------------------------
irt_2pl <- id_sim_gen(inflate=TRUE)

## ----fit_irt_2pl---------------------------------------------------------
# Because of CRAN limitations, only using 2 cores & 2 chains
irt_2pl_correct <- id_estimate(idealdata=irt_2pl,
                               model_type=2,
                               restrict_ind_high = as.character(sort(irt_2pl@simul_data$true_person,
                                                        decreasing=TRUE,
                                                        index=TRUE)$ix[1]),
                              restrict_ind_low = as.character(sort(irt_2pl@simul_data$true_person,
                                                      decreasing=FALSE,
                                                        index=TRUE)$ix[1]),
                               fixtype='vb_partial',
                           ncores=2,
                           nchains=2,
                           niters = 500)

irt_2pl_incorrect <- id_estimate(idealdata=irt_2pl,
                               model_type=8,
                            restrict_ind_high = as.character(sort(irt_2pl@simul_data$true_person,
                                                        decreasing=TRUE,
                                                        index=TRUE)$ix[1]),
                            restrict_ind_low = as.character(sort(irt_2pl@simul_data$true_person,
                                                      decreasing=FALSE,
                                                        index=TRUE)$ix[1]),
                            fixtype='vb_partial',
                           ncores=2,
                           nchains=2,
                           niters=500)


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
id_plot_ppc(irt_2pl_incorrect,ppc_pred=post_incorrect,group=c('1','2'))

## ----log_lik-------------------------------------------------------------
log_lik_irt_2pl_correct <- id_post_pred(irt_2pl_correct,type='log_lik')
log_lik_irt_2pl_incorrect <- id_post_pred(irt_2pl_incorrect,type='log_lik')


## ----loo_show------------------------------------------------------------
correct_loo <- loo(log_lik_irt_2pl_correct,
                   cores=2,
                   r_eff=relative_eff(exp(log_lik_irt_2pl_correct),
                                      chain_id=derive_chain(log_lik_irt_2pl_correct)))

incorrect_loo <- loo(log_lik_irt_2pl_incorrect,
                     cores=2,
                     r_eff=relative_eff(exp(log_lik_irt_2pl_incorrect),
                                      chain_id=derive_chain(log_lik_irt_2pl_incorrect)))

print(correct_loo)
print(incorrect_loo)

## ----loo_compare,warn=F,message=F----------------------------------------
compare(correct_loo,
        incorrect_loo)

