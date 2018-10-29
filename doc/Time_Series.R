## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,warning=FALSE,fig.align = 'center',fig.width=6, fig.height=5)
require(idealstan)
require(dplyr)
require(ggplot2)
require(lubridate)

## ----example_data--------------------------------------------------------
data('senate114')
knitr::kable(head(senate114))

## ----date_hist-----------------------------------------------------------
senate114 %>% 
  distinct(rollnumber,date) %>% 
  ggplot(aes(x=date)) +
  geom_bar() +
  theme_minimal() + 
  ylab('Count of Rollcall Votes') +
  xlab('') +
  ggtitle('Count of Votes by Day in the 114th Senate')

## ----change_date---------------------------------------------------------
day(senate114$date) <- 1

## ----date_hist2----------------------------------------------------------
senate114 %>% 
  distinct(rollnumber,date) %>% 
  ggplot(aes(x=date)) +
  geom_bar() +
  theme_minimal() + 
  ylab('Count of Rollcall Votes') +
  xlab('') +
  ggtitle('Count of Votes by Month in the 114th Senate')

## ----create_data---------------------------------------------------------
senate_data <- id_make(senate114,outcome = 'cast_code',
                       person_id = 'bioname',
                       item_id = 'rollnumber',
                       group_id= 'party_code',
                       time_id='date',
                       miss_val='Absent')

## ----create_data2--------------------------------------------------------
sen_est <- id_estimate(senate_data,
                model_type = 2,
                 use_vb = T,
                fixtype='vb_partial',
                vary_ideal_pts='random_walk',
                 restrict_ind_high = "WARREN, Elizabeth",
                 restrict_ind_low="BARRASSO, John A.",
            seed=84520)

## ----plo_rw1, fig.height=8-----------------------------------------------
id_plot_legis_dyn(sen_est,use_ci = F)

## ----more_rw_var,fig.height=8--------------------------------------------
sen_est <- id_estimate(senate_data,
                model_type = 2,
                 use_vb = T,
                restrict_var_high = .5,
                fixtype='vb_partial',
                vary_ideal_pts='random_walk',
                 restrict_ind_high = "WARREN, Elizabeth",
                 restrict_ind_low="BARRASSO, John A.",
            seed=84520)

id_plot_legis_dyn(sen_est,use_ci = F)

## ----rw_var_est,fig.height=8---------------------------------------------
id_plot_legis_var(sen_est)

## ----var_est-------------------------------------------------------------
out_d <- id_plot_legis_var(sen_est,return_data = T)
knitr::kable(head(out_d$plot_data))

## ----ar1_1,fig.height=8--------------------------------------------------
sen_est <- id_estimate(senate_data,
                model_type = 2,
                 use_vb = T,
                time_sd = .2,
                fixtype='vb_partial',
                vary_ideal_pts='AR1',
                 restrict_ind_high = "WARREN, Elizabeth",
                 restrict_ind_low="BARRASSO, John A.",
            seed=84520)

id_plot_legis_dyn(sen_est,use_ci = F)

## ----sum_ideal_pt--------------------------------------------------------
summary(sen_est,pars='ideal_pts') %>% 
  head %>% 
  knitr::kable(.)

## ----mcmc_stan-----------------------------------------------------------
stan_trace(sen_est,'L_tp1[1,1]')

## ----stationary_groups---------------------------------------------------
sen_est <- id_estimate(senate_data,
                model_type = 2,
                 use_vb = T,
                time_sd=0.2,
                use_groups = T,
                fixtype='vb_partial',
                vary_ideal_pts='AR1',
                 restrict_ind_high = "D",
                 restrict_ind_low="R",
            seed=84520)
id_plot_legis_dyn(sen_est) + scale_colour_manual(values=c(R='red',
                                                          D='blue',
                                                          I='green'),
                                                 name="Parties")
  

## ----party_mid-----------------------------------------------------------
id_plot_legis_dyn(sen_est,item_plot='342',text_size_label = 5) + scale_colour_manual(values=c(R='red',
                                                          D='blue',
                                                          I='green'),
                                                 name="Parties") +
  ggtitle('Time-Varying Party-level Ideal Points for the 114th Senate',
          subtitle = 'Midpoint (Line of Indifference to Voting) for 342nd Roll-call Vote as Dotted Line') +
  guides(color='none') +
  annotate(geom='text',
           x = ymd('2016-01-01'),
           y=-1,
           label='Confirmation Vote for Wilhelmina Wright as U.S. District Judge')

