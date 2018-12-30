require(idealstan)

data('delaware')

delaware_data <- id_make(delaware,outcome = 'outcome',
                         person_id = 'person_id',
                         item_id = 'item_id',
                         group_id= 'group_id',
                         time_id='time_id',
                         miss_val='Absent')

del_est_gp1 <- id_estimate(delaware_data,
                           model_type = 2,
                           use_vb = F,
                           use_groups = T,
                           gp_sd_par = .5,
                           gp_m_sd_par = .5,
                           gp_num_diff=4,
                           fixtype='vb_partial',
                           vary_ideal_pts='GP',
                           restrict_ind_high = "D",
                           restrict_ind_low="R",
                           seed=84520)

del_est_rw3 <- readRDS('del_est_rw3.rds')
del_est_ar2 <- readRDS('del_est_ar2.rds')

id_plot_legis_dyn(list(RW=del_est_rw3,
                       AR1=del_est_ar2,
                       GP=del_est_gp1),
                  include=c('D','R'))
