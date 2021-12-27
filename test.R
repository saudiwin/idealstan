require(idealstan)
require(dplyr)
require(ggplot2)
require(lubridate)

data('delaware')

# Absent to missing

delaware$outcome[delaware$outcome=="Absent"] <- NA

# adjust data to 0/1

delaware$outcome <- as.numeric(delaware$outcome=="Yes")

delaware_data <- id_make(delaware,outcome_disc = 'outcome',
                         person_id = 'person_id',
                         item_id = 'item_id',
                         group_id= 'group_id',
                         time_id='time_id')


del_est_ar2 <- id_estimate(delaware_data,
                           model_type = 1,
                           use_groups = T,
                           fixtype='prefix',
                           nchains=2,time_var=10,
                           ncores=16,
                           vary_ideal_pts='AR1',
                           restrict_ind_high = "D",
                           restrict_ind_low="R",
                           seed=84520)

del_est_gp1 <- id_estimate(delaware_data,
                           model_type = 1,
                           use_groups = T,
                           fixtype='prefix',
                           nchains=2,
                           ncores=16,
                           niters = 500,
                           vary_ideal_pts='GP',
                           restrict_ind_high = "D",
                           restrict_ind_low="R",
                           seed=84520)