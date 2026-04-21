library(idealstan)

# from https://github.com/saudiwin/idealstan

# note that idealstan uses cmdstanr, not rstan

# generate static model with missingness
# output is idealdata object with data in long form
# one row per item per person per obs

miss_data <- id_sim_gen(inflate=TRUE)

# data in attribute score_matrix

str(miss_data@score_matrix)

# outcome_disc = binary outcome
# item_id = item indicator
# person_id = person indicator
# missing data is coded in data as NA

# true ideal points are in attribute on object

true_ideal_pts <- miss_data@simul_data$true_person

str(true_ideal_pts)

# estimate binary outcome model with missing-data adjustment
# model type = 2 (1 is no missing data)
# output of simulation function can go directly to idealstan

restrict_high <- which(true_ideal_pts[,1]==max(true_ideal_pts[,1]))
restrict_low <- which(true_ideal_pts[,1]==min(true_ideal_pts[,1]))

missing_mod <- id_estimate(miss_data, 
                           model_type = 2,
                           ncores = 4,
                           restrict_ind_high = restrict_high,
                           restrict_ind_low = restrict_low,
                           nchains = 1,niters = 1000)

# idealstan has a function to compare true/estimated ideal pts

id_plot_legis(missing_mod, show_true = T)

# the estimated values can be retrieved with the summary function

ideal_pts <- summary(missing_mod)

str(ideal_pts)

# also get the discrimination parameters (missing & observed)

discrim_params <- summary(missing_mod, pars="items")
