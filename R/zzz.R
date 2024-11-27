.onLoad <- function(libname, pkgname) {
  # check for cmdstanR installation
  if (!requireNamespace("cmdstanr")) {
    packageStartupMessage("Note: you have not installed cmdstanr. To do so, please go to https://mc-stan.org/cmdstanr/.")
  } 
  
  # list of global variables to declare
  
  variable_list <- c(
    ".chain",".", ".draw", ".iteration", "ar_sd", "ar1_down", "ar1_up", "basis",
    "boundary_prior", "const_type", "cont", "debug_mode", "diff_miss_sd", "diff_reg_sd",
    "discrete", "discrim_miss_lb", "discrim_miss_scale", "discrim_miss_shape",
    "discrim_miss_upb", "discrim_reg_lb", "discrim_reg_scale", "discrim_reg_shape",
    "discrim_reg_upb", "extra_params.rds", "extract", "fix_high", "fix_low",
    "fixtype",  "gp_m_sd_par", "gp_min_length", "gp_num_diff", "gp_sd_par",
    "grainsize", "Group.1", "Group.2", "het_var", "High", "id_refresh", "ideal_pts_mean",
    "ignore", "inflate_zero", "item_id", "key", "key2", "line_group1", "line_group2", "ll", "Low",
    "lower", "m", "map_over_id", "max_val", "Median", "model_id", "model_type", "ncores", "null",
    "num_cats", "num_restrict_high", "num_restrict_low", "outcomel",
    "person_sd", "prior_only", "restrict_ind_high",
    "restrict_ind_low", "restrict_N_high", "restrict_N_low", "restrict_sd_high",
    "restrict_sd_low", "restrict_var", "sample_it", "sample_size", "sampling",
    "some_missing", "spline_degree", "stanmodels", "subset_group",
    "subset_person", "time_center_cutoff", "time_fix_sd", "time_var", "upper",
    "use_groups", "use_subset", "variable", "within_chain", "x",
    'get_samples2',
      'abs_data',
      'abs_mid',
      'bill_pos',
      'group_id',
      'Iteration',
      'outcome',
      'person_id',
      'reg_data',
      'reg_mid',
      'time_id',
      'time_point',
      'true_pt',
      'variables',
      'item_high',
      'item_low',
      'item_median',
      "Model",
      '5%',
      '95%',
      ":=",
      'item_type')
  
  utils::globalVariables(variable_list)
  
}
