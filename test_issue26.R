library(readr)
library(idealstan)


this_d <- read_csv("issue26.csv")

this_d$ordered_id <- 6
this_d$model_id <- 6

idealstan_object <- id_make(
  score_data = this_d
)

DAN_GALTAN_idealstan_output_PREFIX <- id_estimate(
  idealdata = idealstan_object,
  model_type = 3,use_method = "pathfinder",
  vary_ideal_pts = "random_walk",
  fixtype = "prefix",
  const_type = "items",
  restrict_ind_high = "61",
  restrict_ind_low = "10",
  ncores = parallel::detectCores(),
  nchains = 4,
  id_refresh = 10
)
