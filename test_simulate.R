# Test file to show how package works

# Import latest senate data using pscl
require(pscl)
require(dplyr)
require(idealstan)
require(tidyr)
require(ggplot2)
require(bayesplot)
require(readr)

# Now let's use the simulation functions

test_data <- simulate_absence(num_legis=100,num_bills=500)

# Now make second test_data that strips out the absences and switches to binary

test_data_bin <- make_idealdata(vote_data=test_data@vote_data@vote_matrix,legis_data = test_data@vote_data@legis_data,
                                yes_vote = 3,no_vote = 1,
                                inflate = TRUE,ordinal=FALSE)


#See if this works

test_data_bin <- estimate_ideal(idealdata = test_data_bin,use_vb=TRUE)
test_data_abs <- estimate_ideal(idealdata = test_data,use_vb=TRUE,modeltype='ratingscale_absence_inflate')
plot_model(test_data_abs,hjust_length=-2,show_true=TRUE)
test_data_bin@vote_data@legis_data$legis.names <- paste0('Legis_',test_data_bin@vote_data@legis_data$legis.names)
compare_models(test_data_bin,test_data_abs)

# Now let's see what pscl/dwnominate will do

 