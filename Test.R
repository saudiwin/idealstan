# Test file to show how package works

# Import latest senate data using pscl
require(pscl)
require(dplyr)
require(idealstan)
require(tidyr)
require(ggplot2)
require(bayesplot)
require(readr)

#this is out of date, use new CSV Files
newdata <- readKH(file='senate_114.ord')

#Need to drop Obama

to_use <- newdata$votes[-1,]
newdata$legis.data <- newdata$legis.data[-1,]
to_use <- apply(to_use,2,function(x) {
  y <- recode(x,`1`=3L,`6`=1L,`7`=2L,`9`=4L)
  return(y)
})

bill_data <- read_csv('rollcall_senate_114.csv')

all_vals <- table(to_use)

#Save row names (Congresspeople)

rownames(to_use) <- rownames(newdata$legis.data)

idealdata <- make_idealdata(vote_data=to_use,legis_data=newdata$legis.data,votes=as.character(names(all_vals[1:3])),
                           abs_vote = '4')

# estimated_full <- estimate_ideal(idealdata=idealdata,use_subset = FALSE,sample_it=TRUE,ncores = 2,
#                             use_vb = FALSE)
# 
# estimated_vb <- estimate_ideal(idealdata=idealdata,use_subset = FALSE,sample_it=FALSE,ncores = 2,
# 



# Now try a binary inflated model by excluding the abstention category (2)

ideal_data_binary <- make_idealdata(vote_data=to_use,legis_data=newdata$legis.data,votes=as.character(names(all_vals[1:3])),
                                    abs_vote = '4',exclude_level='2')

estimated_binary_test <- estimate_ideal(idealdata=ideal_data_binary,use_subset = FALSE,ncores = 4,
                                   use_vb = FALSE,nfix=c(5,5),restrict_params ='person',sample_it=FALSE,sample_size=30)

all_out <- rstan::extract(estimated_binary_test@stan_samples,permuted=FALSE)

if(estimated_binary@use_vb==FALSE) {
  all_out <- as.array(all_out)
  mcmc_violin(all_out,pars='B_yes[502]') + theme_minimal()
  mcmc_violin(all_out,regex_pars='L_restrict_high') + theme_minimal()
  mcmc_violin(all_out,regex_pars='L_restrict_low') + theme_minimal()
} else {
  mcmc_dens(all_out,pars='sigma_abs_open[1]')
  mcmc_dens(all_out,regex_pars='sigma_abs_restrict')
}

ideal_data_binary <- make_idealdata(vote_data=to_use,legis_data=newdata$legis.data,votes=as.character(names(all_vals[1:3])),
                                    abs_vote = '4',exclude_level='2',inflate=FALSE)

estimated_binary_no_inflate <- estimate_ideal(idealdata=ideal_data_binary,use_subset = FALSE,ncores = 4,
                                   modeltype='binary_2pl',
                                   use_vb = FALSE,nfix=c(5,5),restrict_params ='person',sample_it=FALSE,sample_size=30)

if(estimated_binary_no_inflate@use_vb==FALSE) {
  all_out <- as.array(rstan::extract(estimated_binary_no_inflate@stan_samples,permuted=FALSE))
  mcmc_violin(all_out,regex_pars='L_restrict_high') + theme_minimal()
  mcmc_violin(all_out,regex_pars='L_restrict_low') + theme_minimal()
} else {
  mcmc_dens(all_out,pars='sigma_abs_open[1]')
  mcmc_dens(all_out,regex_pars='sigma_abs_restrict')
}

compare_models(model1=estimated_binary_test,model2=estimated_binary_no_inflate,
               scale_flip=F,labels=c('Absences','No Absences'),hjust=-0.1)

ggsave(filename = 'compared_UScong.png',width = 10,height=7,units='in')

# Look at differences in ideal points by data

absence <- plot_model(estimated_binary,return_data=TRUE)$plot_data
absence <- mutate(absence,median_pt=median_pt*-1,
                  low_pt=low_pt*-1,
                  high_pt=high_pt*-1) %>% arrange(median_pt)
no_absence <- plot_model(estimated_binary_no_inflate,return_data=TRUE)$plot_data %>% arrange(median_pt)
combined <- bind_rows(mutate(absence,model_type='absence',ranks=rank(median_pt),
                             ranks=if_else(median_pt<0,ranks,101-ranks)),
                      mutate(no_absence,model_type='no absence',ranks=rank(median_pt),
                             ranks=if_else(median_pt<0,ranks,101-ranks)))

group_by(combined,model_type) %>% summarize(mean(abs(high_pt-low_pt)))

combined <- arrange(combined,legis.names,model_type) %>% group_by(legis.names) %>% mutate(changed_legis=median_pt - lag(median_pt),
                                                                                          changed_rank=ranks-lag(ranks))


# Looking at bills

plot_model(estimated_binary_test,bill_plot='Vote 275',text_size_label=3,text_size_party=2)

#manual check of discrim/diff parameters

get_all <- rstan::extract(estimated_binary_test@stan_samples)

avg_particip <- get_all$avg_particip

B_yes <- get_all$B_yes
B_abs <- get_all$B_abs
sigma_abs <- get_all$sigma_abs_open
sigma_full <- get_all$sigma_full

discrim1 <- mean(B_abs[,187]/sigma_abs[,187])
discrim2 <- mean(B_yes[,187]/sigma_full[,187])

all_bs <- apply(B_yes,2,mean)
all_bs_abs <- apply(B_abs,2,mean)
all_sigma <- apply(sigma_full,2,mean)
all_sigma_abs <- apply(sigma_abs,2,mean)



# Now let's use the simulation functions

test_data <- simulate_absence()

# Now make second test_data that strips out the absences and switches to binary

test_data_bin <- make_idealdata(vote_data=test_data@vote_data@vote_matrix,legis_data = test_data@vote_data@legis_data,
                                yes_vote = 3,no_vote = 1,
                                inflate = FALSE,ordinal=FALSE)

#See if this works

test_data_bin <- estimate_ideal(idealdata = test_data_bin,use_vb=TRUE)
test_data_abs <- estimate_ideal(idealdata = test_data,use_vb=TRUE,modeltype='ratingscale_absence_inflate')
plot_model(test_data_bin,hjust_length=-2)
compare_models(test_data_bin,test_data_abs)
