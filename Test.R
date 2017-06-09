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
newdata <- readKH(file = 'senate_114.ord')
out_sum <- summary(newdata,verbose=TRUE)
legis_sum <- out_sum$legisTab
bill_sum <- out_sum$voteTab
#Need to drop Obama

to_use <- newdata$votes[-1, ]
newdata$legis.data <- newdata$legis.data[-1, ]
to_use <- apply(to_use, 2, function(x) {
  y <- recode(
    x,
    `1` = 3L,
    `6` = 1L,
    `7` = 2L,
    `9` = 4L
  )
  return(y)
})


bill_data <- read_csv('rollcall_senate_114.csv')

all_vals <- table(to_use)

#Save row names (Congresspeople)

rownames(to_use) <- rownames(newdata$legis.data)

idealdata <-
  make_idealdata(
    vote_data = to_use,
    legis_data = newdata$legis.data,
    abs_vote = 4,
    yes_vote = 3,
    no_vote = 1,
    abst_vote = 2,
    ordinal = TRUE
  )

estimated_full <-
  estimate_ideal(idealdata = idealdata,
                 model_type = 4,
                 use_vb = T,
                 ncores=4,
                 nfix=8,
                 restrict_params='legis')

# Now try non-inflated binary model

ideal_data_binary <-
  make_idealdata(
    vote_data = to_use,
    legis_data = newdata$legis.data,
    abs_vote = 4,
    yes_vote = 3,
    no_vote = 1,
    abst_vote = 2,
    ordinal = FALSE,
    inflate = FALSE
  )

estimated_no_abs <-
  estimate_ideal(idealdata = ideal_data_binary,
                 modeltype = 'binary_2pl',
                 use_vb = FALSE,
                 ncores = 4,
                 nfix=c(8,8),
                 restrict_params='person')

saveRDS(estimated_full, 'senate_114_bin_abs.rds')
saveRDS(estimated_no_abs, 'senate_114_bin_no_abs.rds')

 all_out <- rstan::extract(estimated_full@stan_samples,permuted=FALSE)


   all_out <- as.array(all_out)
   mcmc_violin(all_out,pars='B_yes[502]') + theme_minimal()
   mcmc_violin(all_out,regex_pars='restrict') + theme_minimal()

   all_out <- rstan::extract(estimated_full@stan_samples,permuted=FALSE)
   
   
   all_out_bin <- as.array(rstan::extract(estimated_no_abs@stan_samples,permuted=FALSE))
   mcmc_violin(all_out_bin,pars='B_yes[502]') + theme_minimal()
   mcmc_violin(all_out_bin,regex_pars='restrict') + theme_minimal()


compare_models(
  model1 = estimated_full,
  model2 = estimated_no_abs,
  scale_flip = F,
  labels = c('Absences', 'No Absences'),
  hjust = -0.3,
  palette='Paired',
  color_direction=-1,
  text_size_label = 2.2
)

ggsave(
  filename = 'compared_UScong.png',
  width = 10,
  height = 7,
  units = 'in'
)

# Look at differences in ideal points by data

absence <- plot_model(estimated_full, return_data = TRUE)$plot_data
absence <- mutate(
  absence,
  median_pt = median_pt * -1,
  low_pt = low_pt * -1,
  high_pt = high_pt * -1
) %>% arrange(median_pt)
no_absence <-
  plot_model(estimated_no_abs, return_data = TRUE)$plot_data %>% arrange(median_pt)
combined <-
  bind_rows(
    mutate(
      absence,
      model_type = 'absence',
      ranks = rank(median_pt),
      ranks = if_else(median_pt < 0, ranks, 101 -
                        ranks)
    ),
    mutate(
      no_absence,
      model_type = 'no absence',
      ranks = rank(median_pt),
      ranks = if_else(median_pt < 0, ranks, 101 -
                        ranks)
    )
  )

group_by(combined, model_type) %>% summarize(mean(abs(high_pt - low_pt)))

combined <-
  arrange(combined, legis.names, model_type) %>% group_by(legis.names) %>% mutate(changed_legis =
                                                                                    median_pt - lag(median_pt),
                                                                                  changed_rank =
                                                                                    ranks - lag(ranks))


# Looking at bills
# This is the bill with the highest number of absences = Vote 480
plot_model(
  estimated_full,
  bill_plot = 'Vote 464',
  text_size_label = 2.2,
  text_size_party = 1,
  abs_and_reg='absence',
  hjust_length=-.3,
  legis_ci_alpha=0.3
) + scale_color_discrete(guide=guide_legend(title='Vote')) + 
  scale_linetype(labels=c('90%','10%'), guide=guide_legend(title='Bill\nAbsence\nMidpoint\nHPD')) +
  theme(strip.text=element_blank())

ggsave(
  filename = 'energy_bill.png',
  width = 10,
  height = 7,
  units = 'in'
)

#manual check of discrim/diff parameters

get_all <- rstan::extract(estimated_full@stan_samples)

avg_particip <- get_all$avg_particip

B_yes <- get_all$B_yes
B_abs <- get_all$B_abs
sigma_abs <- get_all$sigma_abs_full
sigma_full <- get_all$sigma_full

discrim1 <- mean(B_abs[, 187] / sigma_abs[, 187])
discrim2 <- mean(B_yes[, 187] / sigma_full[, 187])

all_bs <- apply(B_yes, 2, mean)
all_bs_abs <- apply(B_abs, 2, mean)
all_sigma <- apply(sigma_full, 2, mean)
all_sigma_abs <- apply(sigma_abs, 2, mean)

bill_discrim <- all_bs_abs/all_sigma_abs
bill_order <- sort(bill_discrim,index.return=TRUE)
bill_order <- data_frame(x=bill_order$x,int=bill_order$ix) %>% mutate(dist_zero=(x-0)^2) %>% 
  arrange(dist_zero)
