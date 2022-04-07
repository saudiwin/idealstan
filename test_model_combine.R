# test new identification options
# March 19, 2019

require(idealstan)
require(dplyr)
require(ggplot2)

# to test this, we'll generate two different models then recombine

bin_irt_2pl_sim <- id_sim_gen(num_person=10,num_bills=100,
                              model_type="ordinal_ratingscale",inflate=F,
                              diff_sd=1,
                              reg_discrim_sd = 1,
                              absence_discrim_sd = 1)

 norm_irt_2pl_sim <- id_sim_gen(num_person=10,num_bills=100,ordinal=F,inflate=F,
                              model_type="normal",
                              diff_sd=1,
                              reg_discrim_sd = 1,
                              absence_discrim_sd = 1)

# combine objects

comb_obj <- bin_irt_2pl_sim

comb_obj@simul_data$true_person <- (comb_obj@simul_data$true_person +
  norm_irt_2pl_sim@simul_data$true_person)/2

norm_irt_2pl_sim@score_matrix$item_id <- factor(as.numeric(norm_irt_2pl_sim@score_matrix$item_id) +
  max(as.numeric(bin_irt_2pl_sim@score_matrix$item_id)))

comb_obj@score_matrix <- bind_rows(comb_obj@score_matrix,
                                   norm_irt_2pl_sim@score_matrix)

comb_obj@score_matrix <-mutate(comb_obj@score_matrix,model_id=c(rep(3,nrow(bin_irt_2pl_sim@score_matrix)),
                                                rep(9,nrow(norm_irt_2pl_sim@score_matrix))))


# create combined data object

comb_obj_tostan <- id_make(comb_obj@score_matrix,
                           miss_val = c(NA,NA))

# run a pretend model

est_comb_obj <- id_estimate(comb_obj_tostan,id_refresh = 100)

# now run a real model with real data

require(haven)
require(tidyr)

hb_data <- read_dta("~/idalstan_compare/data/baldwin_huber/apsrfinaldata.dta") %>% 
  slice(1:46) %>% 
  select(country,ccode,pg,ELF_fearon_std,betweenstd,cultfrac_std,GIstd,gini_net_std,geo_iso_std,lngdpstd,
         popstd,polity2std,afrobarom,wvs,cses,cses_wvs,ELF_ethnic,polity2,between_afrorev) %>% 
  gather(key="item_id",value="value",-country,-ccode,-pg) %>% 
  filter(!is.na(value))

# now assign models

hb_data <- mutate(hb_data,model_id=recode(item_id,
                                          ELF_fearon_std=9,
                                          betweenstd=9,
                                          cultfrac_std=9,
                                          GIstd=9,
                                          gini_net_std=9,
                                          geo_iso_std=9,
                                          lngdpstd=9,
                                          popstd=9,
                                          polity2std=9,
                                          afrobarom=1,
                                          wvs=1,
                                          cses=1,
                                          cses_wvs=1,
                                          ELF_ethnic=9,
                                          polity2=11,
                                          between_afrorev=9),
                  outcome_cont=ifelse(model_id==1,NA,value),
                  outcome_disc=ifelse(model_id==1,value,NA))

# make idealstan object

hb_idealdata <- id_make(hb_data,person_id = "country",person_cov = ~pg)

# run idealstan model!

hb_fit <- id_estimate(hb_idealdata,restrict_ind_high="betweenstd",
                      restrict_ind_low="polity2",
                      const_type="items",id_refresh = 100,
                      fixtype="fixed")

id_plot_cov(hb_fit)

# look at discrimination scores