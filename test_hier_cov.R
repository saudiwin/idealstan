# test hierarchical covariates

require(idealstan)
require(ggplot2)
require(bayesplot)
require(tidyverse)
require(lubridate)

# use a small subset of the rollcall data

rollcalls <- readRDS('~/idalstan_compare/data/rollcalls.rds') %>% 
  select(cast_code,rollnumber,congress,year,district_code,state_abbrev,date,
         bioname,party_code,date_month,unemp_rate) %>% 
  mutate(item=paste0(congress,"_",rollnumber),
         cast_code=recode_factor(cast_code,Abstention=NA_character_),
         cast_code=as.numeric(cast_code)-1,
         bioname=factor(bioname)) %>% 
  filter(bioname %in% c("BARTON, Joe Linus",
                        "DeFAZIO, Peter Anthony",
                        "LEVIN, Sander Martin",
                        "ROGERS, Harold Dallas (Hal)"),
         date_month<ymd("1991-01-01")) %>%
  distinct


unemp1 <- rollcalls %>% 
  id_make(outcome_disc="cast_code",
          item_id="item",
          person_id="bioname",
          group_id="party_code",
          time_id = "date_month",
          person_cov = ~unemp_rate*party_code)


unemp1_fit <- id_estimate(unemp1,model_type=2,
                          vary_ideal_pts = 'AR1',
                          niters=300,
                          warmup=300,
                          nchains=1,
                          ncores=1,const_type = "persons",
                          restrict_ind_high = "BARTON, Joe Linus",
                          restrict_ind_low="DeFAZIO, Peter Anthony",
                          fix_low=0,restrict_sd_low = 3,
                          fixtype="prefix",time_center_cutoff = 5,
                          id_refresh=100)

# now let's do some "technical" analysis

id_plot_legis_dyn(unemp1_fit)

debugonce(id_plot_cov)

id_plot_cov(unemp1_fit)
