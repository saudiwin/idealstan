# test covariates + redoing person time trends

library(idealstan)
library(dplyr)
require(tidyr)
require(idealstan)
require(lubridate)

setwd("~/idalstan_compare")

rollcalls <- readRDS('data/rollcalls.rds') %>% 
  select(cast_code,rollnumber,congress,year,district_code,state_abbrev,date,
         bioname,party_code,date_month,unemp_rate) %>% 
  mutate(item=paste0(congress,"_",rollnumber),
         cast_code=recode_factor(cast_code,Abstention=NA_character_),
         cast_code=as.numeric(cast_code)-1,
         bioname=factor(bioname),
         bioname=relevel(bioname,"DeFAZIO, Peter Anthony")) %>% 
  mutate(bioname=factor(bioname)) %>% 
  distinct %>% 
  filter(date_month>ymd("2017-12-31"))

# drop legislators who vote on fewer than 25 unanimous bills
# drop bills where >95% of the votes are the  same

check_bills <- group_by(rollcalls,item,cast_code) %>% count %>% 
  group_by(item) %>% 
  mutate(n_prop=n/(sum(n))) %>% 
  summarize(high_perc=max(n_prop,na.rm=T)) %>% 
  filter(high_perc>0.95)

rollcalls <- anti_join(rollcalls, check_bills)

legis_count <- group_by(rollcalls, item) %>% 
  mutate(unan=all(cast_code[!is.na(cast_code)]==1) || all(cast_code[!is.na(cast_code)]==0)) %>% 
  group_by(bioname) %>% 
  summarize(n_votes_nonunam=length(unique(item[!unan])))

# check number of days in legislature

num_days <- distinct(rollcalls,bioname,date_month) %>% 
  count(bioname)

rollcalls <- anti_join(rollcalls, filter(legis_count, n_votes_nonunam<25),by="bioname") %>% 
  anti_join(filter(num_days,n<10),by="bioname")

# we probably want to drop unanimous votes

unam_votes <- group_by(rollcalls, item,cast_code) %>% 
  #summarize(unan=all(cast_code[!is.na(cast_code)]==1) || all(cast_code[!is.na(cast_code)]==0))
  count %>% 
  spread(key="cast_code",value = 'n') %>% 
  mutate(perc_miss=`<NA>`/(`<NA>` + `0` + `1`))

# polarizing bills

polar_bills <- count(rollcalls, item, cast_code) %>% 
  filter(!is.na(cast_code)) %>% 
  group_by(item) %>% 
  summarize(vote_split=abs((n[1] - n[2])/sum(n))) %>% 
  filter(vote_split==0)

# check % miss by year

miss_year <- group_by(rollcalls, bioname, date_month) %>% 
  summarize(perc_miss=sum(is.na(cast_code))/n()) %>% 
  ungroup %>% 
  complete(bioname,date_month) %>% 
  group_by(bioname) %>% 
  arrange(bioname,date_month) %>% 
  mutate(perc_miss=case_when(is.na(perc_miss)~0,
                             perc_miss==1~0,
                             TRUE~1))

rollcalls$bioname <- factor(rollcalls$bioname)

unemp1 <- rollcalls %>% 
  filter(bioname %in% c("SCHIFF, Adam",
                        "PELOSI, Nancy",
                        "ROHRABACHER, Dana",
                        "BARTON, Joe Linus")) %>% 
  id_make(outcome_disc="cast_code",
          item_id="item",
          person_id="bioname",
          group_id="party_code",
          time_id = "date_month",
          person_cov = ~unemp_rate*party_code)

unemp1@person_cov <- c(unemp1@person_cov[1],unemp1@person_cov[4:5])
unemp1@score_matrix <- select(unemp1@score_matrix,item_id:unemp_rate,
                              `unemp_rate:party_codeR`:discrete)

unemp1_fit <- id_estimate(unemp1,model_type=2,
                          vary_ideal_pts = 'AR1',
                          niters=300,
                          warmup=300,ignore_db = select(miss_year,
                                                        person_id="bioname",
                                                        time_id="date_month",
                                                        ignore="perc_miss"),
                          nchains=3,
                          ncores=parallel::detectCores(),
                          grainsize=1,
                          restrict_ind_high = "115_919",
                          restrict_ind_low="115_952",
                          #restrict_ind_high = "BARTON, Joe Linus",
                          #restrict_ind_low="DeFAZIO, Peter Anthony",
                          restrict_sd_low = .01,
                          restrict_sd_high = .01,
                          time_var=5000,
                          #max_treedepth=12,
                          #adapt_delta=0.95,
                          #fix_low=0,
                          fixtype="prefix",restrict_var = F,
                          fix_low=-1,const_type="items",
                          compile_optim=F,
                          # pars=c("steps_votes_grm",
                          #        "steps_votes",
                          #        "B_int_free",
                          #        "A_int_free"),
                          #include=F,
                          id_refresh=100)
