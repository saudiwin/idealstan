# Estimate ARP over-time ideal points

# use this command to install development version of idealstan
# devtools::install_github('saudiwin/idealstan',branch='develop')

require(idealstan)
require(ggplot2)
require(dplyr)
require(readr)
require(stringr)
require(lubridate)
require(forcats)
require(tidyr)

# let's use tunisia data 
all_baws_data <- readRDS('all_bawala.rds')

recode_baws <- mutate(all_baws_data,law_date=if_else(law_title==law_date,
                                                     law_type,
                                                     law_date),
                      law_type=if_else(law_type==law_date,
                                       "Projet de loi en sa totalité",
                                       law_type),
                      law_day=str_extract(law_date,'[0-9]{1,2}'),
                      law_month=str_extract(law_date,'[\\p{L}\\p{M}]+'),
                      law_year=str_extract(law_date,'[0-9]{4}'),
                      law_month=recode(law_month,
                                       `août`='August',
                                       `avril`='April',
                                       `décembre`='December',
                                       `février`='February',
                                       `janvier`='January',
                                       `juillet`='July',
                                       `juin`='June',
                                       `mai`='May',
                                       `mars`='March',
                                       `novembre`='November',
                                       `octobre`='October',
                                       `septembre`='September'),
                      law_date=dmy(paste(law_day,law_month,law_year)),
                      law_duration=as.duration(interval(min(law_date),law_date))) 

ggplot(recode_baws,aes(x=law_date)) + geom_density(fill='grey',
                                                   colour=NA) + 
  theme_minimal() + xlab('') + ylab('Density of Bills') +
  theme(panel.grid=element_blank())
ggsave('bill_density.png')

#make a matrix for idealstan

to_ideal <- select(recode_baws,legis_names,clean_votes,law_title,law_type,law_date) %>% 
  mutate(law_unique=paste(law_title,law_type,law_date)) %>% 
  select(legis_names,clean_votes,law_unique) %>% 
  mutate(clean_votes=factor(clean_votes,levels=c('contre','abstenu','pour','absent',
                                                 'excuse')),
         clean_votes=fct_recode(clean_votes,absent='excuse'),
         clean_votes=as.numeric(clean_votes))

# we have duplicate records when people voted twice on the same thing in the same day
require(readr)

examine <- table(to_ideal$law_unique)
examine <- examine[examine>length(unique(to_ideal$legis_names))]
# # to_remove <- filter(to_ideal,law_unique %in% names(examine))
# recoded <-  c(names(examine),)
# load list of laws that had to be changed to make unique

to_remove_changed <- read_csv('to_remove.csv') %>% 
  filter(same_day!='Remove') %>% 
  mutate(clean_votes=factor(clean_votes,levels=c('contre','abstenu','pour','absent',
                                                 'excuse')),
         clean_votes=fct_recode(clean_votes,absent='excuse'),
         clean_votes=as.numeric(clean_votes))
to_ideal <- filter(to_ideal,!(law_unique %in% c(names(examine),
                                                "Projet de loi N°09/2016 relatif aux banques et aux établissements financiers Article 32 2016-05-11",
                                                "Projet de loi N°25/2016 relatif à la révision des avantages fiscaux Principe de passage au vote sur les articles 2017-02-01")))
to_ideal <- bind_rows(to_ideal,to_remove_changed)
to_ideal <- mutate(to_ideal,
                   law_unique=ifelse(is.na(same_day),
                                     law_unique,
                                     paste(law_unique,same_day)))
# now the key is unique even for multiple same-day votes
to_ideal_wide <- select(to_ideal,-same_day) %>% 
  spread(key = law_unique,value = clean_votes)

check_ideal <- group_by(to_ideal,law_unique,legis_names) %>% 
  count %>% filter(n>1)

vote_matrix <- dplyr::select(to_ideal_wide,-legis_names) %>% as.matrix

# need to get time values 

time <- lubridate::ymd(str_extract(colnames(vote_matrix),'[0-9]{4}-[0-9]{2}-[0-9]{2}'))
# standardize dates by converting all days to first day of month
day(time) <- 1
time_vals <- as.numeric(factor(as.numeric(time)))

row.names(vote_matrix) <- to_ideal_wide$legis_names
colnames(vote_matrix) <- paste0('Bill_',1:ncol(vote_matrix))
arp_ideal_data <- id_make(score_data = vote_matrix,
                          person_data=select(to_ideal_wide,legis_names),
                          miss_val=4L,time=time) 

# now see if we can estimate something
# random walk prior

estimate_all <- id_estimate(arp_ideal_data,use_vb = F,
                            use_groups = F,nfix = 1,
                            restrict_ind_high = 2,
                            restrict_ind_low=1,
                            model_type=4,
                            use_ar=T,
                            id_diff=20,
                            time_sd=20,
                            fixtype='vb')



id_plot_legis_dyn(estimate_all,person_labels = 'legis_names',highlight = 'Rim Mahjoub')

# plot the bugger

# we can get all estimated parameters with summary. The legislator ideal points will be
# L_tp1[t,n]

all_params <- summary(estimate_all,pars='L_tp1')

# look at plot 

all_params <- all_params %>% mutate(param_id=stringr::str_extract(parameters,'[0-9]\\]'),
                      param_id=as.numeric(stringr::str_extract(param_id,'[0-9]')),
                      param_id=factor(param_id,labels=c('R','X','D')),
                      time=stringr::str_extract(parameters,'\\[[0-9]+'),
                      time=as.numeric(stringr::str_extract(time,'[0-9]+')))

all_params %>% 
  filter(param_id!='X') %>% 
  ggplot(aes(y=posterior_median,x=time)) +
  geom_line(aes(colour=param_id),size=1) +
  geom_ribbon(aes(ymin=Prob.025,
                  ymax=Prob.975,
                  colour=param_id),
              alpha=0.3)

# now try with an AR(1) (stationary) model

estimate_all <- id_estimate(to_ideal,use_vb = T,
                            use_groups = T,nfix = 1,restrict_type='constrain_twoway',
                            fixtype='constrained',
                            restrict_ind_high = 2,
                            restrict_ind_low=1,
                            use_ar = T)

# we can get all estimated parameters with summary. The legislator ideal points will be
# L_tp1[t,n]

all_params <- summary(estimate_all,pars='L_tp1')

# look at plot 

all_params <- all_params %>% mutate(param_id=stringr::str_extract(parameters,'[0-9]\\]'),
                                    param_id=as.numeric(stringr::str_extract(param_id,'[0-9]')),
                                    param_id=factor(param_id,labels=c('X','R','D')),
                                    time=stringr::str_extract(parameters,'\\[[0-9]+'),
                                    time=as.numeric(stringr::str_extract(time,'[0-9]+')))

all_params %>% 
  filter(param_id!='X') %>% 
  ggplot(aes(y=posterior_median,x=time)) +
  geom_line(aes(colour=param_id),size=1) +
  geom_ribbon(aes(ymin=Prob.025,
                  ymax=Prob.975,
                  colour=param_id),
              alpha=0.3)

