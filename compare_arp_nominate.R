# compare performance of DW-NOMINATE and IDEAL


# NOMINATE and IDEAL ------------------------------------------------------


require(wnominate)
require(pscl)




# ARP Data ----------------------------------------------------------------
require(forcats)
require(rstan)
require(bawsala)
require(bayesplot)
require(tidyr)
require(dplyr)
require(archivist)

set.seed(84520)

#CONTROL PANEL

# whether to estimate the model or just used the saved version

estimate_it <- T

# Keep legislators with have voted on at least this many bills
keep_legis <- 1
# Use only the parties in the subset_party variable?
use_subset <- FALSE
subset_party <- c("Bloc Al Horra","Mouvement Nidaa Tounes",'Front Populaire')
# Check out partial credit IRT
categorical <- FALSE

# What type of identification to use
identify <- 'ref_discrim'

# Which of the legislatures to use-- ARP or ANC
use_both <- FALSE
legislature <- "arp_votes"
# Use variational inference? Faster, but less accurate
use_vb <- FALSE
# Convert absences to a separate category in ordinal regression?
use_nas <- TRUE
# Split absences by whether absences are for/against party votes? (note: indicates the use of a different ordinal model)
split_absences <- TRUE
#Reference bill for absences constraint
ref_absence <- 'Bill_3977'
# Which dataset to use? Put 1 for binary, 2 for abstain, 3 for ordinal
to_run <- 3
# Use only a sample of bills/legislators?
sample_it <- FALSE
# Legislator to use as a reference for IRT model
legislator<- "Bochra Belhaj Hamida"

cleaned <- clean_data(keep_legis=keep_legis,use_subset=use_subset,subset_party=subset_party,
                      use_both=use_both,refleg=legislator,
                      legis=1,use_vb=use_vb,use_nas=use_nas,to_run=to_run,sample_it=sample_it)

vote_matrix <- dplyr::select(cleaned$arp_votes,matches('Bill')) %>% as.matrix
row.names(vote_matrix) <- cleaned$arp_votes$legis.names
arp_ideal_data <- id_make(vote_data = vote_matrix,
                                  legis_data=data_frame(cleaned$arp_votes$bloc),
                                 abs_vote=4L)

ggplot(data_frame(x=as.character(c(arp_ideal_data@vote_matrix))),aes(x=x)) +
  stat_count(width=0.5) + theme_minimal() +
  theme(panel.grid=element_blank()) +
  xlab("") +
  ylab("") + 
  scale_x_discrete(breaks=c("1","2","3","4"),labels=c('No','Abstain','Yes','Absent'))
ggsave('arp_hist.png') 
if(estimate_it==TRUE) {
  estimate_arp <- id_estimate(arp_ideal_data,
                              model_type=4,
                              abs_discrim_sd = 5,
                              reg_discrim_sd = 5,
                              legis_sd = 5,
                              diff_sd=5,
                              nfix=10,
                              use_vb = T,
                              restrict_type='constrain_twoway',
                              fixtype='vb',restrict_params = 'legis',
                              seed=84520)
  saveRDS(estimate_arp,file = 'estimate_arp.rds')
} else {
  estimate_arp <- readRDS('estimate_arp.rds')
}

output <- rstan::extract(estimate_arp@stan_samples)

abs_pts <- data_frame(ideal_pts=apply(output$L_full,3,median),
                      high_pt=apply(output$L_full,3,quantile,probs=0.95),
                      low_pt=apply(output$L_full,3,quantile,probs=0.05),
                      model_type='Absence-Inflated',
                      row_id=as.numeric(row.names(estimate_arp@vote_data@vote_matrix))) %>% 
  arrange(row_id) %>% 
  mutate(ranks=1)

newdata <- rollcall(arp_ideal_data@vote_matrix,yea=1,nay=3,notInLegis = c(2,4),
                    legis.names=arp_ideal_data@legis_data$legis.names)

wnominate_model <- wnominate(newdata,
                             polarity=arp_ideal_data@legis_data$legis.names[abs_pts$ideal_pts==max(abs_pts$ideal_pts)],
                             dims=1,trials=10)

wnominate_pts <- data_frame(ideal_pts=wnominate_model$legislators$coord1D,
                            high_pt=wnominate_model$legislators$coord1D + 1.96*wnominate_model$legislators$se1D,
                            low_pt=wnominate_model$legislators$coord1D - 1.96*wnominate_model$legislators$se1D,
                            model_type='W-NOMINATE')

# run nominate

wnominate_pts <- data_frame(ideal_pts=wnominate_model$legislators$coord1D,
                            high_pt=wnominate_model$legislators$coord1D + 1.96*wnominate_model$legislators$se1D,
                            low_pt=wnominate_model$legislators$coord1D - 1.96*wnominate_model$legislators$se1D,
                            model_type='W-NOMINATE')

legis_data <- mutate(arp_ideal_data@legis_data,
                     party=factor(`cleaned$arp_votes$bloc`,levels=c('Aucun bloc',
                                                                    "Afek Tounes, le mouvement national et l'appel des tunisiens à l'étranger",
                                                                    'Bloc Social-Démocrate',
                                                                    'Front Populaire',
                                                                    'Mouvement Ennahdha',
                                                                    'Mouvement Nidaa Tounes',
                                                                    'Bloc Al Horra',
                                                                    'Union Patriotique Libre'),
                                  labels=c('None','AF','SD','FP','EN','NT','HO','UPL')),
                     party=fct_recode(party,
                                      O='None',
                                      G='AF',
                                      O='SD',
                                      O='FP',
                                      N='EN',
                                      T='NT',
                                      G='HO',
                                      G='UPL') %>% fct_relevel('T','N','G','O')) 


all_perf <- bind_rows(wnominate_pts,abs_pts) %>% 
  mutate(party=rep(legis_data$party,2),
         legislators=rep(legis_data$legis.names,2)) %>% 
  group_by(model_type) %>% 
  mutate(ideal_pts_std=ideal_pts-mean(ideal_pts),
         high_pt_std=high_pt-mean(ideal_pts),
         low_pt_std=low_pt-mean(ideal_pts)) %>% 
  arrange(desc(ideal_pts)) %>% 
  mutate(ranks=1:n())


# create a custom palette

party_palette <- c('T'="#0083C4",'N'="#00A2BF",'G'="#00ADBF",'O'="#00BF7F")
party_palette2 <- c('T'='#762a83','N'='#af8dc3','G'='#e7d4e8','O'='#1b7837')


all_perf %>% 
  ggplot((aes(y=ideal_pts_std,ymin=low_pt_std,ymax=high_pt_std))) +
  geom_pointrange(aes(color=model_type,shape=party,x=reorder(legislators,ideal_pts)),position=position_dodge(width=0.3),
                  size=0.15) + 
  coord_flip() + theme_minimal() +
  theme(panel.grid = element_blank()) +
  ylab('Ideal Point Scores (Liberal to Conservative)') +
  xlab('') +
  # scale_color_manual(values=party_palette2,
  # labels=c('Nidaa Tounes','Nahda','Other\nGoverning\nParty\n','Opposition')) +
  theme(legend.position = 'bottom',
        axis.text.y =element_blank()) +
  scale_colour_brewer(palette='Accent',name="") +
  scale_shape(name="Bloc")

ggsave('tunisia_arp_compare.png',width=10,height=7,scale=1.1,units='in')

