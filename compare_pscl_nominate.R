# Test file to show how package works

# Import latest senate data using pscl
require(pscl)
require(dplyr)
require(idealstan)
require(tidyr)
require(ggplot2)
require(bayesplot)
require(readr)
require(wnominate)

set.seed(84520)
#this is out of date, use new CSV Files
newdata <- readKH(file='senate_114.ord')

# let's see what pscl/dwnominate do with the data
estimate_it <- FALSE
# priors for pscl model to identify the model

cl <- constrain.legis(newdata,
                      x=list('SASSE (R NE)'=1,
                             'SANDERS (Indep VT)'=-1))

pscl_model <- ideal(newdata,store.item=TRUE,normalize = T)
  wnominate_model <- wnominate(newdata,polarity='SASSE (R NE)',dims=1,trials=10)
# we can compare models directly by forming an idealstan object out of pscl_model

pscl_pts <- data_frame(ideal_pts=apply(pscl_model$x,2,median)*-1,
                       high_pt=apply(pscl_model$x,2,quantile,probs=0.95)*-1,
                       low_pt=apply(pscl_model$x,2,quantile,probs=0.05)*-1,
                       model_type='CJR')
wnominate_pts <- data_frame(ideal_pts=wnominate_model$legislators$coord1D,
                            high_pt=wnominate_model$legislators$coord1D + 1.96*wnominate_model$legislators$se1D,
                            low_pt=wnominate_model$legislators$coord1D - 1.96*wnominate_model$legislators$se1D,
                            model_type='W-NOMINATE')

# now run idealstan
newdata <- readKH(file='senate_114.ord')
to_use <- newdata$votes[-1, ]
newdata$legis.data <- newdata$legis.data[-1, ]
to_use <- apply(to_use, 2, function(x) {
  y <- recode(
    x,
    `1` = 2L,
    `6` = 1L,
    `9` = 3L
  )
  return(y)
})

rownames(to_use) <- rownames(newdata$legis.data)

idealdata <-
  id_make(
    vote_data = to_use,
    legis_data = newdata$legis.data,
    abs_vote = 3,
    yes_vote = 2,
    no_vote = 1,
    ordinal = F,
    exclude_level = 7
  )
#,which(row.names(to_use)=='SANDERS (Indep VT)')
if(estimate_it==TRUE) {
  estimated_full <-
    id_estimate(idealdata = idealdata,
                model_type = 2,
                use_vb = F,
                ncores=4,
                nfix=2,
                restrict_type='constrain_twoway',
                restrict_params='legis',
                restrict_ind_high = c(which(row.names(to_use)=='SASSE (R NE)'),
                                      which(row.names(to_use)=='CRUZ (R TX)'),
                                      which(row.names(to_use)=='RUBIO (R FL)')),
                restrict_ind_low=c(which(row.names(to_use)=='SANDERS (Indep VT)'),
                                   which(row.names(to_use)=='REID (D NV)'),
                                   which(row.names(to_use)=='WARREN (D MA)')),
                auto_id=F,
                fixtype='constrained',
                #pin_vals=c(1),
                abs_discrim_sd = 5,
                reg_discrim_sd = 5,
                legis_sd = 1,
                diff_sd=5,
                seed=84520)
  saveRDS(estimated_full,'senate_104_absence_inf.rds')  
} else {
  estimated_full <- readRDS('senate_104_absence_inf.rds')
}

output <- rstan::extract(estimated_full@stan_samples)
abs_pts <- data_frame(ideal_pts=apply(output$L_full,3,median),
                         high_pt=apply(output$L_full,3,quantile,probs=0.95),
                         low_pt=apply(output$L_full,3,quantile,probs=0.05),
                         model_type='Absence-Inflated',
                      row_id=as.numeric(row.names(estimated_full@vote_data@vote_matrix))) %>% 
                        arrange(row_id) %>% 
  mutate(ranks=1)

all_perf <- bind_rows(slice(pscl_pts,-1),slice(wnominate_pts,-1),abs_pts) %>% 
  mutate(legislators=rep(row.names(wnominate_model$legislators)[-1],3)) %>% 
  group_by(model_type) %>% 
  mutate(ideal_pts_std=(ideal_pts/sd(ideal_pts))-mean(ideal_pts),
         high_pt_std=high_pt/sd(ideal_pts)-mean(ideal_pts),
         low_pt_std=low_pt/sd(ideal_pts)-mean(ideal_pts)) %>% 
  arrange(desc(ideal_pts)) %>% 
  mutate(ranks=1:n())

all_perf %>% 
  ggplot((aes(y=ideal_pts_std,ymin=low_pt_std,ymax=high_pt_std))) +
  geom_pointrange(aes(color=model_type,x=reorder(legislators,ideal_pts)),position=position_dodge(width=0.3),
                  size=0.15) + 
  coord_flip() + theme_minimal() +
  theme(panel.grid = element_blank()) +
  ylab('Ideal Point Scores (Liberal to Conservative)') +
  xlab('') +
  scale_colour_brewer(palette='Accent',name="")
  
ggsave('all_perf.png',width=7,height=10,scale=1.1,units='in')


# Identify those legislators who show biggest discrepancies.

big_diff <- group_by(all_perf,legislators) %>% arrange(legislators,model_type) %>% 
  mutate(total_diff=if_else(max(abs(ranks[2]-ranks[1]))>max(abs(ranks[3]-ranks[1])),
                            ranks[2]-ranks[1],ranks[3]-ranks[1])) %>% 
    ungroup() %>% 
           arrange(desc(abs(total_diff))) %>% 
           slice(1:30) %>% 
  group_by(legislators) %>% 
  mutate(labels=ifelse(sample(1:3,size = 1)==model_type,legislators,NA)) %>% 
  ungroup %>% 
         mutate(legislators=factor(legislators,levels=unique(legislators)),
                rank_labels=if_else(total_diff>0,paste0('Rank +',as.integer(total_diff)),
                                    paste0('Rank ',as.integer(total_diff))),
                rank_labels=if_else(model_type=='CJR',rank_labels,''))
          
big_diff %>% ggplot((aes(y=ideal_pts_std,ymin=low_pt_std,ymax=high_pt_std))) +
  geom_pointrange(aes(color=model_type,shape=model_type,x=factor(legislators)),fill=NA,position=position_dodge(width=.5),
                  size=0.5) + 
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text.x=element_text(angle=90,hjust=1)) +
  xlab('Positive Rank Changes Indicate More Conservative and Vice Versa') +
  ylab('') + 
  geom_text(aes(label=rank_labels,x=legislators,
                y=ideal_pts*2.5)) +
  scale_colour_brewer(palette='Set1',name="") +
  guides(shape='none')

ggsave('big_diff.png',width=10,height=5,units='in',scale=1.1)

# Now we want to look at the bill pts
require(readr)
bills_data <- read_csv('rollcall_senate_114.csv') %>% 
  mutate(vote_id=paste0('Vote_',1:n()))
bills <- as_data_frame(output$sigma_abs_full)
names(bills) <- paste0('Vote_',1:ncol(bills))

bills <- mutate(bills,iter=1:n()) %>% gather(bill_name,estimate,-iter) %>% 
  left_join(bills_data,by=c('bill_name'='vote_id')) %>% 
  group_by(bill_name) %>% 
  mutate(abs_ideal=median(estimate),
         high_ideal=quantile(estimate,.95),
         low_ideal=quantile(estimate,.05),
         high_nom=mid.dim1 + 1.96*spread.dim1,
         low_nom=mid.dim1 - 1.96*spread.dim2) 
# 
# bills %>% distinct(bill_name,.keep_all=T) %>% 
#   gather(model_type,midpoint,estimate,mid.dim1) %>%
#   ggplot(aes(y=midpoint,x=reorder(bill_name,midpoint),colour=model_type,shape=model_type)) +
#     geom_pointrange(aes(ymin=position=position_dodge(width=0.3),
#                     size=0.15) + 
#     coord_flip() + theme_minimal() +
#     theme(panel.grid = element_blank()) +
#     xlab('Ideal Point Scores (Liberal to Conservative)') +
#     ylab('') +
#     scale_colour_brewer(palette='Accent',name="")
require(stargazer)
bills %>% 
  ungroup %>% 
  distinct(bill_name,.keep_all=T) %>% 
  arrange(abs_ideal) %>% slice(1:10) %>% 
  dplyr::select(description,date) %>% 
  write_csv('most_cons_bills.csv')

bills %>% 
  ungroup %>% 
  distinct(bill_name,.keep_all=T) %>% 
  arrange(desc(abs_ideal)) %>% slice(1:10) %>% 
  dplyr::select(description,date) %>% 
  write_csv('most_lib_bills.csv')


# plot bills

highest_bill <- sort(bills$estimate,decreasing=T,index.return=T)
lowest_bill <- sort(bills$estimate,index.return=T)

id_plot(estimated_full,bill_plot=highest_bill$ix[1]) +
  scale_color_brewer(type='qual')
