# Generate some representative examples of ideal points

require(dplyr)
require(tidyr)
require(ggplot2)

#standard ideal pt plot
data_frame(x=c(-5,5)) %>% 
  ggplot(aes(x=x)) +
  theme_minimal() +
  theme(panel.grid=element_blank(),
        text=element_text(family='Times')) +
  ylab('Utility') +
  xlab('Ideal Points') +
  stat_function(fun=dnorm,args=list(mean=1),linetype=1,size=1) +
  geom_vline(xintercept = 3,linetype=3) +
    geom_vline(xintercept = -3,linetype=3) +
  geom_vline(xintercept=0,linetype=4) +
  annotate('text',x=c(-3.5,-.8,1,3.5),
           y=c(0.2,0.3,0.05,0.2),
           label=c('No',
                   'Indifference',
                   'italic(x_i)',
                   'Yes'),
           family='Times',
           parse=T)

ggsave('standard_ideal_pt.png',scale=0.7)

#add in abstention curve

data_frame(x=c(-5,5)) %>% 
  ggplot(aes(x=x)) +
  theme_minimal() +
  theme(panel.grid=element_blank(),
        text=element_text(family='Times')) +
  ylab('Utility') +
  xlab('Ideal Points') +
  stat_function(fun=dnorm,args=list(mean=1),linetype=1,size=1) +
  geom_vline(xintercept = 3,linetype=3) +
  geom_vline(xintercept = -3,linetype=3) +
  geom_vline(xintercept=0.1,linetype=3) +
  geom_vline(xintercept=(-3+.1)/2,linetype=4) +
  geom_vline(xintercept=(3-.1)/2,linetype=4) +
  annotate('text',x=c(-3.5,-.6+(-3+.1)/2,.5,.6+(3-.1)/2,1,3.5),
           y=c(0.2,0.39,.16,0.39,0.05,0.2),
           label=c('No',
                   'Indifference',
                   'Abstain',
                   'Indifference',
                   'italic(x_i)',
                   'Yes'),
           family='Times',
           parse=T) + 
  annotate('segment',x=.1+(-3+.1)/2,y=.15,xend=-.1+(3-.1)/2,yend=.15,
           arrow=arrow(length = unit(0.01, "npc"))) +
  annotate('segment',x=-.1+(3-.1)/2,y=.15,xend=.1+(-3+.1)/2,yend=.15,
           arrow=arrow(length = unit(0.01, "npc")))

ggsave('ideal_pt_with_abstain.png',scale=0.7)
  

