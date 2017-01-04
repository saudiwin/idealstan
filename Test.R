# Test file to show how package works

# Import latest senate data using pscl
require(pscl)
require(dplyr)
require(idealstan)

newdata <- readKH(file=url('http://amypond.sscnet.ucla.edu/rollcall/static/S114.ord'))

to_use <- newdata$votes 
to_use <- apply(to_use,2,function(x) {
  y <- recode(x,`1`=3L,`6`=1L,`7`=2L,`9`=4L)
  return(y)
})

all_vals <- table(to_use)

idealdata <- make_idealdata(vote_data=to_use,legis_data=newdata$legis.data,votes=as.character(names(all_vals[1:3])),
                           abs_vote = '4')

estimated2 <- estimate_ideal(idealdata=thisdata,use_subset = FALSE,sample_it=TRUE,ncores = 4,
                            use_vb = TRUE)

num_legis <- nrow(idealdata@vote_matrix)
num_bills <- ncol(idealdata@vote_matrix)
legispoints <- rep(1:num_legis,times=num_bills)
billpoints <- rep(1:num_bills,each=num_legis)
avg_particip <- apply(idealdata@vote_matrix,1,function(x) {
  count_abs <- sum(x==idealdata@vote_count)
  particip_rate <- 1 - (count_abs/length(x))
  return(particip_rate)
}) 
avg_particip <- scale(avg_particip)[,1]
Y <- c(idealdata@vote_matrix)


this_data <- list(N=length(Y),
                  Y=Y,
                  num_legis=num_legis,
                  num_bills=num_bills,
                  ll=legispoints,
                  bb=billpoints,
                  particip=avg_particip)

post_modes <- vb(object=stan_model(file='exec/ordinal_split_absence_nofix.stan'),data =this_data,
                 algorithm='meanfield')
lookat <- summary(post_modes)[[1]]
