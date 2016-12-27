# Test file to show how package works

# Import latest senate data using pscl
require(pscl)
require(dplyr)

newdata <- readKH(file=url('http://amypond.sscnet.ucla.edu/rollcall/static/S114.ord'))

to_use <- newdata$votes 
to_use <- apply(to_use,2,function(x) {
  y <- recode(x,`1`=3L,`6`=1L,`7`=2L,`9`=4L)
  return(y)
})

all_vals <- table(to_use)

thisdata <- make_idealdata(vote_data=to_use,legis_data=newdata$legis.data,votes=as.character(names(all_vals[1:3])),
                           abs_vote = '4')

estimated <- estimate_ideal(idealdata=thisdata,use_subset = FALSE,sample_it=TRUE,ncores = 4)