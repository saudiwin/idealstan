##### Figure 1, Cook-Gelman-Rubin (CGR) test for a Bayesian Linear Regression#########3
library(rstan)
library(loo)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
stan_code='
data {
int <lower=0> N;
int <lower=0> D;
matrix [N,D] x ;
vector [N] y;
}
parameters {
vector [D] b;
real <lower=0> sigma;
}
model {
y ~ normal(x * b, sigma);
b~normal(0,1);
sigma~gamma(0.5,0.5);
}
'
set.seed(1000)
N=1000  # a linear regression with 10^5 data and 100 variables
D=10
beta=rnorm(D,0,1)
x=matrix(rnorm(N*D,0,1), N, D)
#y=as.vector(x%*%beta+rnorm(N,0,2))

m=stan_model(model_code = stan_code)

M=100
set.seed(1000)
b_0=matrix(rnorm(M*D,0,1), D,M)
sigma_0=rgamma(M,0.5,0.5)
y_mean= x%*%b_0
prob=matrix(NA,M,(D+1))
time_flag=proc.time()
for(i in 15:M){
  y_sim=as.vector( y_mean[,i]+rnorm(N,0,sigma_0[i])  )
  fit_vb=vb( m, data=list(x=x,y=y_sim, D=D,N=N),iter=2e4,output_samples=100,tol_rel_obj=0.0001,eta=0.05,adapt_engaged=F)
  vb_samples=extract(fit_vb)
  b_sim=vb_samples$b
  sigma_sim=vb_samples$sigma
  b_mean=apply(b_sim, 2, mean)
  b_sd=apply(b_sim, 2, sd)
  prob[i,1:D]=pnorm(b_0[,i], b_mean,b_sd)
  prob[i,1+D]=pnorm(log(sigma_0[i]), mean( log(sigma_sim)  ),sd(log(sigma_sim)))
  print(paste("===========================iter =", i,    "============================"))
  if(i%%10==0)
  {
    time_elp=c(proc.time()-time_flag)[3]
    print(paste("===========================elapse time =", time_elp,    "============================"))
    print(paste("===========================estimated remaining =", round(time_elp/i*(M-i)/3600 ,2),    "hours============================"))
    
  }
}
#save(prob, file="cgr_liner_10000.RData")
pdf("linear_reg_cgr_2.pdf",height=1, width=4)
par(mfrow=c(1,3),oma=c(0.9,1,0.2,0), pty='m',mar=c(0.5,0.4,0.5,0) ,mgp=c(1.5,0.25,0), lwd=0.5,tck=-0.01, cex.axis=0.6, cex.lab=0.9, cex.main=0.9)
for(i in  c(1,2,11))
{
  hist(prob[,i],breaks = 20,axes=F,xlim=c(0,1),xlab="", ylab="",main = "",ylim=c(0,6.5),probability = T,xpd=T)
  abline(h=1,lwd=0.3,lty=2,col='grey')
  
  lines(x=c(0.5,0.5),y=c(-0.5,1.5),col='red',lwd=1.4)
  axis(1, padj=-1,lwd=0.5, at=c(0,0.5,1), labels = c(0,0.5,1))
  if(i==1)
    axis(2, at=c(0,2,4,6),lwd=0.5, las=2)
  xxx1= (prob[,i])
  xxx2= (1-prob[,i])
  ksTest=ks.test(xxx1,xxx2)
  if (i==1)
    mtext(3, text=paste("KS-test p=",  round(ksTest$p.value,digits=2 ) ), cex=0.7,line=-1  )
  if (i==2)
    mtext(3, text=paste(" p=",  round(ksTest$p.value,digits=2 ) ), cex=0.7,line=-1  )
  if (i==11)
    #mtext(3, text=paste(" p= 0.00",  round(ksTest$p.value,digits=2 ) ), cex=0.7,line=-1  )
    mtext(3,  text=paste("KS-test p=",  round(ksTest$p.value,digits=2 ) ), cex=0.7,line=-1  )
  
  if(i==1)
  {
    mtext(2, text="density", cex=0.5,line=0.7 )
    mtext(3, text=expression(beta[1]), cex=0.7,line=-.3 )
    mtext(1, text=expression(~p[beta   [paste(1,":")]]), cex=0.5,line=0.5 )
  }
  if(i==2){
    mtext(1, text=expression(~p[beta[paste(2,":")]]), cex=0.5,line=0.5 )
    mtext(3, text=expression(beta[2]), cex=0.7,line=-.3 )
  }
  if(i==101){
    mtext(1, text=expression(~p[sigma[":"]]), cex=0.5,line=0.5 )
    mtext(3, text=expression(log~sigma), cex=0.7,line=-.3 )
  }
}
dev.off()


