




results=read.csv("results.csv")
head(results)

results$total_goals=results$home_goals+results$away_goals
hist(results$total_goals,main="Distribution of total goals",xlab = "total goals")

y=matrix(0,nrow=12,ncol=380)

for (i in 1:12){
  y[i,]=results$total[(i*380-379):(380*i)]
}
ymean=rep(0,12)
for (i in 1:12){
  ymean[i]=mean(y[i,])
}


nj=rep(380,12)


gibbs.hier=function(ymean,nj,m,KB,a,b,c,seed){
  
  lambda=matrix(nrow=KB,ncol=m,data=0)
  mu=rep(0,times=KB)
  set.seed(seed)
  lambda[1,]=ymean/mean(y)
  mu[1]=mean(y)
  
  
  for(k in 2:KB){
    
    #updating lambda
    for (i in 1:m){
      lambda[k,i]=rgamma(1,sum(y[i,])+c-1,nj*mu[k-1]+c)
    }
    
    #updating mu
    mu[k]=rgamma(1,sum(y)+a-1,sum(lambda[k,]*nj)+b)
    
  }
  
  list(lambda=lambda,mu=mu)
}


result=gibbs.hier(ymean,nj,12,12000,sum(y),sum(nj),1,4234)



par(mfrow=c(3,4))
for (i in 1:12){
  hist(y[i,],main = paste(2005+i,2006+i,sep="-"),xlab = "goals")
}


library(coda)
result.lambda=mcmc(result$lambda[2001:12000,])
result.mu=mcmc(result$mu[2001:12000])


for (i in 1:12){
  hist(rpois(380,mean(result.mu)*mean(result.lambda[,i])),main=paste("Predicted",paste(2005+i,2006+i,sep="-")),xlab="goals")
}



plot(result.lambda)

plot(result.mu)


acf(result.lambda)

acf(result.mu)

summary(result.lambda)
summary(result.mu)

posterior_lambda=rep(0,12)
for (i in 1:12){
  posterior_lambda[i]=mean(result.lambda[,i])
}
posterior_mean_y=posterior_lambda*mean(result.mu)
overall_posterior_mean_y=mean(result.lambda)*mean(result.mu)
ymean
posterior_mean_y     
overall_posterior_mean_y
