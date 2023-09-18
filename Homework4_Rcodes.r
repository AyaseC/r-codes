#### R codes for Solution to Homework 4

library(DoE.base)
library(FrF2)

### Problem 1

hours = c(22,32,35,55,44,40,60,39,31,43,34,47,45,37,50,41,25,29,50,46,38,36,54,47)

hours.plan = fac.design(nfactors=3, nlevels=2, replication=3, randomize=F)
hours.plan = add.response(hours.plan, response=hours)
hours.fit1 = lm(hours.plan, degree=3)	

summary(hours.fit1)

(Effect.hours = hours.fit1$coef[-1] * 2)

MEPlot(hours.plan)

IAPlot(hours.plan)

anova(hours.fit1)

par(mfrow=c(1,2))
qqnorm(hours.fit1$res)
plot(hours.fit1$fitted, hours.fit1$res, xlab="fitted", ylab="residual", main="Residual vs Fitted value")



### Problem 2

cracklength = c(6.48,13.53,10.71,15.89,9.57,4.02,8.61,12.37,7.88,15.52,12.77,18.24,10.90,5.64,10.30,14.40,
5.87,14.00,11.12,16.39,9.34,3.77,8.52,11.89,8.24,15.69,12.57,18.07,11.35,5.43,10.06,13.85)

crack.plan = fac.design(factor.names=c("D","C","B","A"), nlevels=2, replication=2, randomize=F)
crack.plan = add.response(crack.plan, response=cracklength)
crack.fit1 = lm(crack.plan, degree=4)

summary(crack.fit1)

(Effect.crack = crack.fit1$coef[-1] * 2)

(Effect.crack[1] - Effect.crack[2])/sqrt(2*(4/32)*(summary(crack.fit1)$sigma)^2)
qt(0.975, df=16)



### Problem 3

rr = c(1.00, 1.05,1.21,1.22,1.11,1.19,1.26,1.32,1.16,1.20,1.30,1.35,1.30,1.31,1.40,1.44)/100
logit.rr = log(rr/(1-rr))

rr.plan = fac.design(nfactors=4, nlevels=2, randomize=F)
rr.plan = add.response(rr.plan, response=logit.rr)
rr.fit1 = lm(rr.plan, degree=4)

summary(rr.fit1)

(Effect.rr = rr.fit1$coef[-1] * 2)

DanielPlot(rr.fit1, half=T, alpha=0.05)

rr.fit2 = lm(rr.plan, degree=1)
anova(rr.fit2)



### Problem 4
### (iii)
100- 76.95/2 +67.52/2 + 7.84/2 +18.73/2 +51.32/2 + 11.69/2 + 9.78/2 + 20.78/2 + 14.74/2 + 1.27/2 +2.82/2 +6.50/2 -
10.20/2  +7.98/2  -6.25/2

100+ 76.95/2 -67.52/2 + 7.84/2 +18.73/2 +51.32/2 - 11.69/2 - 9.78/2 - 20.78/2 - 14.74/2 + 1.27/2 +2.82/2 +6.50/2 +
10.20/2  -7.98/2  -6.25/2

100+ 76.95/2 +67.52/2 - 7.84/2 -18.73/2 -51.32/2 + 11.69/2 + 9.78/2 -20.78/2 - 14.74/2 + 1.27/2 +2.82/2 +6.50/2 +
10.20/2  +7.98/2  +6.25/2
 


