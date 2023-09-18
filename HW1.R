#3
#i)
x=c(1,2,3,4,5,6,7,8,9,10,11,12)
y=c(16,35,45,65,86,96,106,124,134,156,164,182)
plot(x,y)
#ii)
model <- lm(y~x)
summary(model)
#The R-squared shows the scattered points fit the regression line well, 
#hence the model is appropriate for analysing data
#iv)
anova(model)
#v)
par(mfrow=c(1,2))
qqnorm(model$resid,main="Normal Q-Q Plot for Residuals")
qqline(model$resid)
plot(model$resid ~ model$fitted, main="Residual vsFitted Values",xlab="fitted values",ylab="residuals")
abline(h=0)
#normally distributed, the plot shows an even spread means the current model is suitable.
#vi)
confint(model,level=0.95)
#vii)
data=data.frame(x=8.5)
predict(model, data, interval="confidence", level=0.95)
predict(model, data, interval="predict", level=0.95)

#4
#i)
technique=c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4)
strength=c(31.29, 30, 28.65, 28.9, 32, 33, 29.75, 31.5, 28, 29, 29.85, 30.5, 26, 27, 26, 27.65)
library(DescTools)
technique=as.factor(technique)
TukeyHSD(aov(strength~technique), ordered = TRUE, conf.level = 0.95)
PostHocTest(aov(strength~technique), method = "hsd")
#We reject pairs 1-4, 2-4, 3-4

summary(aov(strength~technique))
qt(p=.05, df=12, lower.tail=FALSE)

#iii)
library(Rmisc)
T1=c(31.29, 30, 28.65, 28.9)
CI(T1, ci=0.95)
#iv)
CI(strength, ci=0.95)
#v)
model2 <- lm(strength~technique)
par(mfrow=c(1,2))
qqnorm(model2$resid,main="Normal Q-Q Plot for Residuals")
qqline(model2$resid)
#normality assumption is valid
#vi)
plot(model2$resid ~ model2$fitted, main="Residual vsFitted Values",xlab="fitted values",ylab="residuals")
abline(h=0)
#the plot shows an even spread means the current model is suitable.