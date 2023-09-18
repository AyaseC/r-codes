install.packages("DoE.base")
install.packages("FrF2")
library(DoE.base)
library(FrF2)

life.time = c(22,32,35,55,44,40,60,39,31,43,34,47,45,37,50,41,25,29,50,46,38,36,54,47)

life.plan = fac.design(factor.names=c("a","b","c"), nlevels=2, replications=3, randomize=F)

life.plan = add.response(life.plan, response=life.time)


life.fit1 = lm(life.plan, degree=3)	




MEPlot(life.plan)


IAPlot(life.plan)

#a)
#Main Effects:
A=(39+41+47+55+47+46+40+37+36+32+43+29)/12-(22+31+25+35+34+50+44+45+38+60+50+54)/12
A #0.3333333
B=(39+41+47+55+47+46+60+50+54+35+34+50)/12-(22+31+25+32+43+29+44+45+38+40+37+36)/12
B #11.33333
C=(39+41+47+60+50+54+40+37+36+44+45+38)/12-(22+31+25+32+43+29+35+34+50+55+47+46)/12
C #6.833333
#Interaction effect
AB=(39+41+47+55+47+46-40-37-36-60-50-54-32-43-29-35-34-50+44+45+38+22+31+25)/12
AB #-1.666667
AC=(39+41+47+40+37+36-55-47-46-60-50-54-32-43-29-44-45-38+35+34+50+22+31+25)/12
AC #-8.833333
BC=(39+41+47+60+50+54-55-47-46-40-37-36-35-34-50+32+43+29-44-45-38+22+31+25)/12
BC #-2.833333
ABC=(39+41+47-60-50-54-55-47-46-40-37-36+35+34+50+32+43+29+44+45+38-22-31-25)/12
ABC #-2.166667
life.fit1$coef[-1] * 2
#Main effect B and C, interaction effect AC appear to be large.

#b)
summary(life.fit1)
anova(life.fit1)

#c)
# ??y=40.8333+0.1667A+5.6667B+3.4167C-0.8333AB-4.4167AC-1.4167BC-1.0833ABC

#d)
par(mfrow=c(1,2))
qqnorm(life.fit1$res, cex.lab=1.7, pch=19, cex.main=1.7, main="Normal Q-Q plot of residuals")
qqline(life.fit1$res)
plot(life.fit1$fitted , life.fit1$res, pch=19, cex.lab=1.7, cex.main=1.7, xlab="fited value", ylab="residual", main="residual vs fitted value")
abline(h=0)
#There's no obvious problem.

#e)
life.fit2 = lm(life.time ~ b + c + a*c, data=life.plan)
anova(life.fit2)
summary(life.fit2)
MEPlot(life.fit2)
IAPlot(life.fit2)
#The new fitted model is ??y=40.8333+5.6667B+3.4167C-4.4167AC
#To maximize y we choose a,b,c be {-1,1,1}


###########Question2#############
crack.length = c(6.48,13.53,10.71,15.89,9.57,4.02,8.61,12.37,
                 7.88,15.52,12.77,18.24,10.90,5.64,10.30,14.40,
                 5.87,14.00,11.12,16.39,9.34,3.77,8.52,11.89,
                 8.24,15.69,12.57,18.07,11.35,5.43,10.06,13.85)

crack.plan = fac.design( factor.names=c("D","C","B","A"), nlevels=2, replications=2, randomize=F)

crack.plan = add.response(crack.plan, response=crack.length)

crack.fit1 = lm(crack.plan, degree=4)	
#a)
MEPlot(crack.plan)

IAPlot(crack.plan)

anova(crack.fit1)

summary(crack.fit1)

#Main effects A,B,C,D are significant, interaction effects CD, BD, BCD are significant.

#b)
#??y=11.030937+1.387813D+1.829063C-1.654687B+0.900938A+0.889687CD-1.842813BD+1.442813BCD
# To minimize y we choose A,B,C,D be {-1,1,-1,1}

#c)t-statistic=13.465
pt(13.465,16,lower.tail = FALSE)
#1.906161e-10, C and D have different main effect.



##################Question3##############
response.rate = c(1.00,1.05,1.21,1.22,1.11,1.19,1.26,1.32,1.16,1.20,1.30,1.35,1.30,1.31,1.40,1.44)/100

response.plan = fac.design(factor.names=c("A","B","C","D"), nlevels=2, replications=1, randomize=F)

response.plan = add.response(response.plan, response=response.rate)

response.fit1 = lm(response.plan, degree=4)	

effects.response=response.fit1$coef[-1]*2

MEPlot(response.plan)

IAPlot(response.plan)

#i)
#The signs of main effects of the factors are positive.
#ii)
ln.rate=log(response.rate/(1-response.rate))

response.plan2=add.response(response.plan, response=ln.rate)

response.fit2 = lm(response.plan2, degree=4)	

summary(response.fit2)
  
MEPlot(response.plan2)

IAPlot(response.plan2)



response.fit2$coef[-1]*2
#A=0.0425, B=0.1475, C=0.105, D=0.1375, AB=-0.0025, AC=0.005, AD=0.0075, BC=-0.02, BD=-0.0175, CD=0.005
#The signs of main effects are positive which agree with i).

#iii)
halfnormal(response.fit1)
#A, B, C, D are significant
ME.Lenth(effects.response, alpha=0.05)

effects.response
#The effects exceeding ME values are A, B, C, D hence A, B, C, D are significant.

#iv)
#??y=1.239+0.02125A+0.07375B+0.0525C+0.06875D
1.239+0.02125+0.02125+0.0525+0.06875 #1.40275 smaller than 1.44

#v)
response.fit3 = lm(response.rate ~ A + B + C + D, data=response.plan)
anova(response.fit3)
#Residual sum of squares is 0.005
summary(response.fit1)
((-1.250e-03)^2+2.500e-03^2+(-3.750e-03)^2+(-1.000e-02)^2+(-8.750e-03)^2+2.500e-03^2+2.500e-03^2+6.250e-03^2+(-7.500e-03)^2+2.500e-03^2+(-2.819e-18)^2)*16

#vi)
anova(response.fit3)
#A, B, C, D are significant at the level of 0.05.