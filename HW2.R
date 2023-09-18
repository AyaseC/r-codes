technique=c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4)
strength=c(31.29, 30, 28.65, 28.9, 32, 33, 29.75, 31.5, 28, 29, 29.85, 30.5, 26, 27, 26, 27.65)
library(DescTools)
technique=as.factor(technique)
noadj = pairwise.t.test(strength, technique, p.adj="none")
noadj$p.value #only 1&3 not rejected
bonf = pairwise.t.test(strength, technique, p.adj="bonferroni")
bonf$p.value #1&4, 2&4, 3&4 rejected
PostHocTest(aov(strength~technique), method = "hsd")
#1&4, 2&4, 3&4 rejected
#A t-test would just consider a single pair comparison each time, while bonferroni and tukey method are multiple pairwise comparison
#methods, the variance is estimated from the whole set of data as a pooled estimate. Therefore bonferroni and tukey are more reliable.

#Qn2
#i)
#Use a one-way layout with fixed effects, the response would be bioactivity of the drug, the factor would be brands.
#Some randomization would be randomly select in total 60 drugs from the four brands.
#ii)
pf(21.47/2.39,3,26) #Significance in the difference of the treatments
#iii)
1/sqrt(2)*qtukey(0.99,4,26)
t12 <- (66.1-65.75)/(sqrt(2.39)*sqrt(1/7+1/8))
t12
t13 <- (66.1-62.63)/(sqrt(2.39)*sqrt(1/7+1/9))
t13
t14 <- (66.1-63.85)/(sqrt(2.39)*sqrt(1/7+1/6))
t14
t23 <- (65.75-62.63)/(sqrt(2.39)*sqrt(1/8+1/9))
t23
t24 <- (65.75-63.85)/(sqrt(2.39)*sqrt(1/8+1/6))
t24
t34 <- (62.63-63.85)/(sqrt(2.39)*sqrt(1/9+1/6))
t34
#Hence we reject pairs 1&3, 2&3
#iv)
qt(0.01/2,26,lower.tail = FALSE)
c <- 0.5*(66.1+65.75)-0.5*(62.63+63.85)
c
sec <- sqrt(2.39)*sqrt(1/4/7+1/4/8+1/4/9+1/4/6)
sec
Tc <- c/sec
Tc
#Thus we reject the null hypothesis, there is significant difference in brand-name and generic drugs
#v)
sqrt(qf(0.99,1,13))
#>T12,T34
sqrt(qf(0.99,1,14))
#<T13
sqrt(qf(0.99,1,11))
#>T14
sqrt(qf(0.99,1,15))
#<T23
sqrt(qf(0.99,1,12))
#>
#we reject 1&3, 2&3
sqrt(3*qf(0.99,3,26))
#we still reject part iv
#vi)
#Pairs 1&3, 2&3 are significant.

#4
packing.data = read.table("PackingMachine.txt", header=TRUE)
X = packing.data$Machine
X_fac = as.factor(X)
Y = packing.data$Weight
fit.ms = lm(Y~X_fac)

#i)
summary(fit.ms)
anova(fit.ms)
#Very small p-value indicating there is a significant difference in the machine used.

#ii)
sigma2_e <- 0.2659
sigma2_a <- (15.8262-0.2659)/20

#iii)
mu <- mean(Y)
mu_upper <- mu+qt(0.025,2,lower.tail = FALSE)*sqrt(15.8262/60)
mu_lower <- mu+qt(0.025,2,lower.tail = TRUE)*sqrt(15.8262/60)
#95 C.I for mean is [47.89172,52.31128]

#iv)
sigma2_e_lower <- 15.159/qchisq(0.025,57,lower.tail = FALSE)
sigma2_e_upper <- 15.159/qchisq(0.025,57)
#95 C.I for sigma2_e is [0.1900763,0.3986405]

#v)
L <- 1/3*(59.508*1/qf(0.025,2,57,lower.tail = FALSE)-1)
U <- 1/3*(59.508*1/qf(0.025,2,57)-1)
L/(1+L)
U/(1+U)
#95 C.I for the intraclass correlation coefficient is [0.8246664,0.9987242]