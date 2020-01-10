# Ex. 1.7 Analysis of hazard rates, ratios and differences

## Clean my environment
rm(list = ls())

## load packages 
library(Epi)
options(digits = 4) # cut down decimal points in output

## Hand Calculations for a single rate
# Two approaches: 
# 1. Calculate the empirical incidence rate (lambda = D/Y), Standard Error of the empirical rate 
#    (SE = lambda/sqrt(D)) and CI = 1.96*SE
# 2. Calculate the empirical log rate log(lambda) = log(D/Y), SE = 1/sqrt(D), CI = 1.96*SE

# 1. Approach 
# Number of events (death)
D <- 15
# Sum time of follow up in thousands of years
Y <- 5.532
# empirical rate
rate <- D/Y
# Standard error on empirical rate 
SE.rate <- rate/sqrt(D)
# print, rate, se, ci
c(rate,SE.rate,c(-1.96,1.96)*SE.rate)

# 2. Approach 
# Log rate 
theta <- log(rate) 
# Se on the log rate 
SE.theta <- 1/sqrt(D)
# print log rate and se of log rate 
c(theta, SE.theta)
# EF 
EF <- exp(1.96*SE.theta)
# compare 
c(rate,SE.rate,theta,EF,rate/EF,rate*EF)

## Poisson Model for a single rate with logarithmic link

# 1. Simple case
# Response variable: Number of events D 
# Offset term: logarithm of person-years log(Y)
help(glm)
# models 
m <- glm(formula = D ~ 1, family = poisson(link = log), offset = log(Y))
m2 <- glm(formula = (D/Y) ~ 1, family = poisson(link = log)) # equivalent 
# summary
summary(m)
summary(m2) # Note estimate of the intercept corresponds to the log(rate) calculated earlier
# extract confidence intervals of the log estimate rate 
ci.lin(m)
# estimate hazard rate
ci.exp(m)
# confidence intervals of the estimated hazard rate itself 
ci.lin(m, Exp = T)

# 2. Other way of doing it 
# Response: matrix with two columns D and Y
# No offset needed 
# recommended as it allows to have different link functions than the log function

mreg <- glm(cbind(D,Y) ~ 1, family = poisreg(link=log))
ci.exp(mreg)

## Poisson Model for a single rate with identity link 

# model
mid <- glm( cbind(D,Y) ~ 1, family = poisreg(link = identity) )
# confidence intervals of the hazard rate itself here because the link function is identity
ci.lin(mid)
# Compare with empirical rate and CI
c(rate, rate + SE.rate*c(-1.96,1.96))


## Poisson model assuming the same rate for several periods

# Events and person years collected over different periods
# Data 
Dx <- c(3,7,5) # Number of events for each period
Yx <- c(1.412,2.783,1.337) # Person-Years for each period
Px <- 1:3 # 3 periods
rates <- Dx/Yx # empirical rates
rates

D <- sum(Dx) # same as before
Y <- sum(Y) # same as before

# Log link 
m3 <- glm(cbind(Dx,Yx) ~ 1, family = poisreg(link = log))
ci.exp(m3)

# test whether the incidence rate ius the same within each period 
m4 <- glm(cbind(Dx,Yx) ~ factor(Px), family = poisreg(link = log))
# intercept corresponds to the incidence rate for period 1
# rate ratios for period 2 and 3 given afterwards 
ci.exp(m4)
# Goodness of fit 
anova(m3, m4, test = "Chisq")

## Analysis of rate ratios 
# Comparison of two rates i.e. hazard ratios in an exposed group lambda_1 vs unexposed group lambda_0

# Empirical incidence rate ratio
# Estimate of rho = lambda_1/lambda_0 is (D1/Y1) / (D0/Y0)
# Estimate of var of log(rho) is 1/D1 + 1/D0

# Number of events
D0 <- 15
D1 <- 28
# Person Years
Y0 <- 5.532
Y1 <- 4.784
# Rates
R1 <- D1/Y1
R0 <- D0/Y0
# Empirical Rate Ratio & SE
RR <- R1/R0
SE.RR <- sqrt(1/D0 + 1/D1)
EF <- exp(1.96*sqrt(SE.RR))
# print
c(R1,R0,RR,RR/EF,RR*EF)

# Poisson Model 
D <- c(D0,D1)
Y <- c(Y0,Y1)
expos <- 0:1
mm <- glm(cbind(D,Y) ~ factor(expos), family = poisreg(link = log))
# RR and ci 
# Intercept corresponds to the estimate R0, and then we have the estimated RR
ci.exp(mm)


## Analysis of rate differences 
# Empirical incidence rate difference is D1/Y1 - D0/Y0
# its var is D1/Y1^2 + D0/Y0^2
# Rate diff
RD <- D1/Y1 - D0/Y0 
SED <- sqrt(D1/Y1^2 + D0/Y0^2)
c(R1,R0,RD,SED,RD + c(-1,1)*1.96*SED)

# model 
ma <- glm(cbind(D,Y) ~ factor(expos), family = poisreg(link = identity))
ci.lin(ma)[,c(1,5,6)]
