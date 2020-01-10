## Exercise 1.14 Causal Inference 

## Clean my environment
rm(list = ls())

## load packages 
library(Epi)
library(survival)
library(dplyr)

## Generate data 
bdat = data.frame(sex = c(rep(0,500), rep(1,500))) # df with 500 F (0) and 500 M (1)
bdat$beer <- rbinom(1000, 1, 0.20 + 0.5*bdat$sex) # if F, prob(beer) = 0.2, if M, prob(M) = 0.7 
bdat$weight <- 60 + 10*bdat$sex + rnorm(1000,0,7) # F weigh on average 60kg, M weigh 10kg more than F on average
                                                  # no effect of beer on weight
bdat$bp <- 110 + 0.5*bdat$weight + 10*bdat$beer + rnorm(1000,0,10) # on average Blood pressure is 110, 
                                                                   # 0.5 increase/ kilo
                                                                   # increase of 10 if beer consumption
                                                                    

## look at the data 
str(bdat)
## Model 
# Linear Regression with Y = body weight and X = beer drinking, unadjusted 
lm(formula = weight ~ beer, data = bdat )
# strong effect of beer + 4
# adjusted for sex
lm(formula = weight ~ beer + sex, data = bdat )
# effect of beer now -1, strong effect for sex + 10 
# adjusted for sex and blood pressure 
lm(formula = weight ~ beer + sex + bp, data = bdat )
# find an effect from beer on weight -> not supposed to exist that's because we adjust for bp 


## Change body weight so that it depends on beer drinking 
bdat$weight <- 60 + 10*bdat$sex + 2*bdat$beer + rnorm(1000,0,7)
## New Models 
# Linear Regression with Y = body weight and X = beer drinking, unadjusted 
mod1 <- lm(formula = weight ~ beer, data = bdat )
summary(mod1)
# Adjusted for sex
mod2 <- lm(formula = weight ~ beer + sex, data = bdat )
summary(mod2)
# adjusted for sex and blood pressure 
mod3 <- lm(formula = weight ~ beer + sex + bp, data = bdat )
summary(mod3)


## Beer drinking on blood pressure
# No confounders so unadjusted model should give unbiased estimate 

## Mendelian randomization and assumtpions 
## Generate the genotype variable
n <- 10000
mrdat <- data.frame(G = rbinom(n,2,0.2)) # 3 different genotypes: 0,1,2 ? 
table(mrdat$G)
## Generate the confounder U unknown
mrdat$U <- rnorm(n)
## Generate a continuous exposure variable BMI 
mrdat$BMI <- with(mrdat, 25 + 0.7*G +2*U + rnorm(n)) # genotype has an effect on BMI 
## Generate Y 
mrdat$Y <- with(mrdat, 3 + 0.1*BMI - 1.5*U + rnorm(n,0,0.5)) # Y depends on BMI and U but not directly from G
## Simple regression of Y with BMI as covariate
# Get biased estimate for BMI (-0.4) instead of (0.1)
mod4 <- lm(Y ~ BMI, mrdat)
summary(mod4)
## Linear Reg of Y with BMI and G as covariates 
# Y = 3 + 0.1*BMI - 1.5*(BMI -25 - 0.7*G)/2
mod5 <- lm(Y ~ BMI + G, mrdat)
summary(mod5)
# effect of BMI on Y is found to be 0.1 instead of 0.4
## Find an instrumental variable estimate using G as an instrument 
mgx <- lm(BMI ~ G, data = mrdat)
summary(mgx)
ci.lin(mgx) # check the instrument effect
bgx <- mgx$coef[2] # save second coeff, delta 
mgy <- lm( Y ~ G, data = mrdat)
ci.lin(mgy)
bgy <- mgy$coef[2] # save second coeff, beta*delta
causeff <- bgy/bgx # beta = bgy/bgx
causeff # now should be closer to 0.1

## Simulations
n <- 10000
# Initializing simulations 
# 30 simulations (change it, if you want)
nsim <- 30 
mr <- rep(NA,nsim) # empty vector for the outcome parameters
for (i in 1:nsim){ # start the loop
  mrdat <- data.frame(G = rbinom(n,2,0.2))
  mrdat$U <- rnorm(n)
  mrdat$BMI <- with(mrdat, 25 + 0.7*G +2*U + rnorm(n))
  mrdat$Y <- with(mrdat, 3 + 0.1*BMI - 1.5*U + rnorm(n,0,0.5))
  mgx <- lm(BMI ~ G, data = mrdat)
  bgx <- mgx$coef[2]
  mgy <- lm( Y ~ G, data = mrdat)
  bgy <- mgy$coef[2]
  # save the i-th parameter estimate
  mr[i] <- bgy/bgx
} # end of the loop

summary(mr)


## Change the code so that G has now a direct effect on Y 
n <- 10000
# Initializing simulations 
# 30 simulations (change it, if you want)
nsim <- 30 
mr <- rep(NA,nsim) # empty vector for the outcome parameters
for (i in 1:nsim){ # start the loop
  mrdat <- data.frame(G = rbinom(n,2,0.2))
  mrdat$U <- rnorm(n)
  mrdat$BMI <- with(mrdat, 25 + 0.7*G +2*U + rnorm(n))
  mrdat$Y <- with(mrdat, 3 + 0.1*BMI - 1.5*U + 0.05*G + rnorm(n,0,0.5))
  mgx <- lm(BMI ~ G, data = mrdat)
  bgx <- mgx$coef[2] # beta
  mgy <- lm( Y ~ G, data = mrdat)
  bgy <- mgy$coef[2] # beta*delta + delta'
  # save the i-th parameter estimate
  mr[i] <- bgy/bgx # delta + delta'/beta
} # end of the loop

summary(mr)
# biased in the estimate 
# by delta'/beta = 0.05/0.7

library(sem)
summary(tsls(Y ~ BMI, ~G, data = mrdat))






