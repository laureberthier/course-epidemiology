## Exercise 13 
## Time Splitting, time scales and SMR

## Clean my environment
rm(list = ls())

## load packages 
library(Epi)
library(mgcv)
library(splines)
library(popEpi)

sessionInfo()

## import data 
data(DMlate)

## info on Data
?DMlate
str(DMlate)
head(DMlate)
summary(DMlate)

## Set the dataset as Lexis object
# timescales: age, calendar time and duration of diabetes
# Note all time scales in entry must have the same unit
LL <- Lexis(entry = list( age = dodm - dobth, 
                          cal = dodm, 
                          dur = 0), 
            exit = list(cal = dox), 
            exit.status = 1*!is.na(dodth), 
            data = DMlate)
# output: object of class Lexis 
# represented as a data frame with a column for each time scale
# lex.id : identification of the persons, lex.dur duration of follow up, lex.Cst current state, lex.Xst 
# exit state
head(LL)
summary(LL)

## Overview of the mortality 
# Empirical rate within each sex category
stat.table( sex, list(D = sum(lex.Xst), Y = sum(lex.dur), Rate = ratio(lex.Xst, lex.dur,1000)), 
            margins = T, data = LL)

## Assess how mortality depends on age, calendar time, duration of diabetes
# split the follow up time into three time scales
# in practice we only split it up according to one time scale and 
# use the value of the two other time scales at the end of each "split"/"row"
SL <- splitLexis(LL, breaks = seq(0,125,1/2), time.scale  = "age")
summary(SL)
# Note more transitions from 0 to 0 because the data for each individual has been splitted according to the age
# of the patient. But same number of transitions from 0 to 1 and same number of events as before 
# Other way of doing it
SL2 <- splitMulti(LL, age = seq(0,125,1/2))
summary(SL2)
# same results as with using splitLexis


## Age specific mortality 
r.m <- gam( cbind(lex.Xst, lex.dur) ~ s(age, k = 20), family = poisreg, data = subset(SL, sex == "M"))
r.f <- gam( cbind(lex.Xst, lex.dur) ~ s(age, k = 20), family = poisreg, data = subset(SL, sex == "F"))
# residuals plots
par(mfrow = c(2,2))
gam.check(r.m)
par(mfrow = c(1,1))
plot(r.m)
# extract the estimated rates 
nd <- data.frame( age = seq(10,90,0.5))
# the rates are in 1000 PY so we need to multiply the estimated rates by 1000
p.m <- ci.pred(r.m, newdata = nd)*1000
str(p.m)
p.f <- ci.pred(r.f, newdata = nd)*1000
str(p.f)
# plot the predicted rates for men and women together 
matshade(nd$age, cbind(p.m,p.f), plot = T, col = c("blue", "red"), lwd = 3, log = "y", xlab = "Age", 
         ylab = "Mortality of DM ptt per 1000 PY")



