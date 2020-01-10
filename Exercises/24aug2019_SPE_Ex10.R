# Ex. 1.10 Estimation and Reporting of curved effects 

## Clean my environment
rm(list = ls())

## load packages 
library(Epi)
library(dplyr)
library(splines)
library(mgcv)
library(nlme)

## import data
data(births)

## linear regression and basic scatter plot
par(mfrow=c(1,1))
with(births,plot(gestwks,bweight))
mlin <- lm(bweight ~ gestwks, data = births)
abline(mlin)

## diagnostic plots
par(mfrow = c(2,2))
plot(mlin)
# some deviation from the linear model is apparent

## Fitting a natural cubic spline 
# natural regression splines have more reasonable tail behavior than polynomial regression 
mNs5 <- lm(bweight ~ Ns(gestwks, knots = c(28,34,38,40,43)), data = births)
# 5 pre specified knots
# regression coeffcients what are they? 
round(ci.exp(mNs5, Exp = F))
# function to do graphical representation 
plotFitPredInt <- function(xval, fit, pred, ...){
  matshade(xval, fit, lwd = 2, alpha = 0.2)
  matshade(xval, pred, lwd = 2, alpha = 0.2)
  matlines(xval, fit, lty = 1, lwd = c(3,2,2), col = c("red", "blue", "blue"))
  matlines(xval, pred, lty = 1, lwd = c(3,2,2), col = c("red", "green", "green"))
}
# make a vector of gestwks
nd <- data.frame(gestwks = seq(24,45, by = 0.25))
# fitted value for this vector 
fit.Ns5 <- predict(mNs5, newdata = nd, interval = "conf")
# predict bweight for this vector of gestwks using the spline model made earlier 
pred.Ns5 <- predict(mNs5, newdata = nd, interval = "pred")
par(mfrow = c(1,1))
with(births, plot(bweight ~ gestwks, xlim = c(23,46), cex.axis = 1.5, cex.lab = 1.5))
plotFitPredInt(xval = nd$gestwks, fit = fit.Ns5, pred = pred.Ns5)
# diagnostic plots 
par(mfrow=c(2,2))
plot(mNs5)


## New spline model with 10 knots 
# results very sensitive to the number of knots
# number of knots quite arbitrary
# behavior of the curve not good for low values of gestwks
mNs5_2 <- lm(bweight ~ Ns(gestwks, knots = seq(25, 43, by=2)), data = births)
fit.Ns5 <- predict(mNs5_2, newdata = nd, interval = "conf")
# predict bweight for this vector of gestwks using the spline model made earlier 
pred.Ns5 <- predict(mNs5_2, newdata = nd, interval = "pred")
par(mfrow = c(1,1))
with(births, plot(bweight ~ gestwks, xlim = c(23,46), cex.axis = 1.5, cex.lab = 1.5))
plotFitPredInt(xval = nd$gestwks, fit = fit.Ns5, pred = pred.Ns5)
# diagnostic plots 
par(mfrow=c(2,2))
plot(mNs5_2)

## penalized spline model 
# to avoid the arbitrariness of picking a number of knots 
# we use a penalized spline model which imposes a roughness penalty on the curve
# to fit a penalized spline model we use the gam()
help(gam)
# penalized spline model
mPs <- gam(bweight ~ s(gestwks), data = births)
# summary
summary(mPs)
# estimated residual variance
mPs$sig2
# estimated residual standard deviation 
sqrt(mPs$sig2)
# Fitted curve, 95% ci, 95% pred intervals
pr.Ps <- predict(mPs, newdata = nd, se.fit=T)
str(pr.Ps) # only fitted values and its SE
fit.Ps <- cbind(pr.Ps$fit, pr.Ps$fit - 2*pr.Ps$se.fit, pr.Ps$fit + 2*pr.Ps$se.fit)
pred.Ps <- cbind(pr.Ps$fit, pr.Ps$fit - 2*sqrt(pr.Ps$se.fit^2 + mPs$sig2), pr.Ps$fit + 2*sqrt(pr.Ps$se.fit^2 
                + mPs$sig2))
par(mfrow = c(1,1))
with(births, plot(bweight ~ gestwks, xlim = c(24,45), cex.axis = 1.5, cex.lab = 1.5))
plotFitPredInt(xval = nd$gestwks,fit = fit.Ps, pred = pred.Ps)


## Testis cancer
# import data 
data(testisDK)
str(testisDK)
summary(testisDK)
head(testisDK)

























