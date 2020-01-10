## Exercise 1.16 -Time-dependent variables and multiple states 


## Clean my environment
rm(list = ls())

## load packages 
library(Epi)
library(survival)
library(dplyr)
library(foreign)
library(popEpi)
library(splines)
library(mgcv)

## import data 
## The dataset is in stata format so we use the read.dta
renal <- read.dta("https://raw.githubusercontent.com/SPE-R/SPE/master/pracs/data/renal.dta")

## Look at the data 
## id, sex (1 = M, 2  = F), dob, doe (date of entry, 2.5 years after NRA diagnosis), dor (date of remission), 
## dox (date of exit), event (exit status, 1,2,3 = stage (can be death, ESRD for each stage) 0 = censored)
str(renal)
head(renal)

## Factorize sex
renal$sex <- factor(renal$sex, labels = c("M", "F"))
renal$exit_status <- factor(renal$event > 0, labels = c("NRA", "ESRD"))

## Make Lexis object 
Lr <- Lexis(entry = list( per = doe, # date of entry
                          age = doe - dob, # age
                          tfi = 0), # time from initiation
            exit = list(per = dox), 
            exit.status = factor(event > 0, labels = c("NRA", "ESRD") ),  # 0 if NRA and ESRD if > 0
            data = renal) # date of exit

str(Lr)
summary(Lr)

## Visualize the follow up in a Lexis diagram 
plot(Lr, col = "black", lwd = 3)
# issue there is a pb with a negative age! 
subset(Lr, age < 0)
# pb in the dob reported of that person!
# Correct the data and make new plot 
Lr <- transform(Lr, dob = ifelse( dob > 2000, dob - 100, dob), 
                    age = ifelse( dob > 2000, age + 100, age)
)
subset(Lr, id == 586) # dob and age have been corrected to dob - 100, age + 100
plot(Lr, col = "black", lwd = 3) # new plot

## Cox regresssion analysis of the endpoint ESRD (death)
# not taken into account the effect of remission on death 
m1 <- coxph.Lexis(Lr, formula = tfi ~ I(age/10) + sex)
ci.exp(m1)
# F have less risk to go to ESRD compared to M 
# More risk with higher age


## Understand how the occurence of remission influences mortality
## Records need to be cut 
## one record for the time before remission, one record for the time after remission 
## Use cutLexis

Lc <- cutLexis(Lr, cut = Lr$dor,  # where to cut the follow-up
               timescale = "per", # what time scales are we referring to 
               new.state = "Rem", # name of the new state
               split.state = T,  # different states depending on previous 
               precursor.states = "NRA") # which states are less severe
## summary
summary(Lc)
## Look at Lc
head(Lc)
## Look at a few patients
subset(Lc, lex.id %in% c(5,7,9))

## show how states are connected and the transitions between one state to the other
boxes(Lc)
boxes(Lc, boxpos = T, scale.R = 100, show.BE = T) # show rates and the ones that remain in a state 

## Make Cox regression of mortality (i.e. endpoint ESRD) with sex, age at entry and remission as explanatory var
EP <- levels(Lc)[3:4] # The two death levels (either after NRA or after REM)
levels(Lc)
## using coxph
m2 <- coxph(
  ## survival type object used as response variable
  Surv(time = tfi, # from
       time2 = tfi + lex.dur, # to 
       event = lex.Xst %in% EP) ~ # event 
  sex + I((age - 50)/10) + # fixed covariates
  (lex.Cst == "Rem"), # time-dependent variable (current state = Rem) 
  data = Lc
)
# summary 
summary(m2)
# less risk to die from a REM state compared to NRDA
## using coxph.Lexis 
## one can do the cox model regression directly from the Lexis object without having to do a survival object 
m3 <- coxph.Lexis(Lc, formula = tfi ~ sex + I((age - 50)/10) + (lex.Cst == "Rem"))
summary(m3)
## Test assumptions 
cox.zph(m2)

## Splitting the follow-up time 
## split the follow up time every month after entry 
sLc <- splitLexis(Lc, "tfi", breaks = seq(0,30,1/12))
summary(Lc, scale = 100)
summary(sLc, scale = 100)

## Poisson Model corresponding to the Cox model fitted previously
mp <- glm(cbind(lex.Xst %in% EP, lex.dur) ~ Ns(tfi, knots = c(0,2,5,9,14)) + sex + I((doe - dob - 40)/10) + 
            I(lex.Cst == "Rem"), family = poisreg, data = sLc
          )
ci.exp(mp)

mp <- glm.Lexis(sLc,  formula = ~ Ns(tfi, knots = c(0,2,5,9,14)) + sex + I((doe - dob - 40)/10) + 
                  I(lex.Cst == "Rem")
                )
summary(mp)

## Use the Gam function to find the optimal number of knots that smoothens the curve and provide a good fit
mx <- gam( cbind(lex.Xst %in% EP, lex.dur) ~ s(tfi, k = 10) + sex + I((doe - dob - 40)/10) + 
             I(lex.Cst == "Rem"), family = poisreg, data = sLc
)

ci.exp(mp, subset= c("Cst", "doe", "sex"))
ci.exp(mx, subset= c("Cst", "doe", "sex"))
## Compare Cox models and Poisson Reg 
ci.exp(mx, subset= c("Cst", "doe", "sex"), pval = T)
ci.exp(m2)
# small diff in the estimate of the coeff for current state =  REM 
# the model has the same assumptions as the Cox model + an assumption about the smoothness of time since entry 
# estimated affect of tfi (time since initiation)
plot(mx)











