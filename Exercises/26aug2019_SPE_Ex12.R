## Exercise 12 
## Survival Analysis: Oral cancer patients 

## Clean my environment
rm(list = ls())

## load packages 
library(Epi)
library(popEpi)
library(data.table)
library(knitr)
library(survival)

## import data 
orca <- read.table("http://bendixcarstensen.com/SPE/data/oralca2.txt", header = T)
# time = follow up time since diagnosis until censoring or death
# event = 0 if censored, 1 if death from oral cancer, 2 if other cause of death
# stage = TNM stage of the tumour
# sex = 1 = F, 2 = M
head(orca)
str(orca)
summary(orca)

## Total Mortality - Kaplan Meier Analyses 
# Two state model 
# we pool the two causes of death into a single outcome 
# Construct a survival object from the event variable and follow up time
# no longer a data frame, cannot be viewed
orca$suob <- Surv(orca$time, 1*(orca$event > 0))
str(orca$suob)
head(orca)

# Survfit object to do Kaplan Meier Analysis
s.all <- survfit(suob ~ 1, data = orca)
# n = number of persons at risk 
# events = number of events 
# median time to event/death and CL 
s.all
print(s.all)
# Summary 
# at each time, number at risk (n.risk), number of events (n.events), 
# survival = 1- cumulative n.events/n.risk at t0, std.error and CI
summary(s.all)
# Kaplan Meier curves
# plot Kaplan Meier curves + CI
# this is what is plotted by default
plot(s.all)
# Cumulative Mortality Proportions
# fun = "event" to plot the cumulative mortality proportion (1 - S(t))
# mark. time = F to omit marking times when censoring 
# conf.int = F if no confidence intervals to be plotted
lines(s.all, fun = "event", mark.time = F, conf.int = F)
lines(s.all, fun = "event", mark.time = T, conf.int = F)

## Total Mortality by Stages
# Plot separate cumulative mortality curves for the different tumour stages
# green for the lowest stage, red for highest stage
s.stg <- survfit(suob ~ stage, data = orca)
col5 <- c("green", "blue", "black", "red", "gray")
plot(s.stg, col = col5, fun = "event", mark.time = F)
# Two parallel plots, one with the cumulative hazards and one with the log cumulative hazards against log time
# fun = "cumhaz" to plot cumulative hazard
# fun = "cloglog" to plot the log of the cumulative hazards
par(mfrow = c(1,2))
plot(s.stg, col = col5, fun = "cumhaz", main = "cum.hazards")
plot(s.stg, col = col5, fun = "cloglog", main = "cloglog: log cum.haz")
par(mfrow=c(1,1))
# if survival times were exponentially distributed, the cloglog should approximately follow a linear pattern
# looks like it is the case for us
# if survival distributions between the different subpop obey a proportional hazards model, the vertical distances
# between cloglog curves should be the same 
# doesn't look like it is our case

# Confounding effect of age on the hazard ratio estimates 
# age categorical groups
orca$agegr <- cut(orca$age, breaks = c(0,55,65,95))
stat.table(index = list(sex,agegr), list(count(), percent(agegr)), margins = T, data =orca)
# Male patients clearly younger than female patients 
# Plot cumulative mortality curves stratified by age and sex
s.agr <- survfit(suob ~ agegr + sex, data = orca)
plot(s.agr, fun = "event", mark.time = F, xlim = c(0,15), col = rep(c("red","blue"),3), lty = 
       c(2,2,1,1,5,5))
# in each ageband, the mortality is higher for males compared to females

## Event-Specific cumulative mortality curves 
# Competitive risks
cif1 <- survfit(Surv(time, event, type = "mstate") ~ 1, data = orca)
str(cif1)
# Plotting Aalen Johansen curves (cumulative incidence for each event) for competing events 
# No suvival curves for competing events!
plotCIF(x = cif1, event = 1, fun = "event", mark.time = F, main = "Cancer death")
plotCIF(x = cif1, event = 2, fun = "event", mark.time = F, main = "Other deaths")
# Cumulative incidences by stages for both events (both causes of death)
col5 <-  c("green", "blue", "black", "red", "gray")
cif2 <- survfit(Surv(time, event, type = "mstate") ~ stage, data = orca)
str(cif2)
par(mfrow = c(1,2))
plotCIF(x = cif2, event = 1,  fun = "event", mark.time = F, main = "Cancer death by stage", col = col5, 
        ylim = c(0,0.7))
plotCIF(x = cif2, event = 2,  fun = "event", mark.time = F, main = "Other deaths by stage", col = col5, 
        ylim = c(0,0.7))
# no effect of stage on other deaths 
# the higher the stage the faster the death from cancer comes
# Plotting the two incidence curves in one graph 
# lower curve is for cancer dath
# higher curve is for other deaths
par(mfrow = c(1,1))
stackedCIF(cif1, colour = c("gray70", "gray85"))

## Regression Modeling of overall mortality
# Fit Cox model on all deaths with covariates sex, age and stages
# use coxph() from survival package
options(show.signif.stars = F)
m1 <- coxph(suob ~ sex + I((age - 65)/10) + stage, data = orca)
# Estimates of the hazard ratios exp(beta)
summary(m1)
# Higher risk of death for males
# Higher risk for older people
# Higher risk the higher the stage is

# Check whether the data is consistent with the assumtion of proportionnal hazards 
# with respect to each of the var separately 
cox.zph(m1)
#  no evidence against proportionality assumption

# remove stage unknown
orca2 <- subset(orca, stage != "unkn")
# pool stages 1 and 2 together
orca2$st3 <- Relevel( orca2$stage, list(1:2,3,4:5))
levels(orca2$st3) = c("I-II", "III", "IV")
m2 <- update(m1, . ~ . -stage + st3, data = orca2)
# summary with estimates of the hazard ratios
summary(m2)
# only estimates of the hazard ratios
round(ci.exp(m2), 4)

# Plot the predicted cumulative mortality curves by stage, jointly stratified by age and sex, focusing
# on patients between 40 and 80 years old 
newd <- data.frame( sex = c(rep("Male", 6), rep("Female",6)),  
                    age = rep( c(rep(40,3), rep(80,3)), 2), 
                    st3 = rep(levels(orca2$st3), 4)
                    )
col3 <- c("green", "black", "red")
par(mfrow = c(1,2))
plot(survfit(m2, newdata = subset(newd, sex == "Male" & age == 40)), 
     col = col3, fun = "event", mark.time = F)
lines(survfit(m2, newdata = subset(newd, sex == "Female" & age == 40)), 
      col = col3, fun = "event", mark.time = F, lty = 2)
plot(survfit(m2, newdata = subset(newd, sex == "Male" & age == 80)), 
     col = col3, fun = "event", mark.time = F)
lines(survfit(m2, newdata = subset(newd, sex == "Female" & age == 80)), 
      col = col3, fun = "event", mark.time = F, lty = 2)


## Modeling event-specific hazards and hazards of the subdistribution 
# Cox model for the cause specific hazard of cancer deaths with same covariates as above sex, age, st3
m2haz1 <- coxph(Surv(time, event == 1) ~ sex + I((age - 65)/10) + st3, data = orca2)
summary(m2haz1)
round(ci.exp(m2haz1),4)

m2haz2 <- coxph(Surv(time, event == 2) ~ sex + I((age - 65)/10) + st3, data = orca2)
summary(m2haz2)
round(ci.exp(m2haz2),4)
# stage effect much more pronounced for the death by cancer cause

library(cmprsk)
attach(orca2)
m2fg1 <- crr(time, event, cov1 = model.matrix(m2), failcode = 1 ) 
summary(m2fg1)


## Lexis objects with multi state set up 

# Make a Lexis object with the Lexis function 
str(orca)
# note here no entry given because time is already our duration, entry is assumed to be 0 
orca.lex <- Lexis(
  exit = list(stime = time),
  exit.status = factor(event, labels = c("Alive", "Oral ca. death", "Other death")),
  data = orca 
  )
# summary gives the number of patients experiencing each transition 
# Events here are the total of events (oral ca. death + other death)
summary(orca.lex)

# Box diagram of the two state set up of competing transition 
boxes(orca.lex)


## Poisson Regression as an alternative to Cox model 

# Cox model with an unspecified form for the baseline hazard is equivalent to a certain kind of Poisson Reg 
# subset of the lexis object
orca2.lex <- subset(orca.lex, stage != "unkn")
orca2.lex$st3 <- Relevel(orca2$stage, list(1:2,3,4:5) )
levels(orca2.lex$st3) = c("I-II", "III", "IV")
summary(orca2.lex)
str(orca2.lex)
# make break points such that there is only one event per row (in between two breakpoints)
cuts <- sort(orca2$event == 1)
orca2.spl <- splitLexis(orca2.lex, breaks = cuts, time.scale = "stime")
orca2.spl$timeband <- as.factor(orca2.spl$stime)
summary(orca2.spl)
str(orca2.spl)
# Poisson Regression 
m2pois1 <- glm(1*(lex.Xst == "Oral ca. death") ~ -1 + timeband + sex + I((age - 65)/10) + st3, 
               family = poisson, offset = log(lex.dur), data = orca2.spl)
# timebands mid point values 
tb <- as.numeric(levels(orca2.spl$timeband))
ntb <- length(tb)
tbmid <- (tb[-ntb] + tb[-1])/2 # midpoints of the interval
# exponential of the estimates
round(ci.exp(m2pois1),3)
par(mfrow = c(1,1))
plot(tbmid, 1000*exp(coef(m2pois1)[1:(ntb-1)]), ylim = c(5,3000), log = "xy", type = "l")

