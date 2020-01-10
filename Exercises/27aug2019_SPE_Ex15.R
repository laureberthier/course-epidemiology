## Exercise 1.15 - Nested case-control study and case-cohort study: Risk factors of coronary heart disease 


## Clean my environment
rm(list = ls())

## load packages 
library(Epi)
library(survival)
library(dplyr)

## Read in the data cohort and name the resulting dat frame as oc
url <- "https://raw.githubusercontent.com/SPE-R/SPE/master/pracs/data"
oc <- read.table(paste(url, "occoh.txt", sep = "/"), header = T)

## look at the table 
str(oc)
summary(oc)
head(oc)

## Change dates into fractional calendar year 
oc$ybirth <- cal.yr(oc$birth)
oc$yentry <- cal.yr(oc$entry)
oc$yexit <- cal.yr(oc$exit)

## Compute age at entry and at exit 
oc$agentry <- oc$yentry - oc$ybirth 
oc$agexit <- oc$yexit - oc$ybirth 

## Lexis object created from the data.frame 
oc.lex <- Lexis( entry = list(per = yentry, 
                              age = yentry - ybirth),
                 exit = list( per = yexit), 
                 exit.status = chdeath, 
                 id = id, 
                 data = oc
                 )
## Look at the Lexis object 
## lex.dur : duration, lex.cst: current state, lex.Xst: exit state, lex.id : id
str(oc.lex)
summary(oc.lex)

## Lexis diagram: graphical rep of the follow-up lines and outcome cases
## Gray life lines + points at the end of the life lines if outcome
par(mfrow = c(1,1))
plot(oc.lex, xlim = c(1990,2010), grid = T)
points(oc.lex, pch = c(NA, 16)[oc.lex$lex.Xst + 1])

## Illustrate follow-up lines and outcome events according to age instead of calendar years
oc.ord <- cbind(ID = 1:1501, oc[order(oc$agexit, oc$agentry),]) # new ID being ordered by age at exit and entry
oc.lexord <- Lexis( entry = list(age = agentry),  # new lexis object with timescale chosen to be age
                    exit = list(age = agexit), 
                    exit.status = chdeath, 
                    id = ID, 
                    data = oc.ord)
summary(oc.lexord) # look at the Lexis object, same summary as before
str(oc.lexord) # same var as before
plot(oc.lexord, "age")
points(oc.lexord, pch = ifelse(oc.lexord$lex.Xst ==1, 16, NA))
with(subset(oc.lexord, lex.Xst == 1), abline(v = agexit,lty = 3))  # cases occur after age 50 

## zoom in on age between 50 and 58 
plot(oc.lexord, "age", xlim = c(50,58), ylim = c(5,65))
points(oc.lexord, pch = ifelse(oc.lexord$lex.Xst ==1, 16, NA))
with(subset(oc.lexord, lex.Xst == 1), abline(v = agexit,lty = 3))   

#########################################
## Nested case-control study 
#########################################
## Risk set sampling or time matched sampling i.e. nested case control study 
## Superior when we expect a lot of censoring

## The risk sets are defined according to the age at diagnosis of the case
## Generate a categorical variable agen2 for age at entry 
oc.lex$agen2 <- cut(oc.lex$agentry, breaks = seq(40,62,1))
## Matched Sampling from risk sets 
set.seed(1234) # set your own seed
help(ccwc)
cactrl <- ccwc(entry = agentry, # time of entry to follow up
               exit = agexit,  # time of exit from follow-up
               fail = chdeath, # status on exit
               controls = 2, # number of controls per case
               match = agen2, # list of cat var to match cases and controls on
               include  = list(id, agentry), # list of var to be carried accross into the study
               data = oc.lex, 
               silent = F
               )
## Look at the output Data frame cactrl
##  Set: case control set number, Map: row number of records in input df, Time: failure time of the case, Fail: 
## failure status (case or Control status)
str(cactrl) 

## Risk factors for the cases and their matched controls  
## Risk factors include determination of the total cholesterol levels from the frozen sera
# import data on risk factors 
# smok: cigarette smoking, sbp: systolic blood pressure, tchol = total cholesterol level
ocX <- read.table( paste(url, "occoh-Xdata.txt", sep = "/"), header = T )
str(ocX)

## Merge values of the risk factors to the cactrl 
## NB: the same control can be used for different cases 
oc.ncc <- merge(cactrl,ocX %>% select(id,smok,tchol,sbp), by = "id")
str(oc.ncc)
# Convert smoking into a factor variable 
oc.ncc$smok <- factor(oc.ncc$smok, labels = c("never", "ex", "1-14/d", ">14/d"))
str(oc.ncc)
# Start the analysis with simple tabulations 
stat.table( index = list(smok, Fail), 
            contents = list(count(), percent(smok)), 
            margins = T, 
            data = oc.ncc)
## smoking effect with logistic regression not taking into account matching
smok.crncc <- glm(Fail ~ smok , family = binomial, data = oc.ncc) # logit function is log for binomial family
summary(smok.crncc)  
# exponential of the estimates of the parameters
round(ci.exp(smok.crncc),3) 
## Proper analysis: logistic regression taking into account matching 
m.clogit <- clogit( Fail ~ smok + I(sbp/10) + tchol + strata(Set), data = oc.ncc)
summary(m.clogit)  
# Estimates can be comapred to the crude ones obtained before 

#################################
## Case cohort study 
#################################
## Subcohort: random sample from the full cohort, may contain cases or not 
## + Cases not already included in the subcohort = case cohort sample 
## no matching in case cohort study
## To make sure you do not have confounding effects, need to make adjustements in the analysis

## subcohort selected as a simple random sample (n = 260) from the whole cohort
N <- 1501 # total number of ids in the whole cohort
n <- 260
set.seed(12345)
subcids <- sample(N,n) # subcohort ids 
oc.lexord$subcind <- 1*(oc.lexord$id %in% subcids) # make cat var 1 if id in subcids 0 otherwise

## create a new data.frame with cases and controls 
oc.cc <- subset(oc.lexord, subcind == 1 | chdeath == 1)
dim(oc.cc)[1] - n # number of cases 
str(oc.cc)

## add risk factors to the previous dataset 
oc.cc <- merge(oc.cc, ocX[,c("id", "smok", "tchol", "sbp")], by ="id")
## Graphical illustration of the lifetimes contained in case cohort data
plot(subset(oc.cc,chdeath == 0), "age")
lines(subset(oc.cc, chdeath ==1 & subcind == 1), col = "blue")  # cases in the subgroup
lines(subset(oc.cc, chdeath ==1 & subcind == 0), col = "red") # cases not in subgroup
points(subset(oc.cc, chdeath == 1), pch = 16, col = c("blue", "red")[oc.cc$subcind + 1]) 

## smoking cat variable again 
oc.cc$smok <- factor(oc.cc$smok, labels = c("never", "ex", "1-14/d", ">14/d"))

## Crude estimate of the hazard ratio for the various smoking catefories vs non smokers in the subcohort
sm.cc <- stat.table(
  index = smok, 
  contents = list(Cases = sum(lex.Xst), 
                  Pyrs = sum(lex.dur)), # time duration per smoking cat 
  margins = T, 
  data = oc.cc
)
print(sm.cc, digits = c(sum = 0, ratio = 1))
HRcc <- (sm.cc[1,-5]/sm.cc[1,1])/(sm.cc[2,-5]/sm.cc[2,1]) # calculation of the crude HRs
round(HRcc,3) # The more you have smoked the more risk to coronory heart disease

## To analyze case cohort study 
## Cox model with weighted partial likelihood 
oc.cc$survobj <- with(oc.cc, Surv(agentry,agexit,chdeath))
str(oc.cc)
cch.LY <- cch(survobj ~ smok + I(sbp/10) + tchol, stratum = NULL, subcoh = ~subcind, id = ~id, cohort.size = N,
              data = oc.cc, method = "LinYing")

summary(cch.LY)


## Full cohort analysis and comparisons 
## data frame with full cohort 
oc.full <- merge(oc.lex, ocX[,c("id","smok","tchol","sbp")], by.x = "id", by.y = "id")
oc.full$smok <- factor(oc.full$smok, labels = c("never", "ex", "1-14/d", ">14/d"))
## Crude estimates 
sm.coh <- stat.table(
  index = smok, 
  contents = list(Cases = sum(lex.Xst), 
                  Pyrs = sum(lex.dur)), # time duration per smoking cat 
  margins = T, 
  data = oc.full
)

print(sm.coh, digits = c(sum = 0, ratio = 1))
HRcc.coh <- (sm.coh[1,-5]/sm.coh[1,1])/(sm.coh[2,-5]/sm.coh[2,1]) # calculation of the crude HRs
round(HRcc.coh,3)
## Ordinary Cox model to the full cohort 
oc.full$survobj <- with(oc.full, Surv(agentry,agexit,chdeath))
str(oc.full)
cox.coh <- coxph(survobj ~ smok + I(sbp/10) + tchol, data = oc.full )
summary(cox.coh)
## Comparison of the point estimates and se

