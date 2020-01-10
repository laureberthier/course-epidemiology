# Ex. 1.8 Logistic Regression

## Clean my environment
rm(list = ls())

## load packages 
library(Epi)
library(dplyr)
options(digits = 4) # cut down decimal points in output

## Malignant Melanoma in Denmark 

# reading Data 
mel <- read.table("http://bendixcarstensen.com/SPE/data/melanoma.dat", header = T, na.string = ".")
str(mel)
head(mel)

# House keeping 
# load file to do it 
# Mainly transforming num var into factor var
source("http://bendixcarstensen.com/SPE/data/melanoma-house.r")
str(mel)


# summary
mel %>% summary()

# number of case and control by each var
# skin
stat.table(index = skin, contents = ratio(cc,1-cc), data = mel)
effx(cc, type = "binary", exposure = skin, data = mel)
# eyes
stat.table(index = eyes, contents = ratio(cc,1-cc), data = mel)
mf <- glm(cc ~ factor(eyes), family = "binomial", data = mel) # effect of eyes on cc (odds ratios)
round(ci.exp(mf),2) # intercept -- effect for brown eyes, effect ratios for the other two categories
effx(cc, type = "binary", exposure = eyes, data = mel) # same outputs as glm
#freckles
stat.table(index = freckles, contents = ratio(cc,1-cc), data = mel)
effx(cc, type = "binary", exposure = freckles, data = mel)

# controlling for age and sex

# effect of freckles on cc controlled for age and sex
# adjusted estimates after controlling for age and sex
effx(cc, type = "binary", exposure = freckles, control = list(age.cat,sex), data = mel)
mfas <- glm(cc ~ freckles + age.cat + sex, family = "binomial", data = mel)
round(ci.exp(mfas),2)

# likelihood ratio tests
# fitting two models 
# one with no freckles and one with 
mas <- glm(cc ~ age.cat + sex, family = "binomial", data = subset(mel, !is.na(freckles)))
anova(mas, mfas, test = "Chisq")
# deviance change of 48.8 for 2 DF 
1-pchisq(48.786,2)


