## Exercises 
# Ex.1.3 - Tabulation 

## Clean my environment
rm(list = ls())

## load packages 
library(dplyr)
library(Epi)
library(kableExtra)

## import data 
data(births)
names(births)
head(births)

## Transform some of the vars into factors
births$hyp <- factor(births$hyp, labels = c("normal","hyper"))
births$sex <- factor(births$sex, labels = c("M", "F"))
births$agegrp <- cut(births$matage, breaks = c(20,25,30,35,40,45), right = F)
births$gest4 <- cut(births$gestwks, breaks = c(20,35,37,39,45), right = F)

## examine dataset 
str(births)

## One way tables
table(births$sex)
stat.table(index = sex, data = births)
stat.table(index = list("Gender" = sex), contents = list(N = count(),  "(%)" = percent(sex)), data = births)
stat.table(index = sex, contents = list(count(), mean(bweight)), data = births)
stat.table(index = list(sex,lowbw), contents = percent(lowbw), data = births)
help(stat.table)

## Two way tables
stat.table(index = list(sex,hyp), contents = mean(bweight), data = births)




