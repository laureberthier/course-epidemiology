## Exercises 
# Ex.1.1 - Practice with basic R 

## Clean the environment
rm(list = ls())

## Load packages 
library(Epi)
library(dplyr)

## Directory 
# Working directory
getwd()
# Files in directory
dir()
# Set directory 
setwd("\\\\HLU.CORP.LUNDBECK.COM/Users$/HLU/LAEB/Program Learning/R/Epidemiology in R")

## Calculator
a <- round(sqrt(12+16)*5,2)
a
exp(a)
log(a)
log10(a)

## Vector
v <- c(4,6,1,2.2)
v
v+3
3*v
w <- c(4,6,NA)
mean(v)
mean(w)
mean(w,na.rm = T)
str(w) # structure
length(w)

## Sequences
v <- 1:10 # sequence of integers
v
seq(from = 15, to = 85, by = 5) # more general sequence
seq(from = 2, to = 4, by = 0.1)

w <- c(1,-1,2,-2)
w
str(w)
w+1
v <- seq(from = 5, to = 75, by = 5)
v2 <- c(0,1,v)
length(v2)

## Indexing 
x <- c(2,7,0,9,10,23,11,4,7,8,6,0)
x[4]
x[3:5]
x[c(1,5,8)]
x[(1:6)*2]
x[-1]
n <- length(x)
x[n+1]
x > 10
x[x > 10]
x[x >= 10 & x <= 20]
x[x < 10 | x > 20]
x[1] <- 1000
x[x==0] <- 1
x
x[2:4] <- c(0,8,1)
x
y <- x[x < 3] + 3
x[x < 3] <- y
x
ifelse(x%%2 == 0, "even", "odd")
x[x < 10 & x > 4]
x[x > 10] <- 10
x[x < 5] <- x[x < 5]*2

## Lists 
m <- list(4, T, "name of company")
m
m[3]
m[1:2]
str(m[3])
mylist <- list(name = c("Joe", "Ann", "Jack", "Tom"), 
               age = c(34,50,27,42))
mylist$name
mylist$age

## Data frames 
mydata <- data.frame(name = c("Joe", "Ann", "Jack", "Tom"), 
                     age = c(34,50,27,42), 
                     sex = c(1,2,1,1), 
                     height = c(185,170,175,182))
mydata$height
mydata
mydata[1,]
mydata[,c("age","height")]
mydata[2,4]

yourdata <- data.frame(
  name = c("Ann", "Peter", "Sue", "Jack", "Tom", "Joe", "Jane"), 
  weight = c(67,81,56,90,72,79,69)
)
newdata <- merge(mydata,yourdata)
newdata
newdata2 <- merge(mydata, yourdata, all = T)
newdata2

## Built in data frames 
data(births)
data(diet)
objects()
?diet
remove(diet)
objects()

## Indexing 
head(births)
births[1,"bweight"]
births[1,2]
births[1:10,"bweight"]
births[,"bweight"]
births[1,]
births[7,"gestwks"]
births[7,]
births[1:10,"gestwks"]

## summaries 
summary(births)
names(births)
summary(births$hyp)
with(births,summary(hyp))
births %>% summary()

## new variables 
births$logbw <- log(births$bweight)
births$bweight <- births$bweight/1000

## Turning var into factor 
births$hyp <- factor(births$hyp, labels = c("normal", "hyper"))
births$sex <- factor(births$sex, labels = c("M", "F"))

## freq tables 
with(births, table(hyp))
table(births$hyp)
prop.table(table(births$hyp))
prop.table(table(births$sex))
table(births$sex,births$hyp)
births$early <- ifelse(births$gestwks < 30,1,0)
table(births$early)
summary(births)

## Grouping 
births$agegrp <- cut(births$matage, breaks = c(20,30,35,40,45),right = F)
table(births$agegrp)
births$agegrp <- cut(births$matage, breaks = c(20,30,35,40,45),right = T)
table(births$agegrp)

summary(births$gestwks)
births$gest4 <- cut(births$gestwks, breaks = c(20,35,37,39,45))
table(births$gest4)

## Saving and Loading Data 
save(births, file = "births.RData")
load("births.RData")

## Search 
search()
objects()

attach(births)
hyp
detach()

