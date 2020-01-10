## Exercises 
# Ex.1.2 - Reading Data into R

## directories
getwd()
setwd("M:/LAEB/Program Learning/R/Epidemiology in R/data")

## Load packages
library(Epi)
library(foreign)

## import data
objects("package:datasets")
help(Titanic)
data(bdendo)
help(bdendo)


## Text file Data
fem <- read.table("./data/fem.dat",header = T)
names(fem)
colnames(fem)
str(fem)
head(fem)
summary(fem)
fem$IQ[fem$IQ == -99] <- NA
# if you forget headers
fem2 <- read.table("./data/fem.dat")
str(fem2) # all char var assumed to be factors 
fem3 <- read.table("./data/fem.dat",as.is = T) # char var are left as char 
str(fem3)
# assume data values are separated by another separator 
fem4 <- read.table("data/fem.dat", sep = "\t", header = T)
# mis specifying the representation of missing value 
# NAs are "dots"
fem5 <- read.table("data/fem-dot.dat", header = T) 
# "dots" for NAs replace by NA
fem5 <- read.table("data/fem-dot.dat", header = T, na.strings = ".")

## Spreadsheet Data 
args(read.csv) #read.csv is special to csv file, read.table with some specific argument for csv file
args(read.csv2) # read.csv2 when sep = ; and read.csv when sep = ,

## Data from the Internet
fem6 <- read.table("https://www.bendixcarstensen.com/SPE/data/fem.dat", header = T)
str(fem6)

## Reading from the cliboard
# not reproducible, depends on the mouse moves
# loses predictions

## Binary data 
# load packages 
fem7 <- read.dta("data/fem.dta") # stata format
head(fem7)
str(fem7)
attr(fem7,"var.labels")


