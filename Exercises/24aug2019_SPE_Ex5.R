## Ex. 1.5 Graphics in R 

## Clean my environment
rm(list = ls())

## load packages
library(Epi)

## import data
data(births)
str(births)

## Simple Plot on the screen 
# histograms
hist(births$bweight, col = "gray", border = "white")
help("hist")
# scatter plot 
with(births, plot(gestwks, bweight))
plot(1:25, pch = 1:25) # symbols that can be used
with(births, plot(bweight,matage, xlab = "Birth Weight (g)", ylab = "Maternal Age (years)", 
                  main = "Relationship between Maternal Age and Birth Weight", pch = 20))

## Adding to a plot 
# start with an empty plot
with(births, plot(gestwks,bweight,type = "n", xlab = "Gestational weeks", 
                  ylab = "Birth weight"))
# add points 
with(births, points(gestwks[sex == 1],bweight[sex == 1], col = "blue"))
with(births, points(gestwks[sex == 2],bweight[sex == 2], col = "red"))
title("Birth weight vs gestational weeks in 500 singleton births")

## Using Indexing for plot elements 
c("blue","red")
births$sex
c("blue","red")[births$sex]
with(births, plot(gestwks, bweight, pch = 16, col = c("blue","red")[sex]))

births$oldmum <- (births$matage >= 40) + 1 # adding 1 to get a numeric var
with(births, plot(gestwks, bweight, pch = c(16,3)[oldmum], col = c("blue","red")[sex]))

## Generating colors 
rainbow(4)
gray(0) 
# note gray only accepts numbers between 0 and 1
# cex is used to modify the size of the symbols
plot(0:10, pch = 16, cex = 3, col = gray(0:10/10)) 

## saving plots 
pdf(file = "bweight_gwks.pdf", height = 4, width = 4)
with(births, plot(gestwks, bweight, pch = 16, col = c("blue","red")[sex]))
legend("topleft", pch = 1, legend = c("Boys","Girls"), col = c("Blue","Red"))
dev.off()

## The par command 
help(par)
par(mfrow = c(1,1))
hist(births$bweight, col = "gray", border = "white")
with(births, plot(gestwks, bweight))

## Locator
with(births, plot(gestwks, bweight))
locator(1)
