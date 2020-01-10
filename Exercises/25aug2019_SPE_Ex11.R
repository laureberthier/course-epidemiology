## Graphics Meccano 

## Clean my environment
rm(list = ls())

## load packages 
library(ggplot2)
library(grid)
library(cowplot)

## import data
alkfos <- read.csv("./data/alkfos.csv")

str(alkfos)
head(alkfos)

## housekeeping code
source("./data/alkfos-house.r")

## objects in the environment 
objects()

## error bars 
upper <- means + sems
lower <- means - sems

## plot means for group 1 against time
# modify margins 
par(mar = 0.1 + c(8,4,4,2))
# plot group means vs time
# type = "b" adds line between the different points
plot( times, means[1,], type = "b", ylim = c(-40,30), col = "red",
      xlab = "Months after randomization", ylab = "Percent change in serum alkaline phosphatase", 
      xaxt = "n")
points(times,means[2,], type = "b", col = "blue") 
# error bars
segments(times, means[1,],times,upper[1,], col = "red")
segments(times, means[1,],times,lower[1,], col = "red")
segments(times, means[2,],times,upper[2,], col = "blue")
segments(times, means[2,],times,lower[2,], col = "blue")
# horizontal line y = 0 
abline(0,0, col = "black") 
# add x axis 
axis(1, times, labels = T)
# add text 
mtext(available[1,], side = 1, line = 5, at = times)
mtext(available[2,], side = 1, line = 6, at = times)
mtext("Placebo", side = 1, line = 5, adj = 1, at = par("usr")[1])
mtext("Tamofixen", side = 1, line = 6, adj = 1, at = par("usr")[1])
# mtext("Control 23 23 23 23 22 21 21",side = 1, line = 5  )
# mtext(" Tamofixen 23 23 23 22 21 21",side = 1, line = 6  )

## Other way
RStudioGD()
par(mar=.1 + c(8,4,4,2))
# las = 1 to turn by 90 degrees the labels on the y axis
# formula can also be used
# xaxt = "n" to remove the y axis 
plot(means[1,] ~ times, type="b", las=1, ylim=c(-32,20), xaxt="n", 
     lwd=2, ylab="Perctentage change", xlab="Months after randomization",
     font.axis=2, font.lab=2)
axis(1, c(0,3,6,9,12,18,24), font=2) # add y axis
segments(x0=times, y0=lower[1,], y1= upper[1,]) # add error bars
lines(means[2,]~times,type="b", col=2, lty=2,lwd=2) # do the second for second group
segments(x0=times, y0=lower[2,], y1= upper[2,],col=2)
abline(h=0, col="grey")

mt1 <- paste("Control", " ","23", "23","23","23", "22", "21","21", sep="      ")
mt2 <- paste("Tamoxifen", "20", "20","20","19", "19", "18","18", sep="      ")
mtext(mt1, side=1, line=5, cex=1.2, font=2)
mtext(mt2, side=1, line=6, cex=1.2, font=2)



## ggplot 2
# works with data frames
# inspect data 
str(ggdata)
head(ggdata)
# plot 
p1 <- qplot(x = time, y = means, group = treat, geom = c("point", "line"), data = ggdata)
p2 <- qplot(x = time, y = means, group = treat, ymin = means -sems,
            ymax = means + sems, yintercept = 0,  geom = c("point", "line", "linerange"), data = ggdata)
p2
# change x and y names and axis ticks
p2 <- p2 + scale_x_continuous(name = "Months after randomization", breaks = times[1:7]) + 
  scale_y_continuous(name = "% Change in alkaline phosphatase")
p2
# theme 
p2 + theme_bw()
# alternative ggplot function 
p <- ggplot(aes(x = time, y = means, group = treat, ymin = means - sems,
                 ymax = means + sems,  col = treat), data = ggdata) + geom_point(size = 0.7) +
                 geom_line() + geom_linerange() + 
                 scale_x_continuous(name = "Months after randomization", breaks = times[1:7]) + 
                 scale_y_continuous(name = "% Change in alkaline phosphatase") + geom_hline(aes(yintercept = 0))
p

## Grid Graphics
# text table to add below the graph
tab <- ggplot( data = ggdata, aes(x = time, y = treat, label = available) ) + geom_text(size = 3) + xlab(NULL) +
  ylab(NULL) + scale_x_continuous(breaks = NULL)
tab

# layout 
# with 2 rows and 1 line, height gives the heights of each line 
Layout <- grid.layout(nrow = 2, ncol = 1, heights = unit(c(2,0.25), width = c("null","null")))
grid.show.layout(Layout)
# print graph and table in the appropriate parts of the layout 
grid.newpage()
pushViewport(viewport(layout = Layout))
print(p, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(tab, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))

# plot_grid allows to plot multiple plots into a grid, no need for layout
plot_grid(p,tab, align = "v", ncol = 1, nrow = 2, rel_heights =  c(5,1))
theme_set(theme_cowplot())
plot_grid(p, tab, align = "v", ncol = 1, nrow= 2, rel_heights = c(5,1))
