## Chapter 1
############

install.packages("astsa")
library(astsa)


## Dow Jones Industrial Average
#------------------------------

install.packages("TTR")
install.packages("xts")

library(TTR)
library(xts)

djia <- getYahooData(symbol = "DJIA", 
             start = 20060420,
             end = 20160420,
             freq = "daily")


djia_return <- diff(log(djia$Close))
head(djia_return)
#                  Close
#2006-04-20            NA
#2006-04-21  0.0004019814
#2006-04-24 -0.0009813080

djia_return <- djia_return[-1]

plot(djia_return, main = "DJIA Returns", type = "n")
lines(djia_return)


##FMRI
#------

head(fmri1)


## White noise and moving average
#--------------------------------
w <- rnorm(500, 0, 1)

# linear combination of values in ts is referred to as a filtered series
v <- filter(w, filter = rep(1/3, 3), sides = 2L) # moving average

par(mfrow = c(2, 1), mai = c(0.5, 0.4, 0.5, 0.05) )
plot.ts(w, main = "White noise")
plot.ts(v, ylim = c(-3, 3), main = "moving average")


## Expl 1.10 Autoregressions
#----------------------------

par(mfrow = c(1, 1), mai = c(0.5, 0.4, 0.5, 0.05))
w <- rnorm(550, 0, 1) # 50 extra to avoid startup problem
x <- filter(w, filter = c(1, -.9), method = "recursive")[-(1:50)]
plot.ts(x, main = "autoregressive")



## Expl 1.11 Random walk with drift Model
#-----------------------------------------

# 200 obs generated from the model with delta = 0,
# and delta = .2, with sigma_w = 1

set.seed(28-11-2021)
w <- rnorm(200); x <- cumsum(w)
w_dft <-  w + .2; x_dft <- cumsum(w_dft)

range(x_dft) #[1] -3.449574 53.321570
plot.ts(x_dft, ylim = c(-5, 55), main = "Random walk / (with drift)")
lines(x, col = 2)
abline(a = 0, b = 0.2, lty = 4) # straigth line for comparison
abline(h = 0, col = 4, lty = 2) 

