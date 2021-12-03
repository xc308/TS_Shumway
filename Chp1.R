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

par(mfrow = c(1, 1))
set.seed(28-11-2021)
w <- rnorm(200); x <- cumsum(w)
w_dft <-  w + .2; x_dft <- cumsum(w_dft)

range(x_dft) #[1] -3.449574 53.321570
plot.ts(x_dft, ylim = c(-5, 55), main = "Random walk / (with drift)")
lines(x, col = 2)
abline(a = 0, b = 0.2, lty = 4) # straigth line for comparison
abline(h = 0, col = 4, lty = 2) 



## Expl 1.12 Signal in noise
#----------------------------

t <- 1:500
cs <- 2 * cos(2 * pi * 1/50 * t + 0.6 * pi)
w <- rnorm(500, 0, 1)

par(mfrow = c(3, 1), cex.main = 1.5, mar = c(3, 2, 2, 1))
pt_cs <- plot.ts(cs, main = expression(2 * cos(2 * pi * t / 50 + 0.6 * pi)))
pt_cs1 <- plot.ts(cs + w, main = expression(2 * cos(2 * pi * t / 50 + 0.6 * pi) + N(0, 1)))
pt_cs5 <- plot.ts(cs + 5 * w, main = expression(2 * cos(2 * pi * t / 50 + 0.6 * pi) + N(0, 5^2)))

save(pt_cs, file = "Fig/Chp1_plt_cs.jpg")
save(pt_cs1, file = "Fig/Chp1_plt_cs1.jpg")
save(pt_cs5, file = "Fig/Chp1_plt_cs5.jpg")


# Expl 1.25 Sample ACF of SOI
r <- acf(soi, 6, plot = F)
str(r)
# List of 6
#$ acf   : num [1:7, 1, 1] 1 0.6041 0.3738 0.2141 0.0501 ...
#$ type  : chr "correlation"
#$ n.used: int 453
#$ lag   : num [1:7, 1, 1] 0 0.0833 0.1667 0.25 0.3333 ...

r <- round(acf(soi, 6, plot = F)$acf[-1], 3)
print(r)

str(lag(soi))
str(lag(soi, -1))
str(lag(soi, -6))

all(str(lag(soi, -1)), str(lag(soi, -6)))
# [1] TRUE

head(lag(soi, k = -1)) # k: The number of lags (in units of observations)
head(lag(soi, k = -6))

all(lag(soi, k = -1), lag(soi, k = -6))


plot(lag(soi, -1), soi); legend("topleft", legend = r[1])
plot(lag(soi, -6), soi); legend("topleft", legend = r[6])

acf(soi, plot = T)



## Expl 1.26 A simulated ts
# Aim: to compare the sample ACF for different 
# sample sizes to the theoretical ACF. 

# let xt = 1 if H and xt = -1 if T
# yt = 5 + xt - .7 xt-1

# two cases: n = 10, n = 100

set.seed(2-12-2021)
x1 <- 2 * rbinom(11, 1, 0.5) - 1 # to generate 1, -1 rather than 1, 0
x2 <- 2 * rbinom(101, 1, 0.5) - 1

y1 <- 5 + filter(x1, sides = 1L, filter = c(1, -.7))[-1]
y2 <- 5 + filter(x2, sides = 1L, filter = c(1, -.7))[-1]

par(mfrow = c(2, 1))
plot.ts(y1, type = "s")
plot.ts(y2, type = "s")

#type=“p”: for points (by default)
#type=“l”: for lines
#type=“b”: for both; points are connected by a line
#type=“o”: for both ‘overplotted’;
#type=“h”: for ‘histogram’ like vertical lines
#type=“s”: for stair steps
#type=“n”: for no plotting


## sample means
c(mean(y1), mean(y2))  # 5.120 4.994

acf(y1, lag.max = 4, plot = FALSE)
# the std of sample acf: 1/ sqrt(n)  =  1/ sqrt(10) = .32
# Autocorrelations of series ‘y1’, by lag
#  0      1      2      3      4 
#  1.000 -0.234 -0.501  0.238  0.223 


acf(y2, lag.max = 4, plot = F) 
# std of this sample 1/sqrt(100) =.1
# Autocorrelations of series ‘y2’, by lag
# 0      1      2      3      4 
# 1.000 -0.361 -0.155 -0.019  0.107 



acf(speech, lag.max = 250)



## Expl 1.28 SOI and Recruitment Correlation Analysis
par(mfrow = c(2, 1))
acf(soi, lag.max = 48, main = "SOI")
acf(rec, lag.max = 48, main = "Recruitment")
ccf(soi, rec, lag.max = 48, main = "SOI vs Rec", ylab = "CCF")


## Expl 1.29 Prewhittening and Cross Correlation analysis
set.seed(3-12-2021)
num = 120; t = 1:num

X = ts(2 * cos(2 * pi * t / 12) + rnorm(num), frequency =  12)
#X2 = ts(2 * cos(2 * pi * t / 12) + rnorm(num), frequency =  24)
Y = ts(2 * cos(2 * pi * (t + 5) / 12) + rnorm(num), freq = 12)
Yw = resid(lm(Y ~ cos(2 * pi * t / 12) + sin(2 * pi * t / 12),  na.action = NULL))
str(Yw)

#plot.ts(X)
#plot.ts(X2)
#plot.ts(Y)
plot.ts(Yw)


par(mfrow = c(3, 2), mgp = c(1.6, .6, 0), mar = c(3, 3, 1, 1))
plot(X)
plot(Y)
acf(X, lag.max = 48, ylab = "ACF(X)")
acf(Y, lag.max = 48, ylab = "ACF(Y)")
ccf(X, Y, 24, ylab = "CCF(X, Y)")
ccf(X, Yw, 24, ylab = "CCF(X, Yw)", ylim = c(-.5, .5))
range(ccf(X, Yw, 24)) # [1] -0.1390269  0.1516451


## from top pannel: see cyclic pattern for X and Y
## from middle : see corresponding cyclic acf
## from bottom left: show cross correlation even XY are indpt
## bottom right: X and Yw show uncorrelated




















