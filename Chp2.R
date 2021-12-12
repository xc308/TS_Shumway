# Chp 2
# Regression and Exploratory
############################

## Expl 2.1 

str(chicken)
# Time-Series [1:180] from 2002 to 2016: 65.6 66.5 65.7 64.3 63.2 ...

summary(fit <- lm(chicken ~ time(chicken), 
   na.action = NULL))

plot(chicken, ylab = "cents per pound")
abline(fit)

# detrended series
plot(resid(fit))
# see long (5 yr) cycle 
acf(resid(fit))



## Expl 2.5 Differencing Chicken prices
# differenced cycle exhit annual cycle
# that was previously obscured in the detrened data
#


fit <- lm(chicken ~ time(chicken), na.action = NULL)
par(mfrow = c(2, 1))

plot(resid(fit), type = "o", main = "Detrended")
plot(diff(chicken), type = "o", main = "1st difference")
# see annual cycle


par(mfrow = c(3, 1))
acf(chicken, 48, main = "chicken")
acf(resid(fit), 48, main = "detrended")
acf(diff(chicken), 48, main = "1st diff")



## Expl 2.6 Differencing Global Temp
plot(globtemp, type = 'o', main = "gbtemp")
## more of rd walk than trend stationary (will be stationary after detrending),
# so better use diff than detrend

par(mfrow = c(2, 1))

plot(diff(globtemp), type = "o")
acf(diff(globtemp), 48) # minimum autocorrelation
mean(diff(globtemp))  # [1] 0.007925926


## Expl 2.7 log transform remove the nonstationarity

# variation in thickness increases in proportion 
# to the amount deposited, 
# log transform could remove the nonstationarity
# in the variance as a function of time

plot(varve, main = "marve", ylab = "")
plot(log(varve), main = "log(marve)", ylab = "")

hist(varve)
hist(log(varve))
# imporved normality



## Another preliminary tool used for visualizing
# the relation between series at different lats
# i.e. scatter matrices

# ACF tells if a substantial linear relation exists
# betw series and its own lagged values
# gives a profile of the linear correlation at
# all possible lags
# and tell which value of lag h lead to the 
# best predictability

# but mask possible nonlinear relation btw 
# current and past
# 
# Extends to two series where



## Expl 2.8 Scatterplot Matrices SOI and Recruitment

par(mfrow = c(1, 1))
lag1.plot(soi, 12)
lag2.plot(soi, rec, 8)



## Expl 2.9 Regression with lagged variables
dummy <- ifelse(soi < 0, 0, 1)
fish <- ts.intersect(rec, soiL6 = lag(soi, -6), 
             dL6 = lag(dummy, -6), dframe = T) # to combine different ts s

str(fish)
# 'data.frame':	446 obs. of  3 variables:

fit <- lm(rec ~ soiL6 * dL6, data = fish, na.action = NULL)
summary(fit) # piecewise regression

attach(fish)
plot(soiL6, rec)
lines(lowess(soiL6, rec), col = 4, lwd = 2) # lowess
points(soiL6, fitted(fit), pch = "+", col = 2)
# piecewise regression 


# resid of the piecewise regression
plot(resid(fit))
acf(resid(fit)) # not white noise



## Final exploratory tool:
# assessing periodic behavior in ts using 
# regression analysis

## Expl 2.10: Use regression to discover a signal in Noise

set.seed(11-12-2021)
t = 1:500
x <- 2 * cos(2 * pi * t / 50 + 0.6 * pi) + rnorm(500, 0, 5)
z1 <- cos(2 * pi * t / 50)
z2 <- sin(2 * pi * t / 50)

fit <- lm(x ~ 0 + z1 + z2) # 0 to exclude intercept
summary(fit)


par(mfrow = c(2, 1))
plot.ts(x)
plot.ts(x, col = 8, ylab = expression(hat(x)))
lines(fitted(fit), col = 2)

# discuss in detail in Chp4 spectral analysis



## Expl 2.11 Moving Average Smoother
# smoothing weights a0 = a+-1 =...= a+-5 = 1/12,
# a+-6 = 1/ 24

# reoves (filter out) obvious anual temp cycle
# uncovers the EL Nino cycle

wghts = c(.5, rep(1, 11), .5) / 12  # boxcar-type weights

soi_f <- filter(soi, sides = 2, filter = wghts)

plot(soi)
lines(soi_f, lwd = 2, col = 4)

par(fig = c(.65, 1, .65, 1), new = TRUE)
nwgts = c(rep(0, 20), wghts, rep(0, 20))
plot(nwgts, type = "l", ylim = c(-.02, .1), 
     xaxt = "n", yaxt = "n", ann = F)


## But the boxcar-type is too choppy
# can obtain a smoother fit using normal distr for the weigths
## Expl 2.12 Kernel Smoothing
par(mfrow = c(2, 1))
plot(soi)

lines(ksmooth(x = time(soi), soi, kernel = "normal", bandwidth = 1), lwd = 2, col = 4)

ksm <- ksmooth(time(soi), soi, "normal", bandwidth = 1)
str(ksm)
# List of 2
#$ x: num [1:453] 1950 1950 1950 1950 1950 ...
#$ y: num [1:453] 0.205 0.194 0.183 0.172 0.162 ...

par(fig = c(.65, 1, .65, 1), new = T) # insert
gauss <- function(x) {
  1 / sqrt(2 * pi) * exp(- (x^2) / 2)
}

x <- seq(-3, 3, by = 0.001)
plot(x, gauss(x), type = "l", ylim = c(-.02, 0.45),
     xaxt = "n", yaxt = "n", ann = F)

range(gauss(x)) # [1] 0.004431848 0.398942280


## Expl 2.13 Lowess: locally weighted smoothers
par(mfrow = c(1, 1))
plot(soi)
lws <- lowess(soi, f = .05)
str(lws)
# List of 2
#$ x: num [1:453] 1950 1950 1950 1950 1950 ...
#$ y: num [1:453]


lines(lowess(soi, f = .05), lwd = 2, col = 4) # 5% of data to get estimate
lines(lowess(soi, f = 2/3), lwd = 2, col = 2) # defalut fraction of data



## Expl 2.14 smoothing splines
plot(soi)
lines(smooth.spline(time(soi), soi, spar = .5), lwd = 2, col = 4)
# lambda small, emphasize El Nino cycle

lines(smooth.spline(time(soi), soi, spar = 1), lwd = 2, col = 2)
# lambda large, emphasize trend































