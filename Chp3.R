# Chp 3
########

par(mfrow = c(2, 1))

plot(arima.sim(list(order = c(0, 0, 1), ma = .9), n = 100),
     ylab = "xt", main = (expression(MA(1)~~~theta == +.9)))


plot(arima.sim(list(order = c(0, 0, 1), ma = -.9), n = 100), 
     ylab = "xt", main = (expression(MA(1)~~~theta == -.9)))



## ARMA(p, q)
## Expl 3.7 Parameter Redundancy

set.seed(07-15-2021) # Mars, I got your bday
x <- rnorm(150, mean = 15)
arima(x, order = c(1, 0, 1))


## Example 3.12 Weights for an ARMA Model
inf_MA_lst<- ARMAtoMA(ar = .9, ma = .5, 50) # convert ARMA to infinite MA
plot(inf_MA_lst)








