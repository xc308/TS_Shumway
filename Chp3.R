# Chp 3
########

par(mfrow = c(2, 1))

plot(arima.sim(list(order = c(0, 0, 1), ma = .9), n = 100),
     ylab = "xt", main = (expression(MA(1)~~~theta == +.9)))


plot(arima.sim(list(order = c(0, 0, 1), ma = -.9), n = 100), 
     ylab = "xt", main = (expression(MA(1)~~~theta == -.9)))





