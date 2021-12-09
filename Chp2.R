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









