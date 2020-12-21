library(splines)
library('dplyr')

# 1

# Cubic regression
fit.poly.3 = lm(Ozone ~ poly(Temp, degree = 3), data = AQ)
temp.sort = data.frame(Temp = sort(AQ$Temp))
pred.poly.3 = predict(fit.poly.3, temp.sort)


# Cubic Spline
fit.basis.5  = lm(Ozone ~ bs(Temp, df = 5), data = AQ)
fit.basis.7  = lm(Ozone ~ bs(Temp, df = 7), data = AQ)
fit.basis.9  = lm(Ozone ~ bs(Temp, df = 9), data = AQ)
fit.basis.20 = lm(Ozone ~ bs(Temp, df = 20), data = AQ)

pred.basis.5  = predict(fit.basis.5, temp.sort)
pred.basis.7  = predict(fit.basis.7, temp.sort)
pred.basis.9  = predict(fit.basis.9, temp.sort)
pred.basis.20 = predict(fit.basis.20, temp.sort)

plot(AQ$Temp,
     AQ$Ozone,
     main = "Cubic Spline and Regression for Temperature vs Ozone",
     xlab = "Temperature (F)",
     ylab = "Ozone Level")

lines(temp.sort$Temp, pred.poly.3, col = "red")

lines(temp.sort$Temp, pred.basis.5,  col = "blue")
lines(temp.sort$Temp, pred.basis.7,  col = "brown")
lines(temp.sort$Temp, pred.basis.9,  col = "green")
lines(temp.sort$Temp, pred.basis.20, col = "orange")

legend("topright", 
       legend=c("Cubic Regression", "DF = 5", "DF = 7", "DF = 9", "DF = 20"),
       col=c("red", "blue", "brown", "green", "orange"), 
       lty=1, 
       cex = 0.50)


get.MSPE = function(Y, Y.hat){
  return(mean((Y - Y.hat)^2))
}

get.MSPE(temp.sort$Temp, pred.poly.3)

get.MSPE(temp.sort$Temp, pred.basis.5)
get.MSPE(temp.sort$Temp, pred.basis.7)
get.MSPE(temp.sort$Temp, pred.basis.9)
get.MSPE(temp.sort$Temp, pred.basis.20)


# 4

fit.nat.4 = lm(Ozone ~ ns(Temp, df = 3), data = AQ)
fit.nat.5 = lm(Ozone ~ ns(Temp, df = 4), data = AQ)
fit.nat.6 = lm(Ozone ~ ns(Temp, df = 5), data = AQ)

pred.nat.4  = predict(fit.nat.4, temp.sort)
pred.nat.5  = predict(fit.nat.5, temp.sort)
pred.nat.6  = predict(fit.nat.6, temp.sort)


plot(AQ$Temp,
     AQ$Ozone,
     main = "Natural Cubic Spline for Temperature vs Ozone",
     xlab = "Temperature (F)",
     ylab = "Ozone Level")

lines(temp.sort$Temp, pred.nat.4,  col = "brown", lwd = 2)
lines(temp.sort$Temp, pred.nat.5,  col = "blue",  lwd = 2)
lines(temp.sort$Temp, pred.nat.6,  col = "green", lwd = 2)

legend("topright", 
       legend=c("DF = 3", "DF = 4", "DF = 5"),
       col=c("brown", "blue", "green"), 
       lty=1, 
       lwd=2,
       cex = 0.75)








