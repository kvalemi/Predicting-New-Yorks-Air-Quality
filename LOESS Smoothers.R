# read in the helper functions
source('Helper Functions - Main.R')


# 1a

# Smoothing Splines between Temp and Ozone
fit.smooth.5 = smooth.spline(x = AQ$Temp, y = AQ$Ozone, df = 5)
fit.smooth.7 = smooth.spline(x = AQ$Temp, y = AQ$Ozone, df = 7)
fit.smooth.9 = smooth.spline(x = AQ$Temp, y = AQ$Ozone, df = 9)
fit.smooth.20 = smooth.spline(x = AQ$Temp, y = AQ$Ozone, df = 20)

with(AQ, 
     plot(Temp, Ozone,
     main = "Smoothing Splines for Ozone Air Quality Data",
     xlab = "Temperature",
     ylab = "Ozone",
     pch = 19))

legend("topright",
       title = "Smooth Spline DF",
       legend = c("5","7","9","20"),
       col = c("red", "blue", "green", "orange"), lwd = 2)

lines(fit.smooth.5,  col = "red", lwd=2)
lines(fit.smooth.7,  col = "blue", lwd=2)
lines(fit.smooth.9,  col = "green", lwd=2)
lines(fit.smooth.20, col = "orange", lwd=2)



# 1b)

AQ_Unique = AQ %>% distinct(Temp, .keep_all = T)

fit.smooth.CV = smooth.spline(x = AQ$Temp, y = AQ$Ozone, cv=T) # Use CV
fit.smooth.GCV = smooth.spline(x = AQ$Temp, y = AQ$Ozone, cv=F) # Use GCV

# plot the optimal splines
with(AQ, 
     plot(Temp, Ozone,
          main = "Optimal Smoothing Splines for Ozone Air Quality Data",
          xlab = "Temperature",
          ylab = "Ozone",
          pch = 19))

legend("topright",
       title = "Optimal Smooth Spline DF",
       legend = c("CV", "GCV"),
       col = c("red", "blue"), 
       lwd = 1,
       cex= 0.5)

lines(fit.smooth.CV,  col = "red", lwd=1)
lines(fit.smooth.GCV,  col = "blue", lwd=1)



# 2 LOESS

fit.loess.5 = loess(Ozone ~ Temp, data = AQ,   enp.target = 5)
fit.loess.7 = loess(Ozone ~ Temp, data = AQ,   enp.target = 7)
fit.loess.9 = loess(Ozone ~ Temp, data = AQ,   enp.target = 9)
fit.loess.20 = loess(Ozone ~ Temp, data = AQ,  enp.target = 20)

min.temp = min(AQ$Temp)
max.temp = max(AQ$Temp)

vals.temp.raw = seq(from = min.temp, to = max.temp, length.out = 100)
vals.temp = data.frame(Temp = vals.temp.raw)

pred.temp.fit.5  = predict(fit.loess.5, vals.temp)
pred.temp.fit.7  = predict(fit.loess.7, vals.temp)
pred.temp.fit.9  = predict(fit.loess.9, vals.temp)
pred.temp.fit.20 = predict(fit.loess.20, vals.temp)

with(AQ, 
     plot(Temp, Ozone,
          main = "LOESS Smoothers for Ozone Air Quality Data",
          xlab = "Temperature",
          ylab = "Ozone",
          pch = 19))

legend("topright",
       title = "LOESS DF",
       legend = c("5","7","9","20"),
       col = c("red", "blue", "green", "orange"), lwd = 2)

lines(x = vals.temp$Temp, y = pred.temp.fit.5,  col = "red", lwd=2)
lines(x = vals.temp$Temp, y = pred.temp.fit.7,  col = "blue", lwd=2)
lines(x = vals.temp$Temp, y = pred.temp.fit.9,  col = "green", lwd=2)
lines(x = vals.temp$Temp, y = pred.temp.fit.20, col = "orange", lwd=2)





