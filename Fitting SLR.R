help(airquality)

# Ozone is the response
# Temp, Wind, Solar.R are the explanatory

# 1
aq = airquality[1:4]

pairs(aq, main = 'Figure 1: Scatterplot Matrix of Air Quality Data')

res <- cor(aq)
res

# 2
oz.solar = lm(Ozone ~ Solar.R, data = aq)
summary(oz.solar)

oz.wind = lm(Ozone ~ Wind, data = aq)
summary(oz.wind)

oz.temp = lm(Ozone ~ Temp, data = aq)
summary(oz.temp)

# plot the data with the lm() output

par(mfrow=c(2,2))

plot(aq$Solar.R, aq$Ozone,
     main = 'Figure 2. Ozone versus Solar.R',
     xlab = 'Solar.R (ppb)', ylab = 'Ozone (lang)')
abline(oz.solar, col = 'red')

plot(aq$Wind, aq$Ozone,
     main = 'Figure 3. Ozone versus Wind',
     xlab = 'Wind (mph)', ylab = 'Ozone (lang)')
abline(oz.wind, col = 'red')

plot(aq$Temp, aq$Ozone,
     main = 'Figure 4. Ozone versus Temp',
     xlab = 'Temp (F)', ylab = 'Ozone (lang)')
abline(oz.temp, col = 'red')


# 3
library(rgl)  
open3d()
plot3d(aq$Ozone ~ aq$Wind + aq$Temp, col="blue",
       xlab = 'Wind (mph)',
       ylab = 'Temp (F)',
       zlab = 'Ozone (lang',
       main = 'Figure 5. Ozone versus Wind, Temp')


# 4

oz.temp_wind =  lm(aq$Ozone ~ aq$Wind + aq$Temp)
summary(oz.temp_wind)

# b

x <- aq$Wind
y <- aq$Temp
z <- aq$Ozone

# Compute the linear regression (z = ax + by + d)
fit <- lm(z ~ x + y)

# predict values on regular xy grid
grid.lines = 26
x.pred <- seq(min(x), max(x), length.out = grid.lines)
y.pred <- seq(min(y), max(y), length.out = grid.lines)
xy <- expand.grid( x = x.pred, y = y.pred)

z.pred <- matrix(predict(fit, newdata = xy), 
                 nrow = grid.lines, ncol = grid.lines)

# fitted points for droplines to surface
fitpoints <- predict(fit)

# scatter plot with regression plane
scatter3D(x, y, z, pch = 21,
          cex = 0.5, 
          theta = 20, phi = 20, ticktype = "detailed",
          xlab = "Wind (mph)", ylab = "Temp (F)", zlab = "Ozone (lang)",  
          surf = list(x = x.pred, y = y.pred, z = z.pred), main = "Ozonee versus Wind and Temp")

