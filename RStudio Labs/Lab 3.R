grads <- read.csv("recent-grads.csv")
full_men_lm <- lm(Full_time ~ Men, data = grads)
summary(full_men_lm)

plot(x= grads$Men, y = grads$Full_time,
     xlab = "Number of Male Graduates",
     ylab = "Number of Full-time Graduates",
     main = "Regression of Full-time employment vs. Male Graduates")
abline(full_men_lm, col = "red")

full_men_residuals <- full_men_lm$residuals
plot(x = grads$Men, y = full_men_residuals,
     xlab = "Number of Male Graduates",
     ylab = "Residual",
     main = "Residual plot for Full-time vs Men Regression")
abline(h = 0, col = "red")

500*1.363

ice <- read.csv("sea_ice.csv", header = TRUE) 
ice$Date <- as.Date(ice$Date, "%m/%d/%Y")

ice_lm <- lm(Extent ~ Date, data = ice)
summary(ice_lm)

plot(x = ice$Date, y = ice$Extent, type = "l")
abline(ice_lm, col = "red")

plot(x = ice$Date, y = ice_lm$residuals, type = "l")
abline(h = 0, col = "red")

#b
set.seed(123)
rolls <- replicate(10000, sample(1:6, size = 2, replace = TRUE))

class(rolls)
sums <- colSums(rolls)
barplot(table (sums)/10000)

#c

mean(sums == 4 | sums == 11)

mean(sums == 2 | sums == 5 | sums == 12)

#e

