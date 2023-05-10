#1a)
grads <- read.csv("recent-grads.csv")
nrow(grads)
length(grads)

#b)

mean(grads$Unemployment_rate < 0.041)

#c)

education_medians <- grads$Median[grads$Major_category == "Education"]
mean(education_medians)
sd(education_medians)

#d)

noneducation_medians <- grads$Median[grads$Major_category != "Education"]
mean(noneducation_medians)
sd(noneducation_medians)

#e)

boxplot(grads$Median, ylab = "Mean Salary ($)",main = "Boxplot of Median Salaries by Major")

#2)
life <- read.csv("countries_life.csv")

plot(x = life$Income, y = life$Life, xlab = "Mean Capita Income in 1974 ($)", ylab = "Mean Life Expectancy (yrs)", main = "Life Expectancy against Per Capita Income in 1974")


boxplot(life$Income, ylab = "Mean Capita Income ($)", 
        main = "Mean Capita Income in countries in 1974")
hist(life$Income,
     xlab = "Mean Capita Income ($)",
     ylab = "Life Expectnancy (yrs)",
     main = "Life Expectancy vs. Mean Capita Income in countries in 1974")

sd(life$Income)
median(life$Income)

life_under_700 <- life[life$Income < 700,]
life_over_700 <- life[life$Income >=700,]

cor(life_over_700$Life, life_over_700$Income)

plot(x =life_over_700$Income, y = life_over_700$Life,
     main = "Life Expectancy against Income: Countries w. Income >=700",
     xlab = "Mean Income over $700 in 1974 ($)",
     ylab = "Mean Income in (yrs)")

maas <- read.table("http://www.stat.ucla.edu/~nchristo/statistics12/soil.txt", header = TRUE)

summary(maas$lead)
summary(maas$zinc)

hist(maas$zinc, breaks = 20, xlab = "Zinc in Maas River (ppm)",
     main = "Concentration of Zinc in Maas River")
hist(maas$lead, xlab = "Lead in Maas River (ppm)",
     main = "Concentration of Lead in Maas River")

hist(log(maas$zinc), xlab = "Logarithmic amount of Zinc (ppm)",
     main = "Log of zinc concentration in Maas River")
hist(log(maas$lead), xlab = "Logarithmic amount of Lead (ppm)",
         main = "Log of lead concentration in Maas River")

plot(x = log(maas$zinc), y = log(maas$lead),
     xlab = "Zinc in ppm after a log transformation",
     ylab = "lead concentration in log(ppm)",
     main = "log lead concentration against log zinc concentration")

cor(log(maas$zinc), log(maas$lead))

lead_levels <- cut(maas$lead, breaks = c(0, 120, 400, 700))

lead_colors <- c("blue", "yellow", "red")
plot(x = maas$x, y = maas$y,
     col = lead_colors[lead_levels],
     pch = 19,
     cex = maas$lead / mean(maas$lead),
    xlab = "x-coordinate",
    ylab = "y-coordinate",
    main = "Locations of Lead-contaminated Soil Along the Maas River")


LA <- read.table("http://www.stat.ucla.edu/~nchristo/statistics12/la_data.txt", header = TRUE)
library(maps)
plot(x = LA$Longitude, y = LA$Latitude,
     xlim = c(-119, -117.5),
     ylim = c(33.5, 35),
     xlab = "Longitude (deg)",
     ylab = "Latitude (deg)",
     main = "Centers of City of Los Angeles neighborhoods")
map("county", "california", add = TRUE)

LA_nonzero <- LA[LA$Schools != 0, ]
plot(LA_nonzero$Income, y = LA_nonzero$Schools,
     xlab = "Income($)",
     ylab = "Schools",
     main = "Income vs. Schools in LA neighborhoods")
     
