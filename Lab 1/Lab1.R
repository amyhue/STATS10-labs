numbers <- 1
letters <- "a"
word <- "cat"
complex_number <- 3 + 6i

integers <- 2L

vector <- c(2, 3, 5, 6)

schools <- c("UCLA", "UC Berkeley", "USC")

multiple_types <- c(2, "UCLA")

#vector <- (2, 3, 5)

vector[2]

vector[2: 3]

vector[c(1, 3)] 

vector * 2

vector - 1

NCbirths <- read.csv(file = "births2022.csv")

weights <- NCbirths$weight;print(weights)

weights_in_pounds <- weights/16 #1lb = 16 oz
weights_in_pounds[1:10]

F_age = NCbirths$Fage
Fage_mean_std = c(mean(F_age), sd(F_age)); print(Fage_mean_std)


M_smoke = tally(NCbirths$Habit); print(M_smoke)
tally(NCbirths$Habit, format = 'percent')

17 - tally(NCbirths$Habit, format = 'percent') [2]


dotPlot(weights_in_pounds)

hist(weights_in_pounds, breaks = 5)
hist(weights_in_pounds, breaks = 20)
hist(weights_in_pounds, breaks = 100)

boxplot(NCbirths$Mage, NCbirths$Fage)
summary(NCbirths$Mage)
summary(NCbirths$Fage)


histogram(~ weight | Habit, data = NCbirths, layout = c(1, 2))

tally(~Habit | MomPriorCond, data = NCbirths, format = "proportion")

tally(~BirthComp | Habit, data = NCbirths, format = "proportion")

barplot(tally(~BirthComp | Habit, data = NCbirths, format = "proportion"))

plot(x = NCbirths$Gained, y = NCbirths$weight, col = "blue",
     pch = 1, cex = 0.5, xlab = "Weight Gain by Mother (kg)",
     ylab = "Baby Birth weight (oz)",
     main = "Plot of Baby Birth Weight vs Mother's Weight Gain")
