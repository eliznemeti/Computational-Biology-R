getwd()

birddata <-read.csv("birddata.csv", header=TRUE, sep=",")

#creating a scatter plot of all birds data energy vs. time foraging
plot(birddata$daily_energy_expenditure, birddata$time_spent_foraging)

#creating a subset for great tit data
great_tit <- subset(birddata, species == "Great_tit")

#creating a scatter plot of spec great tit energy vs. forgaging time
plot(great_tit$daily_energy_expenditure, great_tit$time_spent_foraging, main = "Relationship between Daily Energy Expenditure and Time Foraging in Great Tits", xlab="Daily Energy Expenditure Kj/d", ylab = "% of Time Spent Forgaging")

#adding line of best fit
fit <- lm(time_spent_foraging~daily_energy_expenditure, data = great_tit)
#implementing line
abline(fit, col = "green")
