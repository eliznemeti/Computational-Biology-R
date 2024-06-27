#####prep#####

setwd()
#toevoreport in desktop

data <- read.table('darwinsfinches.csv', header = TRUE, sep =',')
head(data)
str(data)

#make the survive and location columns a factor, because R
#currently thinks they are numeric
data$survive <- factor(data$survive , levels=0:1,labels=c("Dead", "Alive"))
data$location <- factor(data$location , levels=0:1, labels=c("Daphne", "Santa"))
str(data)


#####postulate 1: Individuals within species are variable######

#calculate the mean, and standard deviation, bill depth for alive and dead birds on Daphne Major
tapply(data$daphne_all, data$survive, sd, na.rm = TRUE)
#answer :  Dead: 1.051031    Alive: 1.028621
tapply(data$daphne_all, data$survive, mean, na.rm = TRUE)
#dead: 9.590106 alive:10.085889 

#for the birds on Santa Cruz + Daphne Major
tapply(data$both_islands, data$location, mean, na.rm = TRUE)
tapply(data$both_islands, data$location, sd, na.rm = TRUE)
#means: daphne: 9.649521 santa: 10.770233 
#SDs: daphne:1.06 santa: 1.02

#Produce histograms that classify the bill depths 
min_x <- min(data$both_islands - 1)
max_x <- max(data$both_islands + 1)

par(mfrow = c(3,1))

hist(data$alive, xlim = c(min_x, max_x), xlab = "Bill Depths of Geospiza Fortis that Survived on Daphne Major (mm)", main = "A")
hist(data$dead, xlim = c(min_x, max_x), xlab = "Bill Depths of Geospiza Fortis that Died on Daphne Major (mm)", main = "B")
hist(data$santa_all, xlim = c(min_x, max_x), xlab = "Bill Depths of Geospiza Fortis on Santa Cruz (mm)", main = "C")

######postulate 2:Some of the variations are passed on to offspring#########

data$mid <- (data$mother + data$father)/2 
lin_mod <- lm (offspring ~ mid, data = data)
summary(lin_mod)
par(mfrow = c(1,1))
plot(data$mid, data$offspring,
     xlab = "Midparent Bill Depth (mm)", ylab = "Offpsring Bill Depth (mm)")
abline(lin_mod)
anova(lin_mod)

######postulate 3:More Offspring are Produced than Survive to Breed#########

#Calculating the mortality rate
survival <- table(data$survive)
mortality_rate <- survival[1]/(survival[1] + survival[2]) *
  100
names(mortality_rate) <- NULL
mortality_rate

#answer: 88.01598 -> 88.02

######postulate 4:Survival and reproduction are not random#########

# if the P value < 0.05, we can infer that the data are not normally distributed
shapiro.test(data$alive)
#answer: p-value: 0.000142 it's normal
shapiro.test(data$dead)
#answer: p-value: 2.863e-08 it's normal

#If they are,we can use the parametric two-sample t-test to test differences between 2 samples
t.test(daphne_all~survive, data = data, var.equal = T)

#answers: t = -4.2089, df = 749, p-value = 2.878e-05
#means: dead = 9.590106  alive = 10.085889

######Additional Exercise 1 #########

#means: daphne: 9.649521 santa: 10.770233 
#SDs: daphne:1.06 santa: 1.02

shapiro.test(data$daphne_all)
#answer: p-value: 1.172e-08 it's normal
shapiro.test(data$santa_all)
#answer: p-value: 0.1721 not normal

wilcox.test(both_islands~location, data = data)
#says W not U even in the lecture examples
#used to compare 2 medians of independent samples
table(data$location)
#report your sample sizes with this test:
#Daphne:751  Santa:43

######Additional Exercise 2 #########

#finding the coef var of daphne all and daphne survivors

#survivors we already know
#SD:1.028621 mean: 10.085889 

#daphne all
#SD: 1.060011 mean:9.649521

#coefs: daphne all: 10.99% and daphne suvivors: 10.2%












