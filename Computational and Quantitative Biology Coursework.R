######Week 1###########
getwd()
setwd()

#importing data
birddata <-read.csv("birddata.csv", header=TRUE, sep=",")
blackbirddata <- read.csv("blackbird.csv", header = TRUE, sep=",")

#sumary for blackbrid
summary(blackbirddata$body_mass)
sd(blackbirddata$body_mass)
IQR(blackbirddata$body_mass)

table(birddata$body_mass)

#barchart
table(birddata$species)
barplot(table(birddata$species), ylim = c(0,15), col = "light blue", xlab = "Species", ylab = "Frequency", main = "Frequency of Bird Species Observed in Aviary", names = c("Blackbird", "Great Tit", "Robin", "Spotted Flycatcher"))

#histogram
hist(birddata$time_spent_foraging, ylim = c(0,15),col = "light green", xlab = "Time Spent Foraging (%)", main = "% of Time Spent Foraging by Four Bird Species")

#scatter graph
#creating subset
greattit <- subset(birddata,species == "Great_tit")
#plotting
plot (greattit$time_spent_foraging, greattit$daily_energy_expenditure, main = "Relationship Great Tit's Time Spent Foraging and Energy Expenditure", xlab="Time Spent Foraging (%)", ylab = "Daily Energy Expenditure kj/d")
#line of best fit
lobfit <- lm(daily_energy_expenditure~time_spent_foraging, data = greattit)
abline(lobfit, col = "red")

#boxplot
boxplot(birddata$time_spent_foraging ~ birddata$species, names = c("Blackbird", "Robin", "Spotted Flycatcher", "Great Tit"), xlab = "Species", ylab = "Time Spent Foraging (%)", ylim = c(0,11), col = "light blue")

######Week 2###########
#spearman test
getwd()
setwd()
#importing data
bird_data <-read.csv("bird_data.csv", header=TRUE, sep=",")
#creating subset
great_tit <- subset(bird_data,species == "Great_tit")
#correlation
cor.test(great_tit$daily_energy_expenditure, great_tit$time_spent_foraging)

#
#histogram
par(mfrow=c(2,2))
hist(great_tit$daily_energy_expenditure, breaks = 2)
qqnorm(great_tit$daily_energy_expenditure)
hist(great_tit$time_spent_foraging, breaks = 2)
qqnorm(great_tit$time_spent_foraging)

#pulls up help info
?cor.test

#pearson's test
cor.test(great_tit$daily_energy_expenditure, great_tit$time_spent_foraging, method= "pearson")
#P value has decreased and the R value is more negative
#with normality assumptions met, pearson's is more powerful

#running a chi squared test
#create a vector
Observed_bills = c(1752, 1895)  

#because the ratio expected is 1:1
Expected_bills_proportions = c(0.5, 0.5) 

chisq.test(x = Observed_bills, p = Expected_bills_proportions) 
#p value = 0.018 so it's significant (under 0.05)

#new chi squared for exercise 3 
#vectors to enter the raw data counts from figure
New_genes <-c(22,1,10)
Old_genes <-c(27,17,57)
Expected_gene_proportions <-c(1/3,1/3,1/3)
#the expected gene frequencies are the same across the 3 stages of development thus 1/3s
chisq.test(x = New_genes, p = Expected_gene_proportions) 
#the test shows these frequencies for the new genes vary depending on stage

Number_of_genes <- matrix(c(New_genes,Old_genes),ncol=3,byrow=TRUE)
#creates a table just like in the word doc

chisq.test(Number_of_genes)
# final result at 0.0001 -> the frequency of genes across developmental stages 
#differs between our new and old genes

######Week 3###########

#Part 1

#you can use tab to auto-complete objects

getwd()
setwd()
drug_data<-read.csv("DrugUptake_vs_pH.csv")
#visualize data 
View(drug_data)
head(drug_data)
#to see the entire dataset in the console just type data name
drug_data

#plotting the data where: response is drug uptake, predictor is pH
#DrugUptake and pH are the column names
#easiest/ most practical form for plotting below
plot(DrugUptake ~ pH, data=drug_data) 

#fitting the regression
#first sorting the model in object "drug_model"
drug_model<-lm(DrugUptake ~ pH,data=drug_data)

#checking assumptions
#independence: is part of the experimental design
#straight-line relationship: done in the plot I made

#normality of residuals
#extracting and storing the model residual
drug_residuals<-residuals(drug_model)

#plotting the histogram next
hist(drug_residuals, col="light blue", breaks = 3)
#appears fairly normally distributed

#qq plot 
qqnorm(drug_residuals)
#line for qq plot
qqline(drug_residuals)

#last assumption: homogeneity of variance
#extract the fitted values of the model
drug_fitted <- fitted(drug_model)
#now  plotting the residuals against the fitted values
plot(drug_residuals~drug_fitted)
#good that the cloud of points is unstructured - valid model

summary(drug_model)
coef(drug_model)

#replotting with a line of best fit
plot(DrugUptake ~ pH, data=drug_data) 
abline (a=coef(drug_model)[1], b=coef(drug_model)[2], col="red")
#an easier way producing the same line automatically
abline(drug_model, col="red")

#time to use the equation to predict drug uptake at 10.5
# predicted_uptake <- intercept + slope * pH
predicted_uptake <- coef(drug_model)[1] + coef(drug_model)[2] * 10.5
predicted_uptake

#adding the prediction to the graph
#first a vertical line
abline(v= 10.5, col="blue")
#then a horizontal line
abline(h= predicted_uptake, col="blue")

#testing the significance of the effect of pH on drug uptake
anova(drug_model)

#Part 2

getwd()
fish <- read.csv("FishHatching_Pauly_Pullin_1988.csv")
head(fish)
View (fish)
plot(HatchingTime_Days ~ Temperature_Degrees, data=fish) 
fish_model<-lm(HatchingTime_Days ~ Temperature_Degrees, data=fish)
fish_residuals<-residuals(fish_model)
hist(fish_residuals, col="light blue", breaks = 3)
#appears skewed-right

#qq plot 
qqnorm(fish_residuals)
#line for qq plot
qqline(fish_residuals)

fish_fitted <- fitted(fish_model)
#now  plotting the residuals against the fitted values
plot(fish_residuals~fish_fitted)
#there is some curvature to the data - not a straight line relationship

#data transformation
fish$LOG_HatchingTime_Days <- log(fish$HatchingTime_Days)
fish$SQRT_HatchingTime_Days <- sqrt(fish$HatchingTime_Days)
fish$squared_HatchingTime_Days <- fish$HatchingTime_Days^2

fish$LOG_Temperature_Degrees <- log(fish$Temperature_Degrees)
fish$SQRT_Temperature_Degrees <- sqrt(fish$Temperature_Degrees)
fish$squared_Temperature_Degrees <- fish$Temperature_Degrees^2

#making 6 plots
#2 rows, 3 columns
par (mfrow=c(2,3))
plot (HatchingTime_Days ~ LOG_Temperature_Degrees, data=fish)
plot (HatchingTime_Days ~ SQRT_Temperature_Degrees, data=fish)
plot (HatchingTime_Days ~ squared_Temperature_Degrees, data=fish)
plot (LOG_HatchingTime_Days ~ Temperature_Degrees, data=fish)
plot (SQRT_HatchingTime_Days ~ Temperature_Degrees, data=fish)
plot (squared_HatchingTime_Days ~ Temperature_Degrees, data=fish)
#only log_hatchingtime_days plot looks straight and should be used for refit

fish_log_model <- lm (LOG_HatchingTime_Days ~ Temperature_Degrees,data=fish) 
fish_log_residuals<-residuals(fish_log_model)
hist(fish_log_residuals, col="light blue", breaks = 10)
#appears normally distirbuted

#qq plot 
qqnorm(fish_log_residuals)
#line for qq plot
qqline(fish_log_residuals)

par (mfrow=c(1,1))
fish_log_fitted <- fitted(fish_log_model)
#now  plotting the residuals against the fitted values
plot(fish_log_residuals~fish_log_fitted)
#data looks much more ustructured - better now

#finalplot
#replotting with a line of best fit
plot(LOG_HatchingTime_Days ~ Temperature_Degrees,data=fish) 
#an easier way producing the same line automatically
abline(fish_log_model, col="red")


summary(fish_log_model)
anova(fish_log_model)
#DFs are 1 and 137


######Week 4#######

install.packages("plotrix")
install.packages("multcomp")

library(plotrix)
library(multcomp)

getwd()
setwd()
conflict <- read.csv("ant_conflict.csv")
View(conflict)
#don't forget V needs to be capital
head(conflict)
summary(conflict)
plot(attrition~treatment,data=conflict) 
plot(attrition~treatment_numeric,data=conflict)

#need to tell R that its categorical
conflict$treatment <- factor(conflict$treatment)
conflict$treatment_numeric <- factor(conflict$treatment_numeric)


plot(attrition~treatment,data=conflict) 
plot(attrition~treatment_numeric,data=conflict)
#now you should get a boxplot

plot(sqrt(attrition)~treatment,data=conflict) 
model <- lm(sqrt(attrition)~treatment,data=conflict) 

hist(residuals(model), col="light blue")
#there's a good bell curve

#qq plot 
qqnorm(residuals(model))
#line for qq plot
qqline(residuals(model), col="green")

#residuals against fitted values (to check the assumption of homogeneity of variance)
plot(residuals(model)~fitted(model), pch=16)

summary(model)
aggregate( sqrt(attrition) ~ treatment, data = conflict, FUN=mean)

#refitting the model with a different specified group instead of control
conflict$treatment_bis <- factor(conflict$treatment,levels=c("QW","WQ","QQ","WW","Control"))
model_bis <- lm(sqrt(attrition)~treatment_bis,data=conflict)
summary(model_bis)
#negative values present (because they're negative in comparison to the reference group)

#post hoc test time
library(multcomp)
pairwise <- glht(model, linfct=mcp(treatment="Tukey"))
summary(pairwise,test=adjusted("bonferroni"))

summary(pairwise,test=adjusted("BH"))

#making a final boxplot
plot(sqrt(attrition)~treatment,data=conflict)
mtext(c("a","b","b","b","b"),side=3, at = 1:5)

######week 5 exercises###

getwd()
biomass<-read.csv("soil_biomass.csv")
View(biomass)
head(biomass)
#telling R that temp is a factor
biomass$temperature <- factor(biomass$temperature)

#creating subsets for the 2 temperature treatments
subset_high <- biomass [ biomass$temperature == "High" , ]
subset_low <- biomass [ biomass$temperature == "Low" , ]

#calculate min and max values
#na.rm =T tells R to remove NA values before doing a calc
#Ymin <- min (my_data$response, na.rm =T)
#Ymax <- max (my_data$response, na.rm =T)
Ymin <- min (biomass$soil_biomass, na.rm=T)
Ymax <- max (biomass$soil_biomass, na.rm=T)
#store the minimum and maximum values of the continuous predictor into object Xmin and Xmax
Xmin <- min (biomass$nitrogen, na.rm=T)
Xmax <- max (biomass$nitrogen, na.rm=T)
#plot for high temp patches
#pch=16 makes whole points, pch=8 makes star points
#xlim and ylim give the x and y ranges
plot (soil_biomass ~ nitrogen , data = subset_high, pch=16, col="red", xlim = c(Xmin, Xmax),  ylim = c(Ymin, Ymax))
points (soil_biomass ~ nitrogen , data = subset_low, pch=8, col="blue")

#fitting the model
model <- lm (soil_biomass ~  temperature + nitrogen + temperature:nitrogen, data=biomass)

#check assumptions
#independence of data points ok
#straight line relationship - ok
#normality of residuals - ok
par(mfrow=c(1,3))
hist(residuals(model),col="grey")
qqnorm(residuals(model))
qqline(residuals(model),col="blue")
#homogeneity of variance
plot(residuals(model)~fitted(model), pch=16)

#testing the hypotheses
anova(model)

#understand the fit
summary(model)
#intercept for high temp: 15.9188
#intercept for low temp: 19.321 (15.9188+3.4022)
#slope for high temp: 4.0280
#slope for low temp: 2.1945 (4.0280-1.8335)

#final plot
par(mfrow=c(1,1))
plot (soil_biomass ~ nitrogen , data = subset_high, pch=16, col="red", xlim = c(Xmin, Xmax),  ylim = c(Ymin, Ymax))
points (soil_biomass ~ nitrogen , data = subset_low, pch=8, col="blue")
#a for intercepts b for slopes
abline (a = 15.9188, b = 4.0280, col="red")
abline (a = 19.321, b = 2.1945, col="blue")

###exercise 2###

ant_data <- read.csv("ant_habituation.csv")
View(ant_data)
head(ant_data)

#specify that the categorical variables are factors 
#specify the levels manually so that R uses “OwnColony” as a reference group 
ant_data$habituation_odour <- factor(ant_data$habituation_odour,levels=c("OwnColony","ColonyB"))
ant_data$test_fight <- factor(ant_data$test_fight, levels=c("ColonyB","ColonyC"))

#plotting
#Here we are only using the interaction
boxplot ( aggressiveness ~ habituation_odour:test_fight, data=ant_data)

#fitdata
model <- lm ( sqrt(aggressiveness) ~ habituation_odour + test_fight + habituation_odour:test_fight, data=ant_data)

#check assumptions
par(mfrow=c(1,3))
hist(residuals(model),col="grey")
qqnorm(residuals(model))
qqline(residuals(model),col="blue")
plot(residuals(model)~fitted(model), pch=16)

#testing hypotheses
anova(model)

#Testing the hypotheses: post-hoc tests
library(multcomp)
#dummy variable
ant_data$dummy_interaction <- interaction(ant_data$habituation_odour,ant_data$test_fight)
head(ant_data)

#fit dummy model
model_dummy <- lm (sqrt(aggressiveness) ~ dummy_interaction, data=ant_data)

#pairwise comparison
pairwise <- glht (model_dummy, linfct=mcp(dummy_interaction="Tukey"))

#display the post-hoc comparisons using Benjamini-Hochberg correction method 
summary(pairwise, test=adjusted("BH"))
cld(summary(pairwise, test=adjusted("BH")))
#tells you what letters to put above the categories in the boxplot

#finalplot
par(mfrow=c(1,1))
boxplot ( sqrt(aggressiveness) ~ habituation_odour:test_fight, data=ant_data)
mtext(c("b","a","b","b"), side=3, at = 1:4)
#3rd side (3) is the top side of the graph
#at = 1:4 because you have 4 groups
#use output of cld to add letters

#######Week 7########
#######Data Report 1########

setwd()
my_data<-read.csv('Report Data 1.csv') 

#attach your data to the R search path
attach(my_data)
my_data
##tips##
#can check if activity budgets of each add up to 1 (all the animal's time)

#sums columns 4 - 7 (forage + rest + travel + socialize) for each row
rowSums(my_data[,4:7]) 

#check the structure
str(my_data)

#making the variables sex and tool factors (categorical/qualitative variables)
sex<- factor(sex)
tool<- factor(tool)

#to remind yourself of the variable names, check data
head(my_data)

#refer to particular cells you can subset data where data[row number, column number], examples:
my_data[1,4] 
#variables for row 1
my_data[1,] 
#all variables in columns 4 to 7 (the activity budgets)
my_data[,4:7] 

#To refer to specific factors in a variable 
Forage[tool=="S"]
Forage[tool=="NS"]

#Part 1

#data visualization
#histograms of the original data of the activity budgets
#quick visual impression of whether the data are roughly normal or skewed
par(mfrow=c(2,2))
#to view all four histograms together
par(mfrow=c(1,1)) 
#return to just one plot in your plotting space 
hist(Forage)
hist(Rest)
hist(Travel)
hist(Socialize)

#changing axis
par(mfrow=c(1,1)) 
hist(Forage)
hist(Forage, xlim=c(0,1))
#can change the limits of the x axis
hist(Forage, ylim=c(0,25))
#change the limits of the y axis

#changing all titles
#adding a mean line
par(mfrow=c(2,2))
hist(Forage, main= "Dolphin Foraging", xlab= "Foraging", ylab="Frequency", abline(v=mean(Forage),col="red")) 
hist(Rest, main= "Dolphin Resting", xlab= "Resting", ylab="Frequency", abline(v=mean(Rest),col="red"))
hist(Travel, main= "Dolphin Travelling", xlab= "Travelling", ylab="Frequency", abline(v=mean(Travel),col="red"))
hist(Socialize, main= "Dolphin Socializing", xlab= "Socializing", ylab="Frequency", abline(v=mean(Socialize),col="red"))
#don't now why buy to see on 2,2 need to load each individually then add abline part, then move on to the next one add abline, etc

#finding the mean values
mean(Forage,na.rm=TRUE)
#0.502
mean(Rest,na.rm=TRUE)
#0.171
mean(Travel,na.rm=TRUE)
#0.171
mean(Socialize,na.rm=TRUE)
#0.076

#Part 2
#checking if each of the activity budgets differs
#if S vs NS differs with histograms, and if M and F  differ
#just for easy visualization
par(mfrow=c(2,2))
hist(Forage[tool=="S"])
hist(Forage[tool=="NS"])
hist(Forage[sex=="M"])
hist(Forage[sex=="F"])

hist(Socialize[tool=="S"])
hist(Socialize[tool=="NS"])
hist(Socialize[sex=="M"])
hist(Socialize[sex=="F"])

hist(Rest[tool=="S"])
hist(Rest[tool=="NS"])
hist(Rest[sex=="M"])
hist(Rest[sex=="F"])

hist(Travel[tool=="S"])
hist(Travel[tool=="NS"])
hist(Travel[sex=="M"])
hist(Travel[sex=="F"])

#males vs females for activity budgets
hist(Forage[sex=="M"])
hist(Forage[sex=="F"])
hist(Socialize[sex=="M"])
hist(Socialize[sex=="F"])
hist(Rest[sex=="M"])
hist(Rest[sex=="F"])
hist(Travel[sex=="M"])
hist(Travel[sex=="F"])

#sponge vs nonspongers for activity budgets
hist(Forage[tool=="S"])
hist(Forage[tool=="NS"])
hist(Socialize[tool=="S"])
hist(Socialize[tool=="NS"])
hist(Rest[tool=="S"])
hist(Rest[tool=="NS"])
hist(Travel[tool=="S"])
hist(Travel[tool=="NS"])

#GLM for forage and sex
#categorical predictor: M or F | response: Forage
par(mfrow=c(1,1))
plot(Forage~sex)
fsmodel<- lm((Forage~sex))

#checking assumptions
#fs for forage/sex
residuals(fsmodel)
hist(residuals(fsmodel))
#check skew: met
qqnorm(residuals(fsmodel))
qqline(residuals(fsmodel))
#check if points follow the line: not really
fitted(fmodel)
plot(residuals(fmodel)~fitted(fsmodel))
#non normal, one too short vs other long
#assumptions: violated -> non-parametric test

x <- Forage[sex=="M"]
y <- Forage[sex=="F"]
#wilcox test bc 1 variable and non-parametric
wilcox.test(x,y, alternative = c("two.sided"))
#P value cutoff at 0.0001
#W = 525, P< 0.0001
#time spent foraging is significantly different for M vs F dolphins

#practicing single predictor variables (not in report)#
#GLM for forage and tool
#categorical predictor: S or NS | response: Forage
par(mfrow=c(1,1))
plot(Forage~tool)
ftmodel<- lm((Forage~tool))

#checking assumptions
#ft for forage/ tool
#independence: part of the experimental design
residuals(ftmodel)
hist(residuals(ftmodel))
#check skew: met
qqnorm(residuals(ftmodel))
qqline(residuals(ftmodel))
#check if points follow the line: met
fitted(ftmodel)
plot(residuals(fmodel)~fitted(ftmodel))
#plotting residuals against the fitted values
#looks normal (the 2 opposite lines are normal with categorical predictor)
#assumptions: NOT violated -> parametric test

#assume that for some of your models the assumptions are not met and do non-paramteric test
#non-parametric equivalent of a t-test is the Wilcoxon-Mann-Whitney test 
x <- Forage[tool=="S"]
y <- Forage[tool=="NS"]
wilcox.test(x,y, alternative = c("two.sided")) 
#W = 2166, P< 0.0001
#time spent foraging is significantly different for S vs NS dolphins

#practice end#

#create a new categorical predictor variable by combining sex and tool
combined<-with(my_data, interaction(tool,  sex), drop = TRUE)
combined
#NS.F is “non-sponger female”, S.F is “sponger female”, NS.M is “non-sponger male” and S.M is “sponger male”

#GLM for combined Activity Budget: Forage
combinedfmodel<-lm(Forage~ combined)
residuals(combinedfmodel)
hist(residuals(combinedfmodel))
#check skew: met
qqnorm(residuals(combinedfmodel))
qqline(residuals(combinedfmodel))
#check if points follow the line: failed
fitted(combinedfmodel)
plot(residuals(combinedfmodel)~fitted(combinedfmodel))
#check: don't think so
#assumptions: violated -> non-parametric test

#non parametric for 2+ groups -> kruskal test
#checks if there are differences between the pairs
kruskal.test(Forage ~ combined)
#P value cutoff at 0.05
#chi-squared = 46.823, df = 3, p-value = 3.79e-10
#significant differences between the 4 groups, don’t know which pairs of groups are different

#post-hoc (pairwise) tests (looking at P values still) to differentiate which groups vary from each other
#using bonferoni correction
#P value cutoff at 0.05
pairwise.wilcox.test(Forage, combined, p.adjust.method = "bonferroni", exact= FALSE)
#     NS.F    S.F     NS.M   
#S.F  2.4e-06 -       -      
#NS.M 0.10170 8.5e-07 -      
#S.M  1.00000 0.00057 0.00278
#Pairs of groups with significance: SF + NSF, NSM + SF, SM + SF, SM + NSM

#final boxplot
par(mfrow=c(1,1))
plot(Forage~combined, ylab="foraging activity", xlab="sex (M and F) and tool users (S and NS)")
mtext(c("a","b","a","a"),side=3, at = 1:4)
#a and b are significantly different, same letter is is no signicant difference
#!confusion on SM+NSM

#GLM for combined Activity Budget: Socialize
combinedsmodel<-lm(Socialize~ combined)
residuals(combinedsmodel)
hist(residuals(combinedsmodel))
#check skew: met
qqnorm(residuals(combinedsmodel))
qqline(residuals(combinedsmodel))
#check if points follow the line: failed
fitted(combinedsmodel)
plot(residuals(combinedsmodel)~fitted(combinedsmodel))
#check: failed
#assumptions: violated -> non-parametric test

#kruskal test
kruskal.test(Socialize ~ combined)
#P value cutoff at 0.05
#chi-squared = 50.288, df = 3, p-value =6.937e-11
#significant difference

#post-hoc test
#bonferoni correction
#P value cutoff at 0.05
pairwise.wilcox.test(Socialize, combined, p.adjust.method = "bonferroni", exact= FALSE)
#     NS.F    S.F     NS.M  
#S.F  0.0241  -       -     
#NS.M 2.1e-05 1.5e-06 -     
#S.M  0.0001  5.2e-06 0.5556
#Pairs of groups with significance: SF+NSF, NSM+NSF, SM+NSF, NSM+SF, SM+SF, SM+NSM

#final boxplot
par(mfrow=c(1,1))
plot(Socialize~combined, ylab="socializing activity", xlab="sex (M and F) and tool users (S and NS)")
mtext(c("a","b","b","b"),side=3, at = 1:4)
#label with a and b
#!confusion on NSM+SF and SM+SF

#GLM for combined Activity Budget: Travel
combinedtmodel<-lm(Travel~ combined)
residuals(combinedtmodel)
hist(residuals(combinedtmodel))
#check skew: met
qqnorm(residuals(combinedtmodel))
qqline(residuals(combinedtmodel))
#check if points follow the line: failed
fitted(combinedtmodel)
plot(residuals(combinedtmodel)~fitted(combinedtmodel))
#!check: not sure
#assumptions: non parametric -> kruskal

#kruskal test
kruskal.test(Travel ~ combined)
#P value cutoff at 0.05
#chi-squared = 18.105, df = 3, p-value =0.0004185
#significant difference

#post-hoc test
#bonferoni correction
#P value cutoff at 0.05
pairwise.wilcox.test(Travel, combined, p.adjust.method = "bonferroni", exact= FALSE)
#     NS.F    S.F     NS.M   
#S.F  0.00064 -       -      
#NS.M 1.00000 0.04985 -      
#S.M  0.29278 1.00000 0.84209
#Pairs of groups with significance: SF+NSF, NSM+SF

#final boxplot
par(mfrow=c(1,1))
plot(Travel~combined, ylab="travelling activity", xlab="sex (M and F) and tool users (S and NS)")
mtext(c("a","b","a","ab"),side=3, at = 1:4)
#completed one

#GLM for combined Activity Budget: Rest
combinedrmodel<-lm(Rest~ combined)
residuals(combinedrmodel)
hist(residuals(combinedrmodel))
#check skew: met
qqnorm(residuals(combinedrmodel))
qqline(residuals(combinedrmodel))
#check if points follow the line: failed
fitted(combinedrmodel)
plot(residuals(combinedrmodel)~fitted(combinedrmodel))
#!check: failed
#assumptions: violated -> non parametric -> kruskal

#kruskal test
kruskal.test(Rest ~ combined)
#P value cutoff at 0.05
#cchi-squared = 21.195, df = 3, p-value =9.59e-05
#significant difference

#post-hoc test
#bonferoni correction
#P value cutoff at 0.05
pairwise.wilcox.test(Rest, combined, p.adjust.method = "bonferroni", exact= FALSE)
#       NS.F   S.F    NS.M  
#  S.F  0.0028 -      -     
#  NS.M 1.0000 0.0012 -     
#  S.M  0.3796 0.9006 0.0089
#significant groups: SF+NSF, NSM+SF, SM+NSM

#final boxplot
par(mfrow=c(1,1))
plot(Rest~combined, ylab="travelling activity", xlab="sex (M and F) and tool users (S and NS)")
mtext(c("a","b","a","a"),side=3, at = 1:4)
#!confusion on SM+SF and SM+NSM

#######Week 8########
#random sampling approach 
?runif
#getting x coordinates first
x_random <- runif(40, min = 0, max = 200)
#now y coords
y_random <- runif(40, min = 0, max = 100)
#dont forget to run xrandom again to pull up the table
#now for the plot
plot(y_random~x_random,asp=1)
plot(y_random~x_random)

#systematic sampling approach
?seq
x_random <- seq(from=0, to=200, by=20)
#results in 11 points along the x acis 20 meters apart
y_random <- seq(from=0, to=100, by=20)
#results in 6 points 
plot(y_random~x_random,asp=1)
#the above command runs an error because the number of points on x and y axis don't match up

Grid <- expand.grid(y_random,x_random)
#running grid brings up a table for Var 1 and Var 2 all the possible combinations
plot(Grid$Var1~Grid$Var2,asp=1)
#results in equally spread/uniform points
#need to be more deviation and randomization -> "noise" we can add noise as below

nrow(Grid)
#66 which is correct - 6x11
Noise1 <- runif(66, min = -1, max = 1)
#creates table for those ranges that can be added to y coords
Var1_noise <- Grid$Var1 + Noise1
#new variable created for y vars, Gridvar1 is the original 
Noise2 <- runif(66, min = -1, max = 1)
#now the x vars
Var2_noise <- Grid$Var2 + Noise2
#plot(Var1_noise ~ Var2_noise ,asp=1)

#adding even more noise for more randomizing up to 3 range
Noise1 <- runif(66, min = -3, max = 3)
Var1_noise <- Grid$Var1 + Noise1
Noise2 <- runif(66, min = -3, max = 3)
Var2_noise <- Grid$Var2 + Noise2
#plot(Var1_noise ~ Var2_noise ,asp=1)

#adding even more noise for more randomizing up to 6 range
Noise1 <- runif(66, min = -6, max = 6)
Var1_noise <- Grid$Var1 + Noise1
Noise2 <- runif(66, min = -6, max = 6)
Var2_noise <- Grid$Var2 + Noise2

plot(Var1_noise ~ Var2_noise ,asp=1)

#part 2
CO2 <- c(390, 600, 700, 900)
#created a vector of our treatments
CO2 <- rep(CO2 ,5)
#5 for 5 repeats
CO2 <- sample(CO2,replace=F)
#same values, just randomly shuffles values around
table(CO2)
CO2 <- sample(CO2,replace=T)
plot(CO2~c(1:20))

#using strings eg names instead of numerical values
Researchers <- c("Claire","Jane","Innes","Kerry")
Researchers <- rep(Researchers ,5)
Researchers <- sample(Researchers,replace=F)

#diff exercise
#randomising the order that you’ll process your samples
Altitude <- c(0, 500, 700, 1000, 1500, 2000)
Altitude <- rep(Altitude ,50)
Altitude <- sample(Altitude,replace=F)
plot(Altitude~c(1:300))
#try complete random block sampling it
Altitude <- c(0, 500, 700, 1000, 1500, 2000)
Altitude <- sample(Altitude,replace=F)
for(i in 1:50) sample(Altitude,replace=F)
for(i in 1:50) print(sample(Altitude,replace=F))

#save data from the loop 
Altitude_orders <- numeric()
#within our loop, we add the output from sample (Altitude,replace=F)
for(i in 1:50) Altitude_orders <- c(Altitude_orders,sample(Altitude,replace=F))
plot(Altitude_orders~c(1:300))
#run all of the 3 functions for the right plot

Altitude_orders <- c(Altitude_orders,sample(Altitude,replace=F))
Altitude_orders

#save our treatment order for processing as a csv
write.csv(Altitude_orders,"Altitude_orders.csv",row.names=F)

#######Week 9########

#Part1
getwd()
data2<-read.csv("Guppy sizes Trinidad.csv")
list.files(getwd())
#run this command to find the file name without going around on your desktop
str(data2) 
#to see what variables there are in this data frame
model1 <- lm(SBL~Predation, data=data2)
#SBL is size body length
#using the linear model
plot(model1)
anova(model1)
#result: small p value, F value over 40

#below is the Resampling section
High_sample<-sample( data2[data2$Predation=="High",]$SBL, size=20, replace=F)
Low_sample<-sample( data2[data2$Predation=="Low",]$SBL, size=20, replace=F)
#now trying with tiny sample size to see if there's a difference, here we get 20 randomly picked from the initil 
SBL_sample <- c(High_sample,Low_sample)
#should be double the number of values since it's both high and low
Predation_sample <- c( rep("High",20),rep("Low",20) )
#this is the explanatory variable for the 20 sample size, c overlays the high and low values 
model2 <- lm(SBL_sample~Predation_sample)
plot(model2)
#plotting the smaller sized sample
#checking assumptions withthe 3 plots that show up
anova(model2)
#P value is much greater due to smaller sample size, F value 

#changing the sample 20 -> 100
High_sample<-sample( data2[data2$Predation=="High",]$SBL, size=100, replace=F)
Low_sample<-sample( data2[data2$Predation=="Low",]$SBL, size=100, replace=F)
SBL_sample <- c(High_sample,Low_sample)
Predation_sample <- c( rep("High",100),rep("Low",100) )
model2 <- lm(SBL_sample~Predation_sample)
plot(model2)
anova(model2)
#low P value and higher F value

#changing the sample 100 -> 200
High_sample<-sample( data2[data2$Predation=="High",]$SBL, size=200, replace=F)
Low_sample<-sample( data2[data2$Predation=="Low",]$SBL, size=200, replace=F)
SBL_sample <- c(High_sample,Low_sample)
Predation_sample <- c( rep("High",200),rep("Low",200) )
model2 <- lm(SBL_sample~Predation_sample)
anova(model2)
#low P value and higher F value

#can swap the numerical value for an object name that represents that value, so when the object name is read, the value we’ve set is used
i <- 50
#makes it easier to replace all the numbers for something else quicker
High_sample <- sample( data2[data2$Predation=="High",]$SBL, size=i, replace=F)
Low_sample <- sample( data2[data2$Predation=="Low",]$SBL, size=i, replace=F)
SBL_sample <- c(High_sample,Low_sample)
Predation_sample <- c( rep("High",i),rep("Low",i) )
#etc

#Part 2 Automating the repetition using a loop
#interest in what is the effect of the sample size on Estimate and P val

#give an indent to distinguish what's in the loop
#Est_sample will be where we store the estimated coefficient of the difference between the low and high predation groups (as found in summary(model_name)
#P_sample will be where we store the P value from the anova() function
Est_sample  <-numeric()
P_sample    <-numeric()
#empty vectors to be filled in
for(i in 1:300) {
  High_sample <- sample( data2[data2$Predation=="High",]$SBL, size=i, replace=F)
  Low_sample <- sample( data2[data2$Predation=="Low",]$SBL, size=i, replace=F)
  SBL_sample <- c(High_sample,Low_sample)
  Predation_sample <- c( rep("High",i),rep("Low",i) )
  model2 <- lm(SBL_sample~Predation_sample)
  Est_sample[i] <- coefficients(model2)[2]
  #2.9529
  #we add 2 for the 2nd value which is predation sample instead of the 1st value which is intercept when you run coefficients(model2)
  P_sample[i]   <- anova(model2)$"Pr(>F)"[1]
  #0.1287
}
#this is 1 iteration here, can use the sidebar arrows to close and open it
#dont worry it says anova test is unreliable in red

length(P_sample)
#100
Sample_size <- 1:300
#gets a sample of 1-100
plot(Est_sample~Sample_size)
#plot each individ against the sample size
#original estimate from data before resampling using anova showed 2.3 estimate for low predation value, can see that reflected approx in the model
#this value becomes more reliable and precise in the model as sample size increases
plot(P_sample~Sample_size)
#P value decreases as sample size increase as expected 

#Part 2 Randomisations
#Part 2.1 Permutation tests
setwd()
getwd()
Penguins<-read.csv("Penguins_for_randomisations.csv")
list.files(getwd())
#Penguins is the name of the data, like earlier data2, not to be confused 

m1<-lm(flipper_length_mm ~ species, data=Penguins)
plot(m1)
#qq plot there's deviation going off at the end
hist(residuals(m1))
#histogram easily show the skew - left skew
str(data2) 
Mean_Adelie <- mean( Penguins[Penguins$species=="Adelie",]$flipper_length_mm )
#mean flipper length of adelie = 188.2
Mean_Chinstrap <- mean( Penguins[Penguins$species=="Chinstrap",]$flipper_length_mm )
#mean flipper length of chinstrap = 192.66
Observed_difference <- Mean_Chinstrap - Mean_Adelie
#4.46mm in flipper length
Penguins$Flipper_reshuffle <- sample(Penguins$flipper_length_mm,replace=F)

Resampled_difference <- numeric()
for(i in 1:1000) {
  Penguins$Flipper_reshuffle <- sample(Penguins$flipper_length_mm,replace=F)
  #Penguins$flipper_length_mm part is original data
  #adding replace=F shuffles it, F for false/ no replacement
  Shuffled_Adelie    <- mean(Penguins[Penguins$species=="Adelie",]$Flipper_reshuffle)
  Shuffled_Chinstrap <- mean(Penguins[Penguins$species=="Chinstrap",]$Flipper_reshuffle)
  Resampled_difference[i]  <- Shuffled_Chinstrap-Shuffled_Adelie
}
#resampled diff is -0.54
#should get 1000 vals when highlighting Resampled_difference
hist(Resampled_difference,xlim=c(-5,5))
abline(v=Observed_difference,col="red")
#centered around zero as we're not expecting any diff betweent he 2 species in the permutation test
#xlim=c(-5,5) is included in the histogram command to make the x axis wide enough to include the observed mean difference in flipper length
sum( abs(Resampled_difference) > abs(Observed_difference) ) /1000
#abs for absolute values/ no minus sign
#value is then divided by 1,000 (the total number of randomly generated mean differences, i.e. the total number of times you ran the loop), this gives us a P value
#= 0 probability

#Part 2.2 Bootstrapping
#to test whether the slope of the observed data is different enough to zero (i.e. where zero is no relationship between the two variables

Chinstraps <- Penguins[Penguins$species=="Chinstrap",]
#focused only on chinstrap data
#interested in whether body mass determines flipper length - our 2 variables
m1 <- lm(flipper_length_mm~body_mass_g,data=Chinstraps)
plot(m1)

Slopes_sample <- numeric()
for(i in 1:1000) {
  Chinstraps_sample <- Chinstraps[sample(1:nrow(Chinstraps),replace=T),]
  #nrow(Chinstraps) = 50 rows (minimum remember)
  model_sample  <- lm(flipper_length_mm~body_mass_g,data=Chinstraps_sample)
  #model sample = Coefficients: (Intercept) 1.848e+02, body_mass_g 2.145e-03 (here's the slope)  
  Slopes_sample[i] <- coefficients(model_sample)[2]
  #coefficient of model sample number 2 because that's body mass which is our slope
}
#sidenote:if in command line the plus keeps showing up, dont forget to add another closing ")" it should call up what you want then

hist(Slopes_sample)
#to see the skew and distribution not the actual slope we just have the number
quantile( Slopes_sample ,c(0.025,0.975))
#run the function quantile on the distribution of your resampled slopes to find the 95% confidence intervals
#between 0.025,0.975 where 95% of the data in the distribution are
#2.5%  0.002592114     97.5%  0.011193168 
#final result:the value of 0 is sufficiently far from the distribution so that the relationship wouldntve occured by chance and is significant

#######Week 10########
#######Data Report 2########

#installing package, download just once
install.packages("asnipe")
library(asnipe)
#To load a package (once it is installed)
install.packages("sna")
install.packages("igraph")

setwd()
data<-read.csv('Report Data 2.csv') 
#load social network data for your report
#dont forget: run just 'data' to see it in command line
#id column - lists the group observation ID code
#id_individual - lists the individual animal code (i.e. SAL)
#Where individuals were seen in the same group, they share a group ID code
gbi <- get_group_by_individual(data, data_format="individuals")
#transform into a group by individual matrix
gbi2 <- gbi[,order(colnames(gbi))] 
#orders the individuals (column names) alphabetically
head(gbi2)
#list first few rows of the data for you to check everything looks ok
#0’s denote that the individual (column) was not in a particular group (row name)
#1’s denote that the individual was in a particular group

#creating the adjacency matrix
network_data <- get_network(gbi2, association_index="SRI", data_format="GBI")
#created a square matrix called network_data from GBi-data-reportdata2 (indv code and group id)
#this one is undirected - look in the matrix - rows and columns are the same
#note: weight = edge thickness proportional to the association index
#association index is the number (proportion of how much interaction between indvs)
#0 for indvs never together before
write.csv(network_data, "AdjacencyMatrix.csv")
#export the adjacency matrix as a csv file
detach(package:asnipe)
#if it doesnt detach and causes an error in the next step, quit and rerun it all

#Calculating social network metrics
library(sna)
degree_metric<- degree(network_data, gmode="graph", ignore.eval=TRUE) 
#gmode is “graph” for undirected network or “digraph” for directed network
#remember DIgraph for DIrected
#ignore.eval=TRUE so we can calculate degree (total number of relationships each individual has)
#do not shorten the object name to “degree” as then it will then conflict with the function “degree”
#run degree_metric each of the 102 vals tells you how many connections/relationships each individual has
individuals<-colnames(network_data) 
#extracts just the individual names from our adjacency matrix
#assigns which number belogns to which animal in columns
data_metrics_final<-data.frame(individuals,degree_metric) 
#combine degree metrics and individual names in one data frame 
#run data metric final and see the table
strength_metric<-degree(network_data,gmode="graph",ignore.eval=FALSE)
#calculating strength centrality 
#ignore.eval set to FALSE - don’t want edge values to be ignored since “strength” sums up these vals
between_metric<-betweenness(network_data, gmode="graph")
#calculating betweenness centrality 
eigen_metric<-evcent(network_data, gmode="graph")
#calculating eigenmetric centrality 

data_metrics_final<-data.frame(individuals, degree_metric,  strength_metric, eigen_metric, between_metric)
#combined the metrics into one data frame
write.csv(data_metrics_final, "DataMetricsAll.csv")
#export it

#Stat test on strength and eigen
#strength
sdata<-data_metrics_final
combined<-with(attributes, interaction(tool,sex), drop=TRUE)
model1<-lm(strength_metric~combined, data=sdata)
summary(model1)
residuals(model1)
hist(residuals(model1))
plot(residuals(model1))
par(mfrow=c(1,1))
qqnorm(residuals(model1))
qqline(residuals(model1))
fitted(model1)
plot(fitted(model1)~residuals(model1))
#eigen

#Visualising the social network

#creating the network plot
library(igraph)
net<-graph_from_adjacency_matrix(network_data, mode="undirected", weighted=TRUE) 
##Remember our adjacency matrix is called “network_data”
#mode specifies it an “undirected” network + weighted specifies it a “weighted” network
plot.igraph(net, vertex.size=10, vertex.label.cex=0.3, edge.color="black")
#specified the node size (vertex.size) as 10 - gets bigger with higher numbers
#size of the node label (vertex.label.cex) as 0.5 - letters get huge
#vertex = node
#edge (lines) color as black
plot.igraph(net, vertex.size=10, vertex.label.cex=0.3, edge.color="black", edge.width=E(net)$weight*10) 
#‘E(net)’ refers to edges of network - gives them assigned weights (they were all the same earlier)

#node colour + shape
colors()
shapes()
#for picking colours
attributes<-read.csv('Individual attributes.csv')
#loading in file w sex and tool usage info too
V(net)[attributes$sex =="M"]$color<-"royalblue"
V(net)[attributes$sex =="F"]$color<-"darksalmon"

V(net)[attributes$tool =="S"]$shape<-"square"
V(net)[attributes$tool =="NS"]$shape<-"circle"

plot.igraph(net, vertex.col=attributes$sex, vertex.size=10, vertex.label.cex=0.3, edge.color="black", edge.width=E(net)$weight*10)
#now coloured the nodes by sex using the colours you specified above
#I personally added the diff shapes to distinguish tools and nontool users via shape()

#node size
plot.igraph(net, vertex.col=attributes$sex, vertex.size= strength_metric*10, vertex.label.cex=0.5, edge.color="black", edge.width=E(net)$weight*15)
#multiplying the strength_metric by an integer e.g. 10 makes nodes bigger
#indvs with larger nodes - higher strength value - better connected in the network
#this one runs the final plot

#adding the attributes and the data metrics together
data3<-data.frame(individuals, attributes$sex, attributes$tool, degree_metric,  strength_metric, eigen_metric, between_metric)
write.csv(data3, "AttributesAndDatametrics.csv") 

##################Experimental##################

#trying to get it to xgef for gephi
library(rgexf)
igraph.to.gexf(net)
delphi<-igraph.to.gexf(net)
write.gexf(delphi)

#for tool if doing an individual plot
#V(net)[attributes$tool =="S"]$shape<-"square"
#V(net)[attributes$tool =="NS"]$shape<-"square"
#plot.igraph(net, vertex.col=attributes$tool, vertex.size=10, vertex.label.cex=0.3, edge.color="black", edge.width=E(net)$weight*10)
#Don’t forget to change M and F to S and NS here
#plot.igraph(net, vertex.col=attributes$tool, vertex.size= strength_metric*10, vertex.label.cex=0.5, edge.color="black", edge.width=E(net)$weight*15)
#plot.igraph(net, vertex.col=attributes$tool, vertex.size= strength_metric*10, vertex.label.cex=0.5, edge.color="black", edge.width=E(net)$weight*15)

library(tcltk)
#had to download and install XQUARTZ app for this
tkplot(net, vertex.col=attributes$sex, vertex.size= strength_metric*20, vertex.label.cex=0.5, edge.color="black", edge.width=E(net)$weight*15)
#opens in quartz and you can move everythign around but doesnt add in shape differences

par(mar=c(0,0,0,0)); plot(net, asp=0)
#makes the plot take up the full window space

########## Evolutionary Biology Investigation ##########

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
















