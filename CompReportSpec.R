########Data Report: Week 7#######
setwd()
my_data<-read.csv('Report Data 1.csv') 

#attach your data to the R search path
attach(my_data)
my_data

#making the variables sex and tool factors (categorical/qualitative variables)
sex<- factor(sex)
tool<- factor(tool)

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
hist(Forage, main=NA, xlab= "Proportion of Time Foraging", ylab="Frequency", abline(v= 0.50206, col="red")) 
hist(Socialize, main=NA, xlab= "Proportion of Time Socializing", ylab="Frequency", abline(v= 0.07586857, col="red"))
hist(Rest, main=NA, xlab= "Proportion of Time Resting", ylab="Frequency", abline(v= 0.1706563, col="red"))
hist(Travel, main=NA, xlab= "Proportion of Time Travelling", ylab="Frequency", abline(v= 0.1714869, col="red"))

mean(Forage) #0.50206
mean(Socialize) #0.07586857
mean(Travel) #0.1714869
mean(Rest) #0.1706563

abline(v=mean(Forage),col="red")
#don't now why buy to see on 2,2 need to load each individually then add abline part, then move on to the next one add abline, etc

#GLMs for Activity Budgets
#used combined dataset
#create a new categorical predictor variable by combining sex and tool
combined<-with(my_data, interaction(tool,  sex), drop = TRUE)
combined
write.csv(combined, "combined.csv")
#Variables: NS.F is “non-sponger female”, S.F is “sponger female”, NS.M is “non-sponger male” and S.M is “sponger male”

########GLM for combined Activity Budget: Forage#####
#step 1: plotting the data
plot(Forage~ combined)
#step 2: fitting the model
combinedfmodel<-lm(Forage~ combined)
par(mfrow=c(1,3))
#step 3: checking assumptions
#checking normal residuals
residuals(combinedfmodel)
hist(residuals(combinedfmodel))
#skew: met
qqnorm(residuals(combinedfmodel))
qqline(residuals(combinedfmodel))
#points follow the line: failed
#checking homogeneity of variance
fitted(combinedfmodel)
plot(residuals(combinedfmodel)~fitted(combinedfmodel))
#check: don't think so
#extra: quantitative double-check with Shapiro
shapiro.test(residuals(combinedfmodel))
#p-value = 0.0004006 - not normal
#assumptions: violated -> non-parametric test

#non parametric for 2+ groups -> kruskal test
#checks if there are differences between the pairs
kruskal.test(Forage ~ combined)
#P value cutoff at 0.05
#chi-squared = 46.823, df = 3, p-value = 3.79e-10
#significant differences between the 4 groups, don’t know which pairs of groups are different

#post-hoc (pairwise) tests (looking at P values still) to differentiate which groups vary from each other
#using bonferoni correction
#P value cutoff at max 0.05
pairwise.wilcox.test(Forage, combined, p.adjust.method = "bonferroni", exact= FALSE)
#     NS.F    S.F     NS.M   
#S.F  2.4e-06 -       -      
#NS.M 0.10170 8.5e-07 -      
#S.M  1.00000 0.00057 0.00278
#Pairs of groups with significance: SF + NSF, NSM + SF, SM + SF, SM + NSM

#final boxplot
par(mfrow=c(1,1))
plot(Forage~combined, ylab="Proportion of Time Foraging", xlab="sex (M and F) and tool users (S and NS)")
mtext(c("a","b","a","ac"),side=3, at = 1:4)
#a and b are significantly different, same letter is is no signicant difference

#calculating boxplots medians
median(Forage[combined=="NS.F"]) #0.4008197
median(Forage[combined=="S.F"]) #0.6666667
median(Forage[combined=="NS.M"]) #0.3269231
median(Forage[combined=="S.M"]) #0.4545455

######GLM for combined Activity Budget: Socialize######
#step 1: plotting the data
plot(Socialize~ combined)
#step 2: fitting the model
combinedsmodel<-lm(Socialize~ combined)
par(mfrow=c(1,3))
#step 3: checking assumptions
#checking normal residuals
residuals(combinedsmodel)
hist(residuals(combinedsmodel))
#skew: met
qqnorm(residuals(combinedsmodel))
qqline(residuals(combinedsmodel))
#points follow the line: failed
#checking homogeneity of variance
fitted(combinedsmodel)
plot(residuals(combinedsmodel)~fitted(combinedsmodel))
#check: failed
#extra: quantitative double-check with Shapiro
shapiro.test(residuals(combinedsmodel))
#p-value = 0.0005276 - not normal
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
plot(Socialize~combined, ylab="Proportion of Time Socializing", xlab="sex (M and F) and tool users (S and NS)")
mtext(c("a","b","c","c"),side=3, at = 1:4)

#boxplot medians
median(Socialize[combined=="NS.F"]) #0.05263158
median(Socialize[combined=="S.F"]) #0
median(Socialize[combined=="NS.M"]) #0.2235294
median(Socialize[combined=="S.M"]) #0.1388889

#######GLM for combined Activity Budget: Travel#####
#step 1: plotting the data
plot(Travel~ combined)
#step 2: fitting the model
combinedtmodel<-lm(Travel~ combined)
par(mfrow=c(1,3))
#step 3: checking assumptions
#checking normal residuals
residuals(combinedtmodel)
hist(residuals(combinedtmodel))
#skew: met
qqnorm(residuals(combinedtmodel))
qqline(residuals(combinedtmodel))
#check if points follow the line: met
#checking homogeneity of variance
fitted(combinedtmodel)
plot(residuals(combinedtmodel)~fitted(combinedtmodel))
#!check: not sure
#extra: quantitative double-check with Shapiro
shapiro.test(residuals(combinedtmodel))
#p-value = 0.4824 - normal
#assumptions: met -> parametric test

#one way ANOVA test
#P value cut off is p<0.05
anova(combinedtmodel)
#          Df  Sum Sq  Mean Sq F value    Pr(>F)    
#combined   3 0.15686 0.052286   7.938 8.595e-05 ***
#Residuals 98 0.64551 0.006587 
#P value = significant
summary(combinedtmodel)

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
plot(Travel~combined, ylab="Proportion of Time Travelling", xlab="sex (M and F) and tool users (S and NS)")
mtext(c("a","b","a","ab"),side=3, at = 1:4)

median(Travel[combined=="NS.F"]) #0.1960784
median(Travel[combined=="S.F"]) #0.1339286
median(Travel[combined=="NS.M"]) #0.1904762
median(Travel[combined=="S.M"]) #0.15

#######GLM for combined Activity Budget: Rest#####
#step 1: plotting the data
plot(Rest~ combined)
#step 2: fitting the model
combinedrmodel<-lm(Rest~ combined)
par(mfrow=c(1,3))
#step 3: checking assumptions
#checking normal residuals
residuals(combinedrmodel)
hist(residuals(combinedrmodel))
#check skew: its okay
qqnorm(residuals(combinedrmodel))
qqline(residuals(combinedrmodel))
#check if points follow the line: failed, barely
#checking homogeneity of variance
fitted(combinedrmodel)
plot(residuals(combinedrmodel)~fitted(combinedrmodel))
#check: failed
#extra: quantitative double-check with Shapiro
shapiro.test(residuals(combinedrmodel))
#p-value = 0.01182 - not normal
#assumptions: violated -> non parametric -> kruskal

#kruskal test
kruskal.test(Rest ~ combined)
#P value cutoff at 0.05
#chi-squared = 21.195, df = 3, p-value =9.59e-05
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
plot(Rest~combined, ylab="Proportion of Time Resting", xlab="sex (M and F) and tool users (S and NS)")
mtext(c("ac","b","a","bc"),side=3, at = 1:4)

median(Rest[combined=="NS.F"]) #0.1999721
median(Rest[combined=="S.F"]) #0.09090909
median(Rest[combined=="NS.M"]) #0.2068966
median(Rest[combined=="S.M"]) #0.1351351

########Data Report: Week 10#######
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
strength_metric<-degree(network_data,gmode="graph",ignore.eval=FALSE)
#calculating strength centrality 
#ignore.eval set to FALSE - don’t want edge values to be ignored since “strength” sums up these vals
between_metric<-betweenness(network_data, gmode="graph")
#calculating betweenness centrality 
eigen_metric<-evcent(network_data, gmode="graph")
#calculating eigenmetric centrality 

#combined all the metrics into one data frame
data_metrics_final<-data.frame(individuals, degree_metric,  strength_metric, eigen_metric, between_metric)
write.csv(data_metrics_final, "DataMetricsAll.csv")
#export it

#GLMs on Node Metrics
#######GLM for strength######
metricsdata<-data_metrics_final
combined<-with(attributes, interaction(tool,sex), drop=TRUE)

#step 1: plotting the data
plot(strength_metric~ combined)
#step 2: fitting the model
strengthmodel<-lm(strength_metric~combined, data=metricsdata)
#step 3: checking assumptions
#checking normal residuals
par(mfrow=c(1,3))
residuals(strengthmodel)
hist(residuals(strengthmodel))
#check skew: failed
qqnorm(residuals(strengthmodel))
qqline(residuals(strengthmodel))
#check: failed
#checking homogeneity of variance
fitted(strengthmodel)
plot(fitted(strengthmodel)~residuals(strengthmodel))
#!check: not sure
#extra: quantitative double-check with Shapiro
shapiro.test(residuals(strengthmodel))
#p-value = 0.004974 - not normal
#assumptions: violated -> transformation doesn't work -> non-parametric

#kruskal test
kruskal.test(strength_metric~combined)
#P value cutoff at max0.05
#cchi-squared = chi-squared = 42.132, df = 3, p-value =3.762e-09
#significant difference

#post-hoc test
#bonferoni correction
#P value cutoff at max 0.05
pairwise.wilcox.test(strength_metric, combined, p.adjust.method = "bonferroni", exact= FALSE)
#     NS.F   S.F     NS.M  
#S.F  0.0156 -       -     
#NS.M 0.0419 2.2e-06 -     
#S.M  0.0054 3.8e-06 1.0000
#significant groups: all except SM+NSM

#final boxplot
par(mfrow=c(1,1))
plot(strength_metric~combined, ylab="Social Strength", xlab="sex (M and F) and tool users (S and NS)")
mtext(c("a","b","c","c"),side=3, at = 1:4)

median(strength_metric[combined=="NS.F"]) #0.390141
median(strength_metric[combined=="S.F"]) #0.2641827
median(strength_metric[combined=="NS.M"]) #0.9186816
median(strength_metric[combined=="S.M"]) #1.041497

#######GLM for Eigenvector#######
#step 1: plotting the data
plot(eigen_metric~ combined)
#step 2: fitting the model
eigenmodel<-lm(eigen_metric~combined, data=metricsdata)
#step 3: checking assumptions
#checking normal residuals
par(mfrow=c(1,3))
residuals(eigenmodel)
hist(residuals(eigenmodel))
#check skew: failed
qqnorm(residuals(eigenmodel))
qqline(residuals(eigenmodel))
#check: failed
#checking homogeneity of variance
fitted(eigenmodel)
plot(fitted(eigenmodel)~residuals(eigenmodel))
#check: failed
#extra: quantitative double-check with Shapiro
shapiro.test(residuals(eigenmodel))
#p-value = 2.091e-13 - not normal
#assumptions: violated -> non parametric

#kruskal test
kruskal.test(eigen_metric~combined)
#P value cutoff at max0.05
#chi-squared = 37.079, df = 3, p-value =4.428e-08
#significant difference

#post-hoc test
#bonferoni correction
#P value cutoff at max 0.05
pairwise.wilcox.test(eigen_metric, combined, p.adjust.method = "bonferroni", exact= FALSE)
#     NS.F    S.F     NS.M   
#S.F  0.10    -       -      
#NS.M 0.28    1.00    -      
#S.M  7.4e-07 5.9e-06 4.7e-05
#significant groups: SM+NSF, SM+SF, SM+NSM

#final boxplot
par(mfrow=c(1,1))
plot(eigen_metric~combined, ylab="Eigenvector Metric", xlab="sex (M and F) and tool users (S and NS)")
mtext(c("","","",""),side=3, at = 1:4)

median(eigen_metric[combined=="NS.F"]) #0.001178236
median(eigen_metric[combined=="S.F"]) #0.00347875
median(eigen_metric[combined=="NS.M"]) #0.004549628
median(eigen_metric[combined=="S.M"]) #0.1459507

#########Visualising the social network#######

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

#node size (and final plot)
plot.igraph(net, vertex.col=attributes$sex, vertex.size= strength_metric*7, vertex.label.cex=0.5, edge.color="black", edge.width=E(net)$weight*15)
#multiplying the strength_metric by an integer e.g. 10 makes nodes bigger
#indvs with larger nodes - higher strength value - better connected in the network
#this one runs the final plot

#######Calculating assortment on continuous vertex values#####
#Test for assortment as weighted network undirected
NumTool<-as.numeric(factor(attributes$tool))
#making tool numerical
assortativity(net, NumTool, types2 = NULL, directed = FALSE)
#0.6506706
#compare to near 50/50 split down the graph

Numsex<-as.numeric(factor(attributes$sex))
#making sex numerical
assortativity(net, Numsex, types2 = NULL, directed = FALSE)
#0.1184031
#compare to near graph

#adding the attributes and the data metrics together
fulldata<-data.frame(individuals, attributes$sex, attributes$tool, degree_metric,  strength_metric, eigen_metric, between_metric)
write.csv(fulldata, "AttributesAndDatametrics.csv")


