setwd()

#####Plotting coalescent trees using R####
install.packages(c("phyclust", "ape"))

source("plot_ms_trees_single_pop.R")
#This will import the function plot_ms_tree()
plot_ms_tree()
#This function has two parameters and you can run differnt scenarios
#1 - The number of haploid samples
#2- The ms options for a single pop
#ms means "make sample" 

plot_ms_tree(10, "-T")
#Standard stationary population
#This is for a sample of 10 gene copies
#“-T” option indicates only to simulate a tree rather than sequence data

plot_ms_tree(10, "-T -G 1000")
#Population growth
#“-G” flag specifies a scaled population growth rate (bigger values, faster growth)

plot_ms_tree(100,"-T -G -2.119 -eG 2.5 0.0")
#Population decrease
#population size is increasing into the past (note negative value)

source("plot_ms_trees_mig.R")
#simulate structured populations
#imports the function plot_ms_tree_mig()

plot_ms_tree_mig(c(10, 10), 10)
#function has two parameters:
#The number of haploid samples per population
#scaled migration parameter 4Nm
#two colors corresponding to two populations
#colours correspond to the different demes from which the genes were sampled
#ms command here is for 20 individuals, 2 demes with 10 samples from each

####
#Try changing the migration rate for instance 4Nm=0.1
#You can also increase the number of populations and sample sizes


####Practical pcadapt####
#access here: https://ritarasteiro.github.io/FieldCourse/pages/PCAdapt_practical

#Reading .bed data into pcadapt
install.packages("pcadapt")
library(pcadapt)

fname <- read.pcadapt("wolf.bed",type="bed")
#reads in 107 individuals and 13092 SNPs

position.details = read.table("wolf.bim")
#reads the wolf.bim file in R
#chrom = position.details[,1]
#chrom.pos = position.details[,4]

test1 = pcadapt(fname,K=10)
plot(test1,option="screeplot")
#If we use, say, K=10, we can get a scree plot, and then can revise value of K (make sure you use upper case K)
test1 = pcadapt(fname,K=5)
#This seems to have a bit of an elbow at 6, so let’s use 5
str(test1)
#see the components of test1
raw.data= bed2matrix(fname)
#get the raw data that pcadaptis using
dim(raw.data)
#107 13092
plot(test1,option="scores")
#gives a PCA plot

#Displaying the population information
descript = read.csv("AllSamples_n107_EnvData_wLatLong_toUpload.csv")
poplist.names = descript[,4] #fourth column contains the population names
#obtain the list of population names from the csv file as
plot(test1,option="scores",pop=poplist.names)
#gives a PCA plot with population information
plot(test1,option="scores", i=1, j=4, pop=poplist.names)
#By default, the projection is done onto the first two principal components
#To plot other principal components, you need to specify the values of i and j

#Evidence of selection
plot(test1,option="manhattan")
#shows a plot of the –log10 pvalues
signif = which(-log10(test1$pvalues) > 15)
position.details[signif,]
#To look at, for example, the SNPs with –log10 p-values > 15
signif
#to see the position in your list of SNPs e.g. first SNP is at position 3981
#function (x, digits = 6)  .Primitive("signif")
position.details
#gives the ‘coordinates’ of the SNP (the chromosome and base position) in the genome assembly
get.pc(test1,signif)
#can see what principal component they have the highest correlation with by
#can look at the distribution of the allele types on the PCA plot associated with high scoring SNPs
plot(test1,option="scores", i=1, j=4, pop=poplist.names)
#remind ourselves of the geographic labels of the individuals
plot(test1$scores[,1],test1$scores[,4],col=raw.data[,3981]+1,pch=16)
#then look at the distribution of genotypes for SNP 3981, using the raw PCA data kept in the test1 object, and the genotype info we have stored in raw.data
#in the plot this SNP seems to be strongly differentiated between the High Arctic population and the others
#here black signifies the 00 homozygote, red signifies the 01 heterozygote, and green signifies the 11 homozygote
#distribution of genotypes within the areas seems broadly consistent with Hardy-Weinberg equilibrium
#can then further investigate the details of this SNP using the NCBI genome browser. To do this, use a web browser to navigate to NCBI, and then navigate to the dog genome (CanFam3.1), then search for the SNP position for the appropriate chromosome
#here: https://www.ncbi.nlm.nih.gov/genome/gdv/browser/genome/?id=GCF_000002285.3


####Project 2 ####
library(pcadapt)
fname <- read.pcadapt("prj.bed",type="bed")
#119, 52783
position.details = read.table("prj.bim")
test1 = pcadapt(fname,K=6)
plot(test1,option="screeplot")
#to identify where the elbow is via Kmax being 2-6
#elbow is at 2 in the graph, thus change test 1's K6 to K2
test1 = pcadapt(fname,K=2)
#no plot needed anymore, but create new object here
#K doesnt change the plot itself, but the P values
#K 2 means 2 clusters eg wild boards vs domesticated, or europe vs asia
raw.data= bed2matrix(fname)
dim(raw.data)
# 119 52783
plot(test1,option="scores")

#Population Information
descript = read.table("prj.fam")
poplist.names = descript[,1]
#1 because pop names are in column 1 in this case

plot(test1,option="scores",pop=new.poplist, col=colours)
#for color graph
#the nice clusters indicate similar genotypes and SNPs among the populations
#eg boars vs pigs tend to cluster in their own groups ie historical hybridization
colours=c("#FF0000", "#990000", "#000099", "#0099FF", "#0033FF", "#FF0099", "#990066", "#00CC00", "#006600", "#0053A0", "#FF6600", "#993300")
new.poplist=poplist.names
#renaming the populations (each individually)

new.poplist[which(new.poplist=="CNWB3")] ="Chinese WB"
new.poplist[which(new.poplist=="CNXI")] ="Chinese P"
new.poplist[which(new.poplist=="THWB")] ="Thai WB"
new.poplist[which(new.poplist=="THCD")] ="Thai P"
new.poplist[which(new.poplist=="KPWB")] ="Korean WB"
new.poplist[which(new.poplist=="KPKO")] ="Korean P"

new.poplist[which(new.poplist=="IBWB")] ="Portuguese WB"
new.poplist[which(new.poplist=="PTBI")] ="Portuguse P"
new.poplist[which(new.poplist=="ITWB1")] ="Italian WB"
new.poplist[which(new.poplist=="ITNS")] ="Italian P"
new.poplist[which(new.poplist=="FIWB")] ="Finnish WB"
new.poplist[which(new.poplist=="SELI")] ="Swedish P"

#Evidence of selection (detected)
plot(test1,option="manhattan")
#should look like the skyline w clear outliers
#strongly stat significant here, approx 20 interesting SNPs available (ie everything around/ above the 10-20 line)
signif = which(-log10(test1$pvalues) > 15)
#run signif to see how many values there are
#indicates the positions of the sig SNPs (above 15 in this case, can change the cutoff)
test1$pvalues[signif]
#brings up corresponding P values
#work downward from highest P value eg 5.474845e-82 to lesser, use the plot to visualize
#strong spike of selection here in the manhattan

#What genes are being selected for?
position.details[signif,]
#shows our sig coordinates to look up on NCBI
#order these in a textfile or smthn in order of most sig to least sig
#V1 is the chromosome; V4 is the base position - insert in NCBI left column
#browse not BLAST genome - use assembly older version 10.2
#can see in NCBI if the SNP is in/out of a gene or close to one
#document these in a table

#2nd plot colour based on genotype - showing frequencies of different alleles in populations
#graph of the INDIVIDUALS not SNPs
plot(test1,option="scores",pop=new.poplist, col=colours)
col=c("#FF0000", "#990000", "#0000FF", "#0099FF", "#0033FF", "#FF0099", "#990066", "#00CC00", "#006600", "#000099", "#FF6600", "#993300")
get.pc(test1,signif)
#PC = 1 means the significance belongs in PC1 
#PC1: Most important axis as it demonstrates the highest variation
#PC2: Less important axis to demonstrate the variation
#divided into PC 1 and 2 because the data is split into 2, bc K=2, so 2 components
par(mfrow=c(3,3))
plot(test1$scores[,1],test1$scores[,2],col=raw.data[,44370]+1,pch=16)
#16 is just code for a symbol
#black and green - homozygous; red is heterozygous of derived allele
# -> relabel labels PC1 and 2
plot(test1$scores[,1],test1$scores[,2],col=raw.data[,22090]+1,pch=16)
plot(test1$scores[,1],test1$scores[,2],col=raw.data[,44379]+1,pch=16)
plot(test1$scores[,1],test1$scores[,2],col=raw.data[,41821]+1,pch=16)
plot(test1$scores[,1],test1$scores[,2],col=raw.data[,16121]+1,pch=16)
plot(test1$scores[,1],test1$scores[,2],col=raw.data[,33307]+1,pch=16)
plot(test1$scores[,1],test1$scores[,2],col=raw.data[,48814]+1,pch=16)
plot(test1$scores[,1],test1$scores[,2],col=raw.data[,48815]+1,pch=16)
#selected plots for presentation: 1, 2, 3, 5
#plot 5: chinese pigs have allele being selected for
#plot 1: smthn going on w Swedish pigs, even in orginal color graph (sampling error?)

#final plots for presentation
par(mfrow=c(2,2))
plot(test1$scores[,1],test1$scores[,2],col=raw.data[,44370]+1,pch=16, cex=0.6, main= "ABLIM1", xlab= "PC1", ylab="PC2")
plot(test1$scores[,1],test1$scores[,2],col=raw.data[,22090]+1,pch=16, cex=0.6, main= "EXOC2", xlab= "PC1", ylab="PC2")
plot(test1$scores[,1],test1$scores[,2],col=raw.data[,44379]+1,pch=16, cex=0.6, main= "TRUB1", xlab= "PC1", ylab="PC2")
plot(test1$scores[,1],test1$scores[,2],col=raw.data[,16121]+1,pch=16, cex=0.6, main= "ADGRL2", xlab= "PC1", ylab="PC2")








