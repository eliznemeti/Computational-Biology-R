###making a simple whittaker plot in R###

#set working directory

mydata<- read.delim("mydata.txt")

site1<-mydata[order(-mydata$P1), ]

plot (site1$P1, type="o", xlim=c(1,14), ylim = c(0,0.6), lwd=3, col="green", pch=19, cex=1, xlab="Rank Abundance", ylab = "Proportion of Abundance")

site2<- mydata[order(-mydata$P2), ]

lines (site2$P2, type="o", lwd=3, col="orange", pch=19, cex=1, )

legend (9, 0.6, legend=c("Serengeti", "De Hogue"), col=c(255,11), lty=1:1, pch=19:19)