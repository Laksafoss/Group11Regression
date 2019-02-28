library(ggplot2)
library(gridExtra)
library(dplyr)
library(Hmisc)
library(corrplot)
library(UpSetR)

narwhal <- readRDS(file = "outputs/narwhal.RDS")

#Combinations of missing values plots
gg_miss_upset(narwhal)
gg_miss_upset(narwhal, nsets = n_var_miss(narwhal))

gg_miss_var(narwhal, facet = Phase)


summary(narwhal$Dist.to.Paamiut[narwhal$Phase=="B"])
A <- summary(narwhal$Phase[!is.na(narwhal$Dist.to.Paamiut)])
B <- summary(narwhal$Phase)
(B-A)/B*100

# Percentage of existing Dist.to.Paamiut observatons for Helge for each phase
C <- summary(narwhal$Phase[is.na(narwhal$Dist.to.Paamiut) & narwhal$Ind == "Helge"])
D <- summary(narwhal$Phase[narwhal$Ind == "Helge"])
pctH <- round((D-C)/D*100,2)

# Percentage of existing Dist.to.Paamiut observatons for Thor for each phase
G<- summary(narwhal$Phase[is.na(narwhal$Dist.to.Paamiut) & narwhal$Ind == "Thor"] )
H<-summary(narwhal$Phase[narwhal$Ind == "Thor"])
pctT<-round((H-G)/H*100,2)


#Table for above percentages
a<-do.call(rbind, Map(data.frame, Helge = pctH, Thor = pctT))
png(filename = "PercentageTable.png")
grid.table(a) 
dev.off()


#Is at least one whale in LOS for non-missing values of dist
summary(narwhal$Los[!is.na(narwhal$Dist.to.Paamiut)])


#Different summary for  
summary(narwhal$Seismik[narwhal$Los==0])

summary(narwhal$Los[narwhal$Phase!= "B"])

summary(as.matrix(diff(narwhal$Datetime[narwhal$Ind == "Helge"])))


#Undersøgelse af tid og phases
summary(narwhal$Datetime[narwhal$Phase == "B"])  #B
summary(narwhal$Datetime[narwhal$Phase == "I0"]) #I0
summary(narwhal$Datetime[narwhal$Phase == "I3"]) #T0
summary(narwhal$Datetime[narwhal$Phase == "T0"]) #I1
summary(narwhal$Datetime[narwhal$Phase == "T3"]) #T1
summary(narwhal$Datetime[narwhal$Phase == "I1"]) #I2
summary(narwhal$Datetime[narwhal$Phase == "I4"]) #T2
summary(narwhal$Datetime[narwhal$Phase == "T1"])
summary(narwhal$Datetime[narwhal$Phase == "T4"])
summary(narwhal$Datetime[narwhal$Phase == "I2"])
summary(narwhal$Datetime[narwhal$Phase == "I5"])
summary(narwhal$Datetime[narwhal$Phase == "T2"])
summary(narwhal$Datetime[narwhal$Phase == "T5"])
difftime("2017-08-11 12:22:00", "2017-08-19 11:49:26")
summary(narwhal)

ggplot(narwhal, aes(Phase, Datetime, group = Ind) ) + geom_point()

sapply(levels(narwhal$Phase), function(x) summary(narwhal$Seismik[narwhal$Phase == x]))


