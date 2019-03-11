library(ggplot2)
library(gridExtra)
library(dplyr)
library(Hmisc)
library(corrplot)
library(UpSetR)
library(naniar)
library(lubridate)

narwhal <- readRDS(file = "outputs/narwhal_modified.RDS")

#Combinations of missing values plots
png(filename = "figs/Missing_Values_Overview.png")
gg_miss_upset(narwhal)
dev.off()

udenD <- within(narwhal, rm(Dist.to.Paamiut, Acou.qua, Area, Buzz, Call, Click, Date, Datetime, Hour, Depth, Diving, Los, Ind, Minut, ODBA, VeDBA, Seismik, Strokerate, Sun))
png(filename = "figs/Missing_values_phase.png")
gg_miss_var(udenD, facet = Phase)
dev.off()

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
a<- t(a)
png(filename = "figs/PercentageTable.png")
grid.table(a) 
dev.off()

#Table for mean dist in phases
b<- sapply(levels(narwhal$Phase), function(x) mean(narwhal$Dist.to.Paamiut[!is.na(narwhal$Dist.to.Paamiut) & narwhal$Ind == "Helge" & narwhal$Phase == x]))
b


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

#ggplot(narwhal, aes(Phase, Datetime, group = Ind) ) + geom_point()


#Phase = Seismik
sapply(levels(narwhal$Phase), function(x) summary(narwhal$Seismik[narwhal$Phase == x & narwhal$Ind == "Helge"]))


#Length of Phases in seconds
sapply(levels(narwhal$Phase), function (x) 
            difftime(max(narwhal$Datetime[narwhal$Phase == x & narwhal$Ind == "Helge"]),
         min(narwhal$Datetime[narwhal$Phase == x & narwhal$Ind == "Helge"]), units = "secs"))



plot(narwhal$Datetime[narwhal$Phase == "T2"], narwhal$Seismik[narwhal$Phase == "T2"])

