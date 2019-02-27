library(lubridate) # for date and times
library(plyr)
library(dplyr) # general purpose data manipulation
narwhal <- readRDS(file = "data/narwhal.RDS")

## Data wrangling
# Change all variables to have capitalized start letter
firstletterupper <- sapply(names(narwhal), function(x) toupper(substring(x,1,1)))
lastletters <- sapply(names(narwhal), function(x) substring(x,2,nchar(x)))
names(narwhal) <- paste0(firstletterupper, lastletters)
# change Strokerate to integer
narwhal$Strokerate <- as.integer(narwhal$Strokerate)
# Add a single datetime object
datetimes <- paste(narwhal[,1], narwhal[,2], sep = " ")
narwhal$Datetime <- parse_date_time(datetimes,"Ymd HMS")
# Correct for positve depths (by substracting maximum value)
narwhal$Depth <- narwhal$Depth - max(narwhal$Depth)
# Order Phase
levels(narwhal$Phase) <- factor(c("B", "I0", "T0", "I1","T1", "I2", "T2", "I3", "T3", "I4", "T4", "I5","T5"), ordered = TRUE)
# Create new (and correct) Phase
NewPhases <- c("B", "I0", "I3", "T0","T3", "I1", "I4", "T1", "T4", "I2", "I5", "T2","T5")
narwhal$NewPhase <- mapvalues(narwhal$Phase,
                              from =levels(narwhal$Phase),
          to = NewPhases)
levels(narwhal$NewPhase) <- factor(c("B", "I0", "I3", "T0","T3", "I1", "I4", "T1", "T4", "I2", "I5", "T2","T5"), ordered = TRUE)

#narwhal %>% group_by(Phase) %>% summarise(start = min(Datetime)) %>% arrange(start)
# Change "-1" to NA in Long, Lat and dist.*
narwhal <- narwhal %>% mutate(Long = na_if(Long,-1),
                   Lat = na_if(Lat,-1),
                   Dist.to.Paamiut = na_if(Dist.to.Paamiut,-1),
                   Dist.to.shore = na_if(Dist.to.shore,-1),
                   ODBA = na_if(ODBA,-1),
                   VeDBA = na_if(VeDBA,-1),
                   Strokerate = na_if(Strokerate,-1))

# Duplicates
duplicates1 <- which(duplicated(narwhal[,c("Datetime","Ind")]))
duplicates2 <- which(duplicated(narwhal[,c("Datetime","Ind")],fromLast = TRUE))
dupsId <- c(duplicates1, duplicates2)
dups <- narwhal[dupsId,] %>% arrange(Ind, Datetime)  
## save dups
saveRDS(dups, file = "outputs/duplicates.RDS")
newrows <- dups[9:12,]
newrows[1,"Dist.to.Paamiut"] <- mean(newrows[1:2,"Dist.to.Paamiut"])  
newrows[3,"Dist.to.Paamiut"] <- mean(newrows[3:4,"Dist.to.Paamiut"])  
# Remove dupls (use mean when different Dists)
narwhal <- narwhal[-dupsId,]
narwhal <- rbind(newrows[c(1,3),],narwhal)
saveRDS(newrows[c(1,3),], file = "outputs/duplicates.RDS")
# Add binary varbiale indicating if both whales are present
narwhal <- narwhal %>% group_by(Datetime) %>% mutate(BothWhales = all(c("Helge","Thor") %in% Ind))
# max Depth
DepthCutoff <- -10
narwhal$Diving <- narwhal$Depth < DepthCutoff
findDives <- function(Diving) {
  Diving <- as.vector(Diving)
  DiveNumber <- 0
  IsDiving <- Diving[1]
  Dives <- rep(NA, NROW(Diving))
  for(i in 1:NROW(Diving)) {
  if (Diving[i]) {
    if(!IsDiving) {
      DiveNumber <- DiveNumber + 1
      IsDiving <- TRUE
    }
    Dives[i] <- DiveNumber 
  } else {
    IsDiving <- FALSE
    }
  }
  Dives
}

narwhal <- narwhal %>% group_by(Ind) %>% arrange(Datetime) %>% mutate(DiveNumber = findDives(Diving))
narwhal <- narwhal %>% group_by(Ind, DiveNumber) %>% mutate(DiveDepth = ifelse(is.na(DiveNumber),NA,min(Depth))) %>% arrange(Ind, Datetime)

narwhal_summarised <- narwhal %>% group_by(Ind, DiveNumber) %>% summarise(MaxDepth = max(Depth),
                                                               MeanDepth = mean(Depth),
                                                               DatetimeStart = min(Datetime),
                                                               DiveEnd = max(Datetime),
                                                               CallsSum = sum(as.numeric(Call)),
                                                               BuzzSum = sum(as.numeric(Call)),
                                                               ODBAMax = max(ODBA),
                                                               ODBAMean = mean(ODBA),
                                                               VeDBA = max(VeDBA),
                                                               VeDBAMean = mean(VeDBA),
                                                               StrokeRateMax = max(Strokerate),
                                                               StrokeRateMean = mean(Strokerate),
                                                               Dist.to.shoreMax = max(Dist.to.shore),
                                                               Dist.to.shoreMean = mean(Dist.to.shore),
                                                               Dist.to.PaamiutMax = max(Dist.to.Paamiut),
                                                               Dist.to.PaamiutMean = mean(Dist.to.Paamiut)
)
                                                               
                                                               
# Save
narwhal <- narwhal %>% ungroup()

# Add the summarizing variable Sound
narwhal$Sound <- as.numeric(narwhal$Call) + as.numeric(narwhal$Buzz) + as.numeric(narwhal$Click) - 3
narwhal$Sound.bi <- ifelse(narwhal$Sound == 0, 0, 1) 
                      
saveRDS(narwhal, file = "outputs/narwhal_modified.RDS")
# Save reduced data set (selected such that both whales are present
saveRDS(narwhal[(369254-10000):(369254+10000),], file = "outputs/narwhal_modified_reduced.RDS")
