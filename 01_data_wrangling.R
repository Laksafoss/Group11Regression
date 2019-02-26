library(lubridate) # for date and times
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

# Save
narwhal <- narwhal %>% ungroup()

# Add the summarizing variable Sound
narwhal$Sound <- as.numeric(narwhal$Call) + as.numeric(narwhal$Buzz) + as.numeric(narwhal$Click) - 3
narwhal$Sound.bi <- ifelse(narwhal$Sound == 0, 0, 1) 
                      
saveRDS(narwhal, file = "outputs/narwhal_modified.RDS")
# Save reduced data set (selected such that both whales are present
saveRDS(narwhal$Ind[(369254-10000):(369254+10000)], file = "outputs/narwhal_modified_reduced.RDS")
