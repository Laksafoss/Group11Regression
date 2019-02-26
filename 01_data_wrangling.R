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

# Something looks odd with the number of observations from 7 dates
dttable <- table(narwhal$Datetime)
table(dttable)
datetimecounts <- narwhal %>% group_by(Datetime) %>% summarise(count = n())
dt <- datetimecounts %>% filter(count > 2)
three_obs <- narwhal %>% filter(Datetime %in% dt$Datetime) %>% arrange(Datetime)
duplicates <- duplicated(three_obs)
## Maybe there are more?
NROW(narwhal)
narwhal <- narwhal %>% group_by(Datetime) %>% distinct()
NROW(narwhal)
# Add binary varbiale indicating if both whales are present
narwhal <- narwhal %>% group_by(Datetime) %>% mutate(BothWhales = all(c("Helge","Thor") %in% Ind))
# Save
narwhal <- narwhal %>% ungroup()

# Add the summarizing variable Sound
narwhal$Sound <- as.numeric(narwhal$Call) + as.numeric(narwhal$Buzz) + as.numeric(narwhal$Click) - 3
narwhal$Sound.bi <- ifelse(narwhal$Sound == 0, 0, 1) 
                      
saveRDS(narwhal, file = "outputs/narwhal_modified.RDS")
# Save reduced data set (selected such that both whales are present
saveRDS(narwhal$Ind[(369254-10000):(369254+10000)], file = "outputs/narwhal_modified_reduced.RDS")

