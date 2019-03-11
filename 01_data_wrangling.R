library(lubridate) # for date and times
library(dplyr) # general purpose data manipulation



# ==  STEP 1  :  CLEAN UP RAW DATA A BIT  ======================================
narwhal <- readRDS(file = "data/narwhal.RDS")
## Data wrangling
# Change all variables to have capitalized start letter
firstletterupper <- sapply(names(narwhal), function(x) toupper(substring(x,1,1)))
lastletters <- sapply(names(narwhal), function(x) substring(x,2,nchar(x)))
names(narwhal) <- paste0(firstletterupper, lastletters)

# Add a single datetime object
datetimes <- paste(narwhal[,1], narwhal[,2], sep = " ")
narwhal$Datetime <- parse_date_time(datetimes,"Ymd HMS")

# Jeg slog soltimerne for Lat 70, Long -26 den 16 august 2017 op
sundown <- c("21","22","23","00","01","02")
sunup1 <- c("03","04","05","06","07","08","09","10","11") # mid day kl 11:48
sunup2 <- c("12","13","14","15","16","17","18","19","20")
narwhal$Sun <- as.factor(ifelse(substring(narwhal$Hour, 1,2) %in% sundown, "down", "up"))


# Correct for positve depths (by substracting maximum value) - make positive
narwhal$Depth <- - (narwhal$Depth - max(narwhal$Depth))

# Order Phase
Phases <- c("B", "I0", "T0", "I1","T1", "I2", "T2", "I3", "T3", "I4", "T4", "I5","T5")
narwhal$Phase <- factor(narwhal$Phase, levels = Phases, ordered = TRUE)
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
saveRDS(dups, file = "outputs/AllDuplicates.RDS")
newrows <- dups[9:12,]
newrows[1,"Dist.to.Paamiut"] <- mean(newrows[1:2,"Dist.to.Paamiut"])  
newrows[3,"Dist.to.Paamiut"] <- mean(newrows[3:4,"Dist.to.Paamiut"])  
# Remove dupls (use mean when different Dists)
narwhal <- narwhal[-dupsId,]
narwhal <- rbind(newrows[c(1,3),],narwhal)
saveRDS(newrows[c(1,3),], file = "outputs/TwoDuplicates.RDS")


## FIX NAs in ODBS Snd VeDBA by Removing them
narwhal <- narwhal %>% filter(!is.na(ODBA))

## FIX NAs in Strokerate
### First we see that it's the first and last rows. Then we remove them.
NaStroke <- narwhal %>% mutate(index = 1:NROW(narwhal)) %>%
  filter(is.na(Strokerate)) %>%
  group_by(Ind) %>%
  mutate(NAgroup = 
           cut(index, c(-Inf, index[which(diff(index) > 1 )] + 0.5, Inf)
               , include.lowest = TRUE)) %>% 
  group_by(NAgroup) %>% 
  summarise(Ind = factor(Ind[1]),
            start = min(Datetime),
            end = max(Datetime),
            duration = n())
NAStrokeDF <- data.frame(NaStroke)
fullDF <- data.frame(Ind = c("Thor","Helge"),
                     start = c(min(narwhal[narwhal$Ind == "Thor","Datetime"]),
                               min(narwhal[narwhal$Ind == "Helge","Datetime"])),
                     end = c(max(narwhal[narwhal$Ind == "Thor","Datetime"]),
                             max(narwhal[narwhal$Ind == "Helge","Datetime"])),
                     duration = c(NA,NA))
NaStrokeDF <- rbind(NAStrokeDF[,2:5], fullDF)
saveRDS(NaStrokeDF, file = "outputs/NAStrokeDf.RDS")

narwhal <- narwhal %>% filter(!is.na(Strokerate))
# Save
narwhal <- narwhal %>% ungroup()

## ==  STEP 2 : CREATE SUMMARIZED DATASETS  ====================================

# I have made a funciton that summarizes the data as we want it
find_sub_data <- function(x) {
  X <- narwhal_summarised <- narwhal %>%
    group_by((!!as.name(x)), Ind) %>%
    summarise(Start = min(Datetime, na.rm = T),
              Depth = mean(Depth, na.rm = T),
              Seismik = mean(as.numeric(Seismik) - 1, na.rm = T),
              Phase = first(Phase),
              Area = first(Area),
              Acou.qua = mean(as.numeric(Acou.qua) - 1, na.rm = T),
              Dist.to.Paamiut = mean(Dist.to.Paamiut, na.rm = T),
              Dist.to.shore = mean(Dist.to.shore),
              ODBA = mean(ODBA, na.rm = T),
              VeDBA = mean(VeDBA, na.rm = T),
              Strokerate = mean(Strokerate, na.rm = T),
              Los = mean(as.numeric(Los)-1, na.rm = T),
              Lat = mean(Lat, na.rm = T),
              Long = mean(Long, na.rm = T),
              Sun = mean(as.numeric(Sun)-1, na.rm = T),
              CallSum = sum(as.numeric(Call) - 1, na.rm = T),
              ClickSum = sum(as.numeric(Click) - 1, na.rm = T),
              BuzzSum = sum(as.numeric(Buzz) - 1, na.rm = T),
              CallMean = mean(as.numeric(Call) - 1, na.rm = T),
              ClickMean = mean(as.numeric(Click) - 1, na.rm = T),
              BuzzMean = mean(as.numeric(Buzz) - 1, na.rm = T),
              CallMax = max(as.numeric(Call) - 1, na.rm = T),
              ClickMax = max(as.numeric(Click) - 1, na.rm = T),
              BuzzMax = max(as.numeric(Buzz) - 1, na.rm = T)
              
    )
  X <- X[ ,-1]
  X$Dive <- X$Depth > 10
  X$ClickBi <- 0
  X$ClickBi[X$ClickSum>=20] <- 1
  return(X)
} 


## TIME OF DAY ANALYSIS ========================================================
narwhal$Minut <- substring(narwhal$Datetime, 1,15) # every 10 minuts
TenMinData <- find_sub_data("Minut")

narwhal$Minut <- substring(narwhal$Datetime, 1,16) # every minut
MinData <- find_sub_data("Minut")


## DIVING ANALYSIS  ============================================================
create_dive_cut <- function(x) {
  N <- nrow(narwhal)
  X <- numeric(N)
  Diving <- ifelse(narwhal$Depth > x, 1, -1)
  jumps <- c(0, which(diff(Diving) != 0), N)
  rep.int(1:(length(jumps)-1), times = diff(jumps))
}

narwhal$Diving <- create_dive_cut(10)
Diving10Data <- find_sub_data("Diving")



## ==  STEP 3 : EXPORT EVERYTHING  =============================================
saveRDS(narwhal, file = "outputs/narwhal_modified.RDS")
saveRDS(Diving10Data, file = "outputs/narwhal_Diving.RDS")
saveRDS(TenMinData, file = "outputs/narwhal_TenMinut.RDS")
saveRDS(MinData, file = "outputs/narwhal_Minut.RDS")
# Save reduced data set (selected such that both whales are present
#saveRDS(narwhal[(369254-10000):(369254+10000),], file = "outputs/narwhal_modified_reduced.RDS")
