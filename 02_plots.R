library(Hmisc)
library(corrplot)
library(gridExtra)
library(plyr)
library(dplyr)
library(ggplot2)

narwhal <- readRDS(file = "outputs/narwhal_Minut.RDS")
#narwhal <- readRDS(file = "outputs/narwhal_modified.RDS")

# histrogram/Density plots of explanatory numerical variables 
numnames <- c("Dist.to.shore", "ODBA")
numplotslist <- lapply(numnames,
                       function(name) ggplot(narwhal, aes_string(x = name, y="..density..")) +
                         geom_histogram(bins = 50) +
                         geom_density())
numplots <- marrangeGrob(numplotslist,  ncol = 2, nrow = 1, top = "Density plots of numerical variables")
ggsave(numplots, filename = "figs/numerical_densities_exp.png",device = "png", dpi = 300,
       width = 6, height = 4, units = "in")
summary(narwhal$Dist.to.shore)

# histogram/Density plots of Response variables
numnames <- c("Depth", "Strokerate", "ClickSum", "CallSum")
numplotslist <- lapply(numnames,
                   function(name) ggplot(narwhal, aes_string(x = name, y="..density..")) +
                     geom_histogram(bins = 50) +
                     geom_density())
numplots <- marrangeGrob(numplotslist,  ncol = 2, nrow = 2,top = "Density plots of numerical variables")
ggsave(numplots, filename = "figs/numerical_densities_resp.png", device = "png" ,dpi = 300,
       width = 6, height = 4, units = "in")

# histogram/Density plots of Click/Call max variables
narwhal$logDepth <- log(narwhal$Depth)
numnames <- c("logDepth" )
numplotslist <- lapply(numnames,
                       function(name) ggplot(narwhal, aes_string(x = name, y="..density..")) +
                         geom_histogram(bins = 50) +
                         geom_density())
numplots <- marrangeGrob(numplotslist,  ncol = 1, nrow = 1,top = "log of Depth")
ggsave(numplots, filename = "figs/logDepth.png", device = "png",dpi = 300,
       width = 6, height = 3, units = "in")


# Depth/Timeplot

depthtimeplot <- ggplot(narwhal, aes(x = Start)) +
  geom_line(aes(y = -Depth)) +
  facet_wrap(c("Ind", "Phase"), labeller = "label_both", scales = "free_x") +
  ggtitle("Depth over time by phase and Ind") + ylab("Depth")
ggsave(depthtimeplot, filename = "figs/depthtimeplot.png", device = "png",dpi = 300,
       width = 6, height = 4, units = "in")

times <- narwhal %>%
  group_by(Phase, Ind) %>%
  summarise(start = min(Start),
            end = max(Start))

depthtimeplotPhases <- ggplot(narwhal) +
  geom_rect(
    aes(x = NULL ,xmin = start, xmax = end, fill = Phase),
    ymin = -Inf, ymax = Inf, alpha = 0.8,
    data = times
  ) + 
  geom_line(aes(x = Start, y = -Depth), size = 0.3) +
  facet_wrap(c("Ind"), labeller = "label_both", nrow = 2) +
  ggtitle("Depth over time by Ind, shaded by Phase") + ylab("Depth")

ggsave(depthtimeplotPhases, filename = "figs/depthtimeplotPhases.png", device = "png", dpi = 300,
       width = 6, height = 4, units = "in")

depthtimeplotPhasesNoB <- ggplot(narwhal[narwhal$Phase != "B",]) +
  geom_rect(
    aes(x = NULL ,xmin = start, xmax = end, fill = Phase),
    ymin = -Inf, ymax = Inf, alpha = 0.8,
    data = times[times$Phase != "B",]
  ) + 
  geom_line(aes(x = Start, y = -Depth), size = 0.3) +
  facet_wrap(c("Ind"), labeller = "label_both", nrow = 2) +
  ggtitle("Depth over time by Ind, shaded by Phase without the B-Phase")+ ylab("Depth")
depthtimeplotPhasesNoB

ggsave(depthtimeplotPhasesNoB, filename = "figs/depthtimeplotPhasesNoB.png", device = "png", dpi = 300,
       width = 6, height = 2, units = "in")

depthtimeplotPhasesNoBHelge <- ggplot(narwhal[narwhal$Ind == "Helge" & narwhal$Phase != "B",]) +
  geom_rect(
    aes(x = NULL ,xmin = start, xmax = end, fill = Phase),
    ymin = -Inf, ymax = Inf, alpha = 0.8,
    data = times[times$Phase != "B",]
  ) + 
  geom_line(aes(x = Start, y = -Depth), size = 0.3) +
  ggtitle("Helges Depth over time after B-Phase, shaded by Phase") + ylab("Depth")

ggsave(depthtimeplotPhasesNoBHelge, filename = "figs/depthtimeplotPhasesNoB.png", device = "png", dpi = 300,
       width = 6, height = 3, units = "in")

# Phase and time plots

PhasePlot <- ggplot(times, aes(x = Phase, color = Phase)) +
  geom_linerange(aes(ymin = start, ymax = end), size = 10) +
  facet_wrap("Ind", nrow = 2) + coord_flip()
ggsave(PhasePlot, filename = "figs/PhasePlot.png", device = "png")



# Location plot
startstop <- narwhal %>%
  group_by(Ind) %>%
  filter(!is.na(Long),
         !is.na(Lat)) %>% 
  filter(
    Start %in% c(min(Start),max(Start))
  )
startstop$StartStop <- c("Start","Stop")  


locationplot <- ggplot(narwhal,
                       aes(x = Lat, y = Long, group = Ind)) +
  geom_path(aes(colour = Start))  +
  geom_point(data = startstop, aes(shape = StartStop), size = 2) +
  facet_wrap("Ind", labeller = "label_both")

locationplotPhase <- ggplot(narwhal,
                       aes(x = Lat, y = Long, group = Ind)) +
  geom_path(aes(colour = Phase))  +
  geom_point(data = startstop, aes(shape = StartStop), size = 2) +
  facet_wrap("Ind", labeller = "label_both")

locationsplots <- arrangeGrob(locationplot, locationplotPhase, nrow = 2, top = "Location plots by Time and by Phase")

ggsave(locationplot, filename = "figs/locationplot.png", device = "png",dpi = 300,
       width = 6, height = 4, units = "in")
ggsave(locationplotPhase, filename = "figs/locationplotPhase.png", device = "png" ,dpi = 300,
       width = 6, height = 2, units = "in")
ggsave(locationsplots, filename = "figs/locationsplots.png", device = "png" ,dpi = 300,
       width = 6, height = 2, units = "in")

# Location plot no b
startstopNoB <- narwhal %>% group_by(Ind) %>% 
  filter(!is.na(Long),
         !is.na(Lat),
         Phase != "B") %>% 
  filter(
    Start %in% c(min(Start),max(Start))
  )

startstopNoB$StartStop <- c("Start","Stop")  

locationplotNoB <- ggplot(narwhal[narwhal$Phase != "B",],
                       aes(x = Lat, y = Long, group = Ind)) +
  geom_path(aes(colour = Start))  +
  geom_point(data = startstopNoB, aes(shape = StartStop), size = 2) +
  facet_wrap("Ind", labeller = "label_both", scales = "free") + 
  ggtitle("Excluding Phase B")

locationplotNoB

locationsplots <- arrangeGrob(locationplot, locationplotNoB, nrow = 2, top = "Location plots by Start with and without Phase B")
ggsave(locationsplots, filename = "figs/locationsplotswNoB.png", device = "png",  dpi = 300,
       width = 6, height = 2, units = "in")


# Location plot no b
startstopNoB <- narwhal %>% 
  filter(Ind == "Helge",
         !is.na(Long),
         !is.na(Lat),
         Phase != "B") %>% 
  filter(
    Start %in% c(min(Start),max(Start))
  )
startstopNoB$StartStop <- c("Start","Stop")  

locationplotNoBNoHelge <- ggplot(narwhal[narwhal$Phase != "B" & narwhal$Ind == "Helge" ,],
                       aes(x = Lat, y = Long, group = Ind)) +
  geom_path(aes(colour = Start))  +
  geom_point(data = startstopNoB, aes(shape = StartStop), size = 2) +
  ggtitle("Location Plot")

locationplotPhaseNoBNoHelge <- ggplot(narwhal[narwhal$Phase != "B" & narwhal$Ind == "Helge" ,],
                            aes(x = Lat, y = Long, group = Ind)) +
  geom_path(aes(colour = Phase), size = 1)  +
  geom_point(data = startstopNoB, aes(shape = StartStop), size = 2) +
  ggtitle("Location Plot")
locationsplotsNoBoHelge <- arrangeGrob(locationplotNoBNoHelge, locationplotPhaseNoBNoHelge, ncol = 2, top = "Location plots by Time and by Phase")
ggsave(locationplotNoB, filename = "figs/locationplotnoB.png", device = "png" ,dpi = 300,
       width = 6, height = 2, units = "in")
ggsave(locationplotNoBNoHelge, filename = "figs/locationplotnoBNH.png", device = "png" ,dpi = 300,
       width = 6, height = 2, units = "in")
ggsave(locationplotPhaseNoBNoHelge, filename = "figs/locationplotPhasenoBNH.png", device = "png" ,dpi = 300,
       width = 6, height = 2, units = "in")
ggsave(locationsplotsNoBoHelge, filename = "figs/locationsplotsNoBNH.png", device = "png" ,dpi = 300,
       width = 6, height = 2, units = "in")


#### Correlation matrices for numerical and categorical variables
#Setup numerical
narwhalnum <- narwhal[, c("ODBA", "VeDBA", "Strokerate", "Dist.to.shore", "Depth")]
cormat1 <- cor(data.matrix(na.omit(narwhalnum)), method = "pearson")

#Setup categorical
narwhalcat <- narwhal[, c("Ind", "Area", "ClickSum", "CallSum","BuzzSum", "Acou.qua", "Seismik", "Phase", "Los")]
cormat2 <- cor(data.matrix(na.omit(narwhalcat)), method = "spearman")

#Plot matrices
png("figs/correlationmatrix.png", width = 1480, height = 1080, units = "px", pointsize = 20)
par(mfrow=c(1,2))
catcorrmatplot1 <- corrplot(cormat1, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45, number.digits = 3, addCoef.col = "white")
catcorrmatplot2 <- corrplot(cormat2, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45, number.digits = 3, addCoef.col = "white")
par(mfrow=c(1,1))
dev.off()

# Peridiocty plot
SECS_IN_DAY <- 60*60*24
scaler <- function(x) x/SECS_IN_DAY * pi * 2
toPeriodic <- function(x) cos(scaler(x)) + sin(scaler(x))
secs <- seq(0,SECS_IN_DAY,by = 60)
tmpDF <- data.frame(periodic_value = toPeriodic(secs),
                    Time = secs)
periodicplot <- ggplot(tmpDF, aes(y = periodic_value, Time)) +
  geom_line() +
  scale_x_time(breaks = seq(0,24, by = 3) * 60 * 60) +
  labs(x = "Time of Day",
       y = "Periodic value") +
  ggtitle("Periodic daytime value for a single day") +
  theme(axis.text=element_text(size=5),axis.title=element_text(size=8))

ggsave(periodicplot, filename = "figs/periodicplot.png", device = "png", dpi = 300,
       width = 6, height = 2, units = "in")
