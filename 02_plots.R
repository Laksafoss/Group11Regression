library(Hmisc)
library(corrplot)
library(gridExtra)
library(plyr)
library(dplyr)
library(ggplot2)

narwhal <- readRDS(file = "outputs/narwhal_modified.RDS")

# histrogram/Density plots of numerical variables
numnames <- c("Depth","Dist.to.Paamiut","Dist.to.shore","ODBA", "VeDBA", "Lat", "Long", "Strokerate")
numplotslist <- lapply(numnames,
                   function(name) ggplot(narwhal, aes_string(x = name, y="..density..")) +
                     geom_histogram(bins = 50) +
                     geom_density())
numplots <- marrangeGrob(numplotslist,  ncol = 2, nrow = 4, top = "Density plots of numerical variables")
ggsave(numplots, filename = "figs/numerical_densities.png",device = "png")

# Depth/Timeplot

depthtimeplot <- ggplot(narwhal, aes(x = Datetime)) +
  geom_line(aes(y = Depth)) +
  facet_wrap(c("Ind", "Phase"), labeller = "label_both", scales = "free_x") +
  ggtitle("Depth over time by phase and Ind")
ggsave(depthtimeplot, filename = "figs/depthtimeplot.png", device = "png")

times <- narwhal %>%
  group_by(Phase, Ind) %>%
  summarise(start = min(Datetime),
            end = max(Datetime))

depthtimeplotPhases <- ggplot(narwhal) +
  geom_rect(
    aes(x = NULL ,xmin = start, xmax = end, fill = Phase),
    ymin = -Inf, ymax = Inf, alpha = 0.5,
    data = times
  ) + 
  geom_line(aes(x = Datetime, y = Depth), size = 0.3) +
  facet_wrap(c("Ind"), labeller = "label_both", nrow = 2) +
  ggtitle("Depth over time by Ind, shaded by Phase")

ggsave(depthtimeplotPhases, filename = "figs/depthtimeplotPhases.png", device = "png")

depthtimeplotPhasesNoB <- ggplot(narwhal[narwhal$Phase != "B",]) +
  geom_rect(
    aes(x = NULL ,xmin = start, xmax = end, fill = Phase),
    ymin = -Inf, ymax = Inf, alpha = 0.5,
    data = times[times$Phase != "B",]
  ) + 
  geom_line(aes(x = Datetime, y = Depth), size = 0.3) +
  facet_wrap(c("Ind"), labeller = "label_both", nrow = 2) +
  ggtitle("Depth over time by Ind, shaded by Phase without the B-Phase")
depthtimeplotPhasesNoB

ggsave(depthtimeplotPhasesNoB, filename = "figs/depthtimeplotPhasesNoB.png", device = "png")

depthtimeplotPhasesNoBHelge <- ggplot(narwhal[narwhal$Ind == "Helge" & narwhal$Phase != "B",]) +
  geom_rect(
    aes(x = NULL ,xmin = start, xmax = end, fill = Phase),
    ymin = -Inf, ymax = Inf, alpha = 0.5,
    data = times[times$Phase != "B",]
  ) + 
  geom_line(aes(x = Datetime, y = Depth), size = 0.3) +
  ggtitle("Helges Depth over time after B-Phase, shaded by Phase")

ggsave(depthtimeplotPhasesNoBHelge, filename = "figs/depthtimeplotPhasesNoB.png", device = "png")

# Phase and time plots

PhasePlot <- ggplot(times, aes(x = Phase, color = Phase)) +
  geom_linerange(aes(ymin = start, ymax = end), size = 10) +
  facet_wrap("Ind", nrow = 2) + coord_flip()
ggsave(PhasePlot, filename = "figs/OldPhasePlot.png", device = "png")


#NewPhase data + plot
narwhal$NewPhase <- narwhal$Phase
narwhal$NewPhase <- mapvalues(narwhal$NewPhase, c("B", "I0", "I3", "T0", "T3", "I1","I4","T1","T4","I2","I5","T2","T5"), 
          c("B","I0","T0","I1","T1","I2","T2","I3","T3","I4","T4","I5","T5"))
narwhal$NewPhase <- ordered(narwhal$NewPhase, levels = c("B","I0","T0","I1","T1","I2","T2","I3","T3","I4","T4","I5","T5"))
NewPhasePlot <- ggplot(narwhal, aes(Datetime, NewPhase)) +   
  geom_line() + facet_wrap("Ind", nrow = 2) + coord_flip()
ggsave(NewPhasePlot, filename = "figs/NewPhasePlot.png", device = "png")


# Location plot
startstop <- narwhal %>%
  group_by(Ind) %>%
  filter(!is.na(Long),
         !is.na(Lat)) %>% 
  filter(
    Datetime %in% c(min(Datetime),max(Datetime))
  )
startstop$StartStop <- c("Start","Stop")  


locationplot <- ggplot(narwhal,
                       aes(x = Lat, y = Long, group = Ind)) +
  geom_path(aes(colour = Datetime))  +
  geom_point(data = startstop, aes(shape = StartStop), size = 2) +
  facet_wrap("Ind", labeller = "label_both")
locationplotPhase <- ggplot(narwhal,
                       aes(x = Lat, y = Long, group = Ind)) +
  geom_path(aes(colour = Phase))  +
  geom_point(data = startstop, aes(shape = StartStop), size = 2) +
  facet_wrap("Ind", labeller = "label_both")

locationsplots <- arrangeGrob(locationplot, locationplotPhase, nrow = 2, top = "Location plots by Time and by Phase")

ggsave(locationplot, filename = "figs/locationplot.png", device = "png")
ggsave(locationplotPhase, filename = "figs/locationplotPhase.png", device = "png")
ggsave(locationsplots, filename = "figs/locationsplots.png", device = "png")

# Location plot no b only Helge
startstopNoB <- narwhal %>% 
  filter(Ind == "Helge",
         !is.na(Long),
         !is.na(Lat),
         Phase != "B") %>% 
  filter(
    Datetime %in% c(min(Datetime),max(Datetime))
  )
startstopNoB$StartStop <- c("Start","Stop")  

locationplotNoB <- ggplot(narwhal[narwhal$Phase != "B" & narwhal$Ind == "Helge" ,],
                       aes(x = Lat, y = Long, group = Ind)) +
  geom_path(aes(colour = Datetime))  +
  geom_point(data = startstopNoB, aes(shape = StartStop), size = 2) +
  ggtitle("Location Plot")

locationplotPhaseNoB <- ggplot(narwhal[narwhal$Phase != "B" & narwhal$Ind == "Helge" ,],
                            aes(x = Lat, y = Long, group = Ind)) +
  geom_path(aes(colour = Phase), size = 1)  +
  geom_point(data = startstopNoB, aes(shape = StartStop), size = 2) +
  ggtitle("Location Plot")
locationsplotsNoB <- arrangeGrob(locationplotNoB, locationplotPhaseNoB, ncol = 2, top = "Location plots by Time and by Phase")

ggsave(locationplotNoB, filename = "figs/locationplotnoB.png", device = "png")
ggsave(locationplotPhaseNoB, filename = "figs/locationplotPhasenoB.png", device = "png")
ggsave(locationsplotsNoB, filename = "figs/locationsplotsNoB.png", device = "png")


# Correlation matrix for categorical variables
narwhalcat <- narwhal[, c(4,5,6,7,8,10,12,13,19)]

cormat1 <- cor(data.matrix(na.omit(narwhalcat)), method = "spearman")

png("figs/correlationmatrix.png")
catcorrmatplot1 <- corrplot(cormat1, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
dev.off()

