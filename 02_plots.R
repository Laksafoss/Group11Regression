library(Hmisc)
library(corrplot)
library(gridExtra)
library(plyr)
library(dplyr)
library(ggplot2)

narwhal <- readRDS(file = "outputs/narwhal_Minut.RDS")

# histrogram/Density plots of exp numerical variables 
numnames <- c("Dist.to.Paamiut","Dist.to.shore", "Lat", "Long")
numplotslist <- lapply(numnames,
                       function(name) ggplot(narwhal, aes_string(x = name, y="..density..")) +
                         geom_histogram(bins = 50) +
                         geom_density())
numplots <- marrangeGrob(numplotslist,  ncol = 2, nrow = 2, top = "Density plots of numerical variables")
ggsave(numplots, filename = "figs/numerical_densities_exp.png",device = "png")


# histrogram/Density plots of Resp numerical variables
numnames <- c("Depth","ODBA", "VeDBA","Strokerate")
numplotslist <- lapply(numnames,
                   function(name) ggplot(narwhal, aes_string(x = name, y="..density..")) +
                     geom_histogram(bins = 50) +
                     geom_density())
numplots <- marrangeGrob(numplotslist,  ncol = 2, nrow = 2,top = "Density plots of numerical variables")
ggsave(numplots, filename = "figs/numerical_densities_resp.png",device = "png")

# Depth/Timeplot

depthtimeplot <- ggplot(narwhal, aes(x = Start)) +
  geom_line(aes(y = -Depth)) +
  facet_wrap(c("Ind", "Phase"), labeller = "label_both", scales = "free_x") +
  ggtitle("Depth over time by phase and Ind") + ylab("Depth")
ggsave(depthtimeplot, filename = "figs/depthtimeplot.png", device = "png")

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

ggsave(depthtimeplotPhases, filename = "figs/depthtimeplotPhases.png", device = "png")

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

ggsave(depthtimeplotPhasesNoB, filename = "figs/depthtimeplotPhasesNoB.png", device = "png")

depthtimeplotPhasesNoBHelge <- ggplot(narwhal[narwhal$Ind == "Helge" & narwhal$Phase != "B",]) +
  geom_rect(
    aes(x = NULL ,xmin = start, xmax = end, fill = Phase),
    ymin = -Inf, ymax = Inf, alpha = 0.8,
    data = times[times$Phase != "B",]
  ) + 
  geom_line(aes(x = Start, y = -Depth), size = 0.3) +
  ggtitle("Helges Depth over time after B-Phase, shaded by Phase") + ylab("Depth")

ggsave(depthtimeplotPhasesNoBHelge, filename = "figs/depthtimeplotPhasesNoB.png", device = "png")

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

ggsave(locationplot, filename = "figs/locationplot.png", device = "png")
ggsave(locationplotPhase, filename = "figs/locationplotPhase.png", device = "png")
ggsave(locationsplots, filename = "figs/locationsplots.png", device = "png")

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
ggsave(locationsplots, filename = "figs/locationsplotswNoB.png", device = "png")


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
ggsave(locationplotNoB, filename = "figs/locationplotnoB.png", device = "png")
ggsave(locationplotNoBNoHelge, filename = "figs/locationplotnoBNH.png", device = "png")
ggsave(locationplotPhaseNoBNoHelge, filename = "figs/locationplotPhasenoBNH.png", device = "png")
ggsave(locationsplotsNoBoHelge, filename = "figs/locationsplotsNoBNH.png", device = "png")


# Correlation matrix for numerical variables
narwhalnum <- narwhal[, c("ODBA", "VeDBA", "Strokerate")]

cormat1 <- cor(data.matrix(na.omit(narwhalnum)), method = "pearson")

png("figs/correlationmatrixnum.png")
catcorrmatplot1 <- corrplot(cormat1, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45, number.digits = 3, addCoef.col = "white")
dev.off()
catcorrmatplot1
?cor
?corrplot.mixed
# Correlation matrix for categorical variables
narwhalcat <- narwhal[, c("Ind", "Area", "Click", "Buzz", "Acou.qua", "Seismik", "Phase", "Los")]
cormat1 <- cor(data.matrix(na.omit(narwhalcat)), method = "spearman")

png("figs/correlationmatrix.png")
catcorrmatplot1 <- corrplot(cormat1, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
dev.off()
?corrplot
