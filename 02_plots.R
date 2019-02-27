library(Hmisc)
library(corrplot)
library(gridExtra)
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

timesNew <- narwhal %>% group_by(Ind, NewPhase) %>%
  summarise(start = min(Datetime),
            end = max(Datetime)) %>% 
  arrange(Ind, start)


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
    data = times[-1,]
  ) + 
  geom_line(aes(x = Datetime, y = Depth), size = 0.3) +
  facet_wrap(c("Ind"), labeller = "label_both", nrow = 2) +
  ggtitle("Depth over time by Ind, shaded by Phase")
depthtimeplotPhasesNoB
ggsave(depthtimeplotPhasesNoB, filename = "figs/depthtimeplotPhasesNoB.png", device = "png")


# Phase and time plots

oldPhasePlot <- ggplot(times, aes(x = Phase, color = Phase)) +
  geom_linerange(aes(ymin = start, ymax = end), size = 10) +
  facet_wrap("Ind", nrow = 2) + coord_flip()
ggsave(oldPhasePlot, filename = "figs/OldPhasePlot.png", device = "png")

newPhasePlot <- ggplot(timesNew, aes(x = NewPhase, color = NewPhase)) +
  geom_linerange(aes(ymin = start, ymax = end), size = 6) +
  facet_wrap("Ind", nrow = 2) + coord_flip()
ggsave(newPhasePlot, filename = "figs/NewPhaseplot.png", device = "png")

# Location plot
locationplot <- ggplot(narwhal,
                       aes(x = Lat, y = Long, group = Ind)) +
  geom_path(aes(colour = Datetime))  +
  facet_wrap("Ind", labeller = "label_both") +
  ggtitle("Location Plot")
ggsave(locationplot, filename = "figs/locationplot.png", device = "png")

# Correlation matrix for categorical variables
narwhalcat <- narwhal[, c(4,5,6,7,8,10,12,13,19)]

cormat1 <- cor(data.matrix(na.omit(narwhalcat)), method = "spearman")

png("figs/correlationmatrix.png")
catcorrmatplot1 <- corrplot(cormat1, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
dev.off()
