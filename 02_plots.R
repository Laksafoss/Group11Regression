library(ggplot2)
library(gridExtra)
library(dplyr)
library(Hmisc)
library(corrplot)
narwhal <- readRDS(file = "outputs/narwhal_modified.RDS")

# histrogram/Density plots of numerical variables
numnames <- c("Depth","Dist.to.Paamiut","Dist.to.shore","ODBA", "VeDBA", "Lat", "Long", "Strokerate")
numplotslist <- lapply(numnames,
                   function(name) ggplot(narwhal, aes_string(x = name, y="..density..")) +
                     geom_histogram(bins = 50) +
                     geom_density())
numplots <- marrangeGrob(numplotslist,  ncol = 2, nrow = 4, top = "Density plots of numerical variables")
ggsave(numplots, filename = "figs/numerical_densities.png",device = "png")

# boxplots plots of numerical variables
numnames <- c("Depth","Dist.to.Paamiut")
numplotslist <- lapply(numnames,
                       function(name) ggplot(narwhal, aes_string(x = name, y="..density..")) +
                         geom_boxplot())
numplots <- marrangeGrob(numplotslist,  ncol = 2, nrow = 4, top = "Density plots of numerical variables")
numplots
ggsave(numplots, filename = "figs/numerical_densities.png",device = "png")

# Depth/Timeplot

depthtimeplot <- ggplot(narwhal, aes(x = Datetime)) +
  geom_line(aes(y = Depth)) +
  facet_wrap(c("Ind", "Phase"), labeller = "label_both", scales = "free_x") +
  ggtitle("Depth over time by phase and Ind")
ggsave(depthtimeplot, filename = "figs/depthtimeplot.png", device = "png")

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
