library(ggplot2)
library(gridExtra)
library(dplyr)
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

# Timeplot
depthtimeplot <- ggplot(narwhal, aes(x = Datetime)) +
  geom_line(aes(y = Depth)) +
  facet_wrap(c("Ind", "Phase"), labeller = "label_both", scales = "free_x") +
  ggtitle("Depth over time by phase and Ind")
ggsave(depthtimeplot, filename = "figs/depthtimeplot.png", device = "png")
# Location plot
locationplot <- ggplot(narwhal,
                       aes(x = Lat, y = Long, group = Ind)) +
  geom_path()  +
  facet_wrap("Ind", labeller = "label_both") +
  ggtitle("Location Plot")
ggsave(locationplot, filename = "figs/locationplot.png", device = "png")


