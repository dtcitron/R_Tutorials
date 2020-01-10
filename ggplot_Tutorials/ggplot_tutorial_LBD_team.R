# LBD team's ggplot orientation

library(ggplot2)

mydat <- travel.model.data

# elements of a ggplot
# 1. initialize
gg1 <- ggplot() + 
# 2. geometries/layers
  geom_point(data = mydat,
             # has corresponding aesthetics
             aes(x = pop, y = prall_map))
gg1

# manipulate additional aesthetics
gg1 <- ggplot() + 
  # 2. geometries/layers
  geom_point(data = mydat,
             # has corresponding aesthetics
             aes(x = pop, y = prall_map, 
                 color = ad2, alpha = 1-areaId/3000), color = "black")
gg1

# beware setting variables inside and outside of 
# note that setting the variable outside aes() overrides what I wrote inside aes()
# 
gg1 <- ggplot() + 
  # 2. geometries/layers
  geom_point(data = mydat,
             # has corresponding aesthetics
             aes(x = pop, y = prall_map, 
                 color = ad2, alpha = 1-areaId/3000), color = "black")
gg1
# note also that setting color inside the aes() function makes it look for a particular
# mapping - it's looking for a variable name, not a specific value
gg1 <- ggplot() + 
  # 2. geometries/layers
  geom_point(data = mydat,
             # has corresponding aesthetics
             aes(x = pop, y = prall_map, 
                 color = "blue"))
gg1


# Theme
# Theme function reference: https://ggplot2.tidyverse.org/reference/element.html

gg1 <- ggplot() + 
  # geometries/layers
  geom_point(data = mydat,
             # has corresponding aesthetics
             aes(x = pop, y = prall_map, 
                 color = ad2, alpha = 1-areaId/3000)) + 
  # set themes
  theme(legend.position = "none") + 
  ggtitle("This is my Title") +
  xlab("independent") + ylab("dependent")
gg1


# Color is very contentious - use viridis:
library(viridis)
gg1 <- ggplot() + 
  geom_point(data = mydat, 
             aes(x = pop, y = prall_map, color = ad2)) + 
  scale_colour_viridis_d()
gg1

# The repel package: 
# Annotate points or other elements of the graph, without those annotationns overlapping one another
library(ggrepel)
gg1 <- ggplot() + 
  geom_point(data = mydat,
             aes(x = pop, y = prall_map)) + 
  geom_text_repel(data = mydat[seq(1,194,10)], aes(x = pop, y = prall_map, label = ad2)) + 
  theme(legend.position = "none") 
gg1


# How to present multiple plots in the same place
library(ggpubr)

gg2 <- ggplot() + 
  geom_point(data = mydat,
             aes(x = pop, y = prall_map)) + 
  geom_text_repel(data = mydat[seq(5,194,10)], aes(x = pop, y = prall_map, label = ad2)) + 
  theme(legend.position = "none") 
gg2

ggarrange(gg1, gg2)

# We can also arrange this using facet_grid or facet_wrap
gg3 <- ggplot() + 
  geom_point(data = mydat,
             aes(x = pop, y = prall_map, color = ad2)) +
  scale_color_viridis_d() + 
  facet_wrap(~ad2) + 
  theme(legend.position = "none") 
gg3


map <- p2 + geom_polygon(data = plot.data, aes(x = long, y = lat, group = group, fill = R0.tar), color = NA, size = 0.25) +
  scale_fill_gradient(name="R0, TaR", low="#D1EFFF", high="#0000D6", limits=c(0,2)) +
  geom_polygon(data = bioko, aes(x = long, y = lat, group = group), color = "black", fill=NA, size = 0.25) +
  geom_text_repel(data = plot.data, aes(x = long, y = lat, label = id)) + 
  theme(axis.line=element_blank(),axis.text.x=element_blank(), axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),axis.title.y=element_blank(), panel.background=element_blank(), 
        legend.position=c(0.2, 0.75), legend.key.size = unit(12, "mm"), 
        legend.title = element_text(size = 20),
        legend.text=element_text(size=18))
map
