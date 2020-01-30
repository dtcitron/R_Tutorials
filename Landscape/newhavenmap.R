# From Chapter 3 of R for Spatial Analysis and Mapping

# Load package and data
library(GISTools)
data(newhaven)

# plot blocks and roads spatial data
plot(blocks)
plot(roads, add = TRUE, col = 'red')
# add map scale
map.scale(534750, 152000, miles2ft(2), "Miles", 4, 0.5)
# add arrow for orientation!
north.arrow(534750, 154000, miles2ft(0.25), col = "lightblue")
# Add a title
title('New Haven, CT')

# Once this script is saved, we can run it from the command line using source()
#source("newhavenmap.R")