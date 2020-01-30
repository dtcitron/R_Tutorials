# This is for splitting up Haiti according to the available admin data
# Generate some plots showing the population distribution and show them to Dave

library(sp) # necessary for rgdal
library(rgdal)
library(raster)
library(rgeos)
library(maptools)

worldpop <- raster("/Volumes/snfs/WORK/11_geospatial/01_covariates/00_MBG_STANDARD/worldpop/total/5y/worldpop_total_5y_2015_00_00.tif")

admin2 <- readShapePoly("/Volumes/snfs/WORK/11_geospatial/06_original shapefiles/GAUL_admin/admin2/g2015_2014_2/g2015_2014_2.shp")

HTI <- admin2[admin2$ADM0_NAME=="Haiti",]
# There are 42 A2 divisions of haiti

plot(HTI)

# Output this, for a demo
writeOGR(obj=HTI, dsn='Documents/Landscape/Haiti_Demo/Haiti_admin2_shapefiles', layer='HTI', driver='ESRI Shapefile')

# Plot the population on the island:
HTI.pop <- crop(worldpop, HTI)

HTI.pop2 <- mask(HTI.pop, HTI)

plot(HTI.pop, frame = FALSE)
plot(HTI, add = TRUE)

plot(HTI.pop2, legend = FALSE, axes = FALSE)
plot(HTI, add = TRUE)


# This is the 5k data, what about the 1k population data?

# And what about the histogram of the populations?
pop <- as.vector(HTI.pop2)
pop <- pop[!is.na(pop)]

# Plot logarithm of population, to get a better sense of where the high density areas are
# (not just dominated by highest density area ie Port-au-Prince)
HTI.pop2.log <- HTI.pop2
values(HTI.pop2.log) <- log10(values(HTI.pop2.log))
plot(HTI.pop2.log, legend = TRUE, axes = FALSE)
plot(HTI, add = TRUE)

# What is the population in each of the A2 regions?
COR <- HTI[HTI$ADM2_NAME == "Corail",]
plot(COR)
COR.pop <- mask(HTI.pop, COR)
plot(COR)
plot(COR.pop, axes = FALSE, add = TRUE)
plot(COR, add = TRUE)

COR.sum <- extract(HTI.pop, COR, fun = sum, na.rm = TRUE)

h <- values(COR.pop)
length(h[!is.na(h)])

# Create a list that links the populations of each A2 region to the name of that region
a2.names <- HTI$ADM2_NAME
reg.pop.list <- function(x){
  reg <- HTI[HTI$ADM2_NAME == x,]
  reg.sum <- extract(HTI.pop, reg, fun = sum, na.rm = TRUE)
  return(reg.sum)
}
reg.pops <- lapply(a2.names, reg.pop.list)
names(reg.pops) <- a2.names
reg.pops[["Corail"]]

# What are the names of these regions?
# This is a function from sp that extracts a labeling location, which appears to be close to the centroid
# getSpPPolygonsLabptSlots
# But there is zero documentation and it throws cryptic errors, so this might not be correct?
reg.loc.label <- function(x) {
  reg <- HTI[HTI$ADM2_NAME==x,]
  loc <- getSpPPolygonsLabptSlots(reg)
  return(loc)
}
# obtain centroids for each of the regions:
ll <- sapply(a2.names, reg.loc.label)
lon <- ll[1,]
lat <- ll[2,]
plot(HTI)
pl <- pointLabel(lon, lat, a2.names, offset = 0, cex = .75)

# Or we can plot the total population at each location
# 
library(plyr)
reg.loc.pop <- function(x) {
  return(round_any(as.integer(reg.pops[[x]]), 1000)/1000)
}
reg.pop <- sapply(as.character(a2.names), reg.loc.pop)
plot(HTI.pop2, legend = FALSE, axes = FALSE)
plot(HTI, add = TRUE)
pl <- pointLabel(lon, lat, as.character(reg.pop), offset = 0, cex = .75)

# plot A1 regions, with borders bolded (a little slow...)
a1.names <- unique(data.frame(HTI)$ADM1_NAME)
plot(HTI.pop2, legend = FALSE, axes = FALSE)
plot(HTI, add = TRUE)
for (x in a1.names){
  h1 <- gUnaryUnion(HTI[HTI$ADM1_NAME == x,])
  plot(h1, lwd = 3, add=TRUE)
}

# Next: read up on the access data set, how it was calculated and such
# Next: create "topological" map of access data set, juxtaposed with the population, in 1k pieces
# also need to consider: do I need GRUMP or some other data set?  Worldpop is doing artificial things to the map



