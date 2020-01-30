# https://geoscripting-wur.github.io/IntroToRaster/

# Here's what I did first:
# This is the R package version of the Geospatial Data Abstraction Library - http://www.gdal.org
# a translator library for raster/vector geospatial data formats
# install.packages("sp")
# install.packages("rgdal")
# This is the R package for raster processing.  It allows me to read and write raster data of most commonly used formats.
# The raster package extensively uses GDAL through rgdal
# install.packages("raster")
# For spatial dependences, such as adjacency
# install.packages("spdep")

# install.packages("maptools")
# install.packages("rgeos")
library(sp) # necessary for rgdal
library(rgdal)
library(raster)
library(rgeos)
library(maptools)

# After signing in to the J drive, load in a raster file:
worldpop <- raster("/Volumes/snfs/WORK/11_geospatial/01_covariates/00_MBG_STANDARD/worldpop/total/5y/worldpop_total_5y_2015_00_00.tif")
# This describes, at a 5kmX5km scale (or close to thereabouts, using latitude and longitude),
# the population in every "pixel"
# worldpop is a RasterLayer object, which is a single-layer raster of the map of the world with pop values at each pixel
# (There are also multi-layer rasters called RasterBrick and RasterStack)

# If we want to know the population of a general region, all we have to do is find the
# boundaries of that region and sum over the population within those boundaries
# Boundaries are defined using shape files

# Read in a shape file
# A shape file stores data that partition landscapes into subparts, such as country borders
# GAUL compiles and disseminates information on administrative units for countries worldwide
# admin0 refers to the highest level of administrative unit - countries, in this case
# admin1 and higher are more fine-grained levels of administrative unit
world <- readShapePoly("/Volumes/snfs/WORK/11_geospatial/06_original shapefiles/GAUL_admin/admin0/g2015_2014_0/g2015_2014_0.shp")

# Another shape file, this time using IHME's definitions for administrative regions
# This will have different names/codes for different countries
GBD <- readShapePoly("/Volumes/snfs/DATA/SHAPE_FILES/GBD_geographies/master/GBD_2016/master/shapefiles/GBD2016_analysis_final.shp")

# Look at the data frame:
world
# Pull out the part of the data frame that describes the borders of Uganda
UGA <- world[world$ADM0_NAME=="Uganda",]
# We can even plot Uganda as well
plot(UGA)

# Extract the population from the raster data
# fun is the function used to aggregate data
# na.rm refers to removing the parts of the map with "NA" values given, ie water
UGA.pop <- extract(worldpop, UGA, fun = sum, na.rm = TRUE)
print(UGA.pop)

# Extract the population from the raster data using the GBD's shape file:
UGA.GBD <- GBD[GBD$loc_name=="Uganda",]
plot(UGA.GBD)

# Exercise: pull out the full population of the world (this takes a long time)

# Exercise: locate all of the countries in sub-Saharan Africa
SSA <- GBD[GBD$spr_reg_id==166,]
plot(SSA)

# Exercise: locate all of the sub-regions in Uganda
world.admin1 <- readShapePoly("/Volumes/snfs/WORK/11_geospatial/06_original shapefiles/GAUL_admin/admin1/g2015_2014_1/g2015_2014_1.shp")
UGA.admin1 <- world.admin1[world.admin1$ADM0_NAME=="Uganda",]
plot(UGA.admin1)
# Print out an ordered list of subregions of UGA
UGA.admin1$ADM1_NAME[order(UGA.admin1$ADM1_NAME)]
# pull out Tororo
TOR <- UGA.admin1[UGA.admin1$ADM1_NAME=="Tororo",]
plot(TOR)
# what is the population of Tororo?
TOR.pop <- extract(worldpop, TOR, fun = sum, na.rm = TRUE)
print(TOR.pop)

# can we show the raster map within the borders of Uganda?
# https://gis.stackexchange.com/questions/92221/extract-raster-from-raster-using-polygon-shapefile-in-r
# use the crop() function
UGA.pop <- crop(worldpop, extent(UGA))
plot(UGA.pop)
# Now, unfortunately the extent() function only returns a rectangle, but we can cut off the edges
# of that rectangle using the mask() function with the borders of Uganda as an argument:
UGA.pop.mask <- mask(UGA.pop, UGA)
plot(UGA.pop.mask)
# Superpose the admin regions over the image
plot(UGA.admin1, add = TRUE)
# Now let's plot Tororo
TOR.pop <-crop(worldpop, extent(TOR))
TOR.pop.mask <- mask(TOR.pop, TOR)
# Plot!
# https://stackoverflow.com/questions/9280737/plotting-a-raster-behind-a-shapefile
plot(TOR.pop)
plot(TOR, add = TRUE)
plot(TOR.pop.mask)


# How do I make this beautiful and interactive?
library(leaflet)
#https://rstudio.github.io/leaflet/markers.html
leaflet(TOR) %>% addTiles() %>% addRasterImage(TOR.pop.mask)
leaflet(TOR) %>% addTiles() %>% addPolygons()

leaflet(UGA) %>% addPolygons()
leaflet(UGA.pop) %>% addPolygons()

leaflet(UGA.admin1) %>% addPolygons()

# Plot the UGA administrative districts, along with the population raster
leaflet(UGA.admin1) %>% addRasterImage(UGA.pop.mask) %>% addPolygons()

# Adjacency to Tororo:
# https://aledemogr.wordpress.com/2015/10/09/creating-neighborhood-matrices-for-spatial-polygons/
library(maptools)
library(Matrix)
library(spdep)
# Generate a "neighbors" list
nb.FOQ = poly2nb(UGA.admin1, queen = TRUE, row.names = UGA.admin1$ADM1_NAME)
# can plot that neighbors list, just to show how the adjacencies are encoded
plot(nb.FOQ, coordinates(UGA.admin1))
plot(UGA.admin1, add = TRUE)
# but how on earth do I actually manipulate the output here?
# If I want to explore the graph in any other format, or if I want to extract the names of the neighbors of a particular site?]
# convert the nb to a matrix, where the rownames are given by the admin1 names
mat <- nb2mat(nb.FOQ)>0
# These are the neighbors of Tororo
TOR.neighbors <- rownames(mat)[mat['Tororo',]]

# Here's a slightly less awkward way of doing this:
# This generates a list of polygons, which is slightly different from the above syntax...
TOR.neighbors.shape <- lapply(X=TOR.neighbors, FUN = function(x){UGA.admin1[UGA.admin1$ADM1_NAME==x,]})
# Join together polygons
# taken from: https://gis.stackexchange.com/questions/180682/merge-a-list-of-spatial-polygon-objects-in-r
joined = SpatialPolygons(lapply(TOR.neighbors.shape, function(x){x@polygons[[1]]}))
plot(joined, col = "Blue")
plot(TOR, col = "Green", add = TRUE)
plot(UGA.admin1, add = TRUE)
# And what about instead generating the equivalent of the long argument from above,
# a single logical array that picks out the neighbors
# Can use lapply, which generates a list of logical arrays,
# and then use the Reduce() function to combine each row of the list
y <- Reduce("|", lapply(X = TOR.neighbors, FUN = function(x){UGA.admin1$ADM1_NAME==x}))
TOR.neighbors.plot.1 <- UGA.admin1[y,]
plot(TOR.neighbors.plot.1, col = "Blue")
plot(TOR, col = "Green", add = TRUE)
plot(UGA.admin1, add = TRUE)

# rgeos also has a way to do this?
