# https://geoscripting-wur.github.io/IntroToRaster/

library(raster)

# Generate a RasterLayer object
r <- raster(ncol = 40, nrow = 20)
class(r)
r

# Fill in the values of our new raster object
r[] <- rnorm(n = ncell(r))

# RasterLayer objects are single-layer objects, but there are also multi-layer
# raster objects RasterStack and RasterBrick

# Create a 3-layer stack
s <- stack(x = c(r, r*2, r))
# Create a 3-layer brick
b <- brick(x = c(r,r*2, r))
# Plot all layers of the brick
plot(r)
# Plot just a single layer of the brick
plot(b$layer.1)


# Manipulating raster objects
# I/O from/to a file - usually we'll be looking at Geo data, such as stored in a Geo-TIF file
#
# setwd("/Users/dtcitron/Documents/R Tutorials")
# download.file(url = 'https://raw.githubusercontent.com/GeoScripting-WUR/IntroToRaster/gh-pages/data/gewata.zip',
# destfile = 'gewata.zip', method = 'auto')
# unzip('gewata.zip')
# for showing files with .tif
# list.files(pattern = glob2rx('*.tif'))
# Load in the file we just downloaded as a brick:
gewata <- brick("LE71700552001036SGS00_SR_Gewata_INT1U.tif")
gewata
# print the number of layers in our brick
nlayers(gewata)
# print the names of the layers
names(gewata)
# If we only want to read in a single layer, we can read in the file as a raster 
# instead of as a brick and specify which layer we want
gewataB1 <- raster("LE71700552001036SGS00_SR_Gewata_INT1U.tif", 1)

# Cropping a raster object
# crop() allows us to limit the extent of a raster function
# Its first argument is a raster object
# Its second argument is an extent object, which can be generated using extent(), or can
# be passed in really flexibly
plot(gewata, 1)
# This is a very cool function that lets us define an extent by actually drawing on the plotted raster!
e <- drawExtent(show = TRUE)
# Crop our raster using the extent we just made
gewataSub <- crop(gewata, e)
# Now plot!
plot(gewataSub)


# Raster arithmetic - pretty straightforward, all we need to do is add/subtract etc
# Practice raster arithmetic using normalized difference vegetation index - 
# NDVI is a measure for estimating vegetation (or other things) from remote sensing data:
ndvi <-  (gewata[[4]] - gewata[[3]])/(gewata[[4]] + gewata[[3]])
ndvi
plot(ndvi)
plot(crop(ndvi, e))
# This, of course, is not the best way to perform these types of calculations if we have
# extremely large raster objects.  Instead, use calc(), which is more RAM friendly
ndvCalc <- function(x) {
  ndvi <-  (x[[4]] - x[[3]])/(x[[4]] + x[[3]])
  return(ndvi)
}
ndvi2 <- calc(gewata, ndvCalc)
# Check that our calculation did the right thing:
all.equal(ndvi, ndvi2)


# Re-projections:
# We don't actually know where this geo data was taken from, so let's try projecting it onto Google Earth
# First, we have to re-project our data into Lat/Long
# Use the projectRaster() function, which takes a raster object and a crs proj4 expression as arguments
# crs are strings that provide parameters of cartographic projections
# (An alternative is gdalwarp)
ndviLL <- projectRaster(ndvi, crs = '+proj=longlat')
ndviLL
# export as a file that can be read using Google Earth:
KML(x=ndviLL, filename='gewataNDVI.kml')
# It's now projected onto Ethiopia!


# Raster arithmetic: performing value replacements
# We will work with Landsat data on surface reflectance, correcting for cloud coverage
# Our data set will include information about the presence/absence of cloud coverage as well as shadowing effects
# Download our data set (Tahiti!)
#download.file(url='https://raw.githubusercontent.com/GeoScripting-WUR/IntroToRaster/gh-pages/data/tahiti.zip', 
#              destfile='tahiti.zip', method='auto')
#unzip(zipfile='tahiti.zip')
#
# Load in our data as a RasterBrick:
tahiti <- brick("LE70530722000126_sub.grd")
tahiti
# 7 layers:
names(tahiti)
# Plot RGB
plotRGB(tahiti, r = 3, g = 4, b = 5)
# The cloud mask layer is in the 7 layer marked "cfmask"
plot(tahiti, 7)
# Extract the cloud layer from the brick:
cloud <- tahiti[[7]]
# Want to not change anything that is coded as having no clouds or shadowing:
# Replace `clear land` with NA
cloud[cloud == 0] <- NA
# Now, plot the two of them together, showing which parts of the map are cloud-influenced and which are clear
plotRGB(tahiti, 3, 4, 5)
plot(cloud, add = TRUE, legend = FALSE)
# Drop the cloud mask layer from the tahiti data set:
fmask <- tahiti[[7]]
tahiti6 <- dropLayer(tahiti, 7)
# Perform a value replacement on the new tahiti data set
# This finds all the places where the cloud layer is relevant, and crosses them out:
tahiti6[fmask != 0] <- NA
plotRGB(tahiti6, 3,4,5)
# This is doable simply because the data set is not very big.
# To do this more intelligently, we should use calc() or overlay()
# Define a function that removes all relevant pixels from fmask from the tahiti6 raster:
cloud2NA <- function(x,y){
  x[y != 0] <- NA
  return(x)
}
tahiti6_2 <- dropLayer(tahiti, 7)
tahitiCloudFree <- overlay(x = tahiti6_2, y = fmask, fun = cloud2NA)
plotRGB(tahitiCloudFree, 3,4,5)


# Exercise: assess change in NDVI over time
# after downloading, untar
list.files()
# one file from 12/31/2014
untar("LC81970242014109-SC20141230042441.tar", exdir="CloudCoverage")
# one file from 01/07/2015
untar("LT51980241990098-SC20150107121947.tar", exdir="CloudCoverage")
# Our different tiff layers are all in different files! Let's try create a stack that includes each of these
list.files('CloudCoverage/', pattern = glob2rx('*.tif'), full.names = TRUE)
list1 <- list.files('CloudCoverage/', pattern = glob2rx('LC81970242014109LGN00*.tif'), full.names = TRUE)
list2 <- list.files('CloudCoverage/', pattern = glob2rx('LT51980241990098KIS00*.tif'), full.names = TRUE)
data1 <- stack(list1)
data2 <- stack(list2)
data1
plot(data1$LC81970242014109LGN00_sr_band3)
plot(data2$LT51980241990098KIS00_sr_band3)
# Now, unfortunately, the extents on each of these data sets are *different*
# Let's use the intersect function to find the overlapping parts of the two sets:
extent(data1)
extent(data2)
overlap <- extent(intersect(data1, data2))
# now, crop each of the data sets using the new extent
data1.crop <- crop(data1, overlap)
data2.crop <- crop(data2, overlap)
plotRGB(data1.crop, 4,5,6)
data2.crop
# They now have the same extent, and can be compared directly
# Let's now calculate NDVI using calc() (a good practice)
calcNDVI_1 <- function(x){
  return((x[[5]]-x[[4]])/(x[[5]]+x[[4]]))
}
calcNDVI_2 <- function(x){
  return((x[[7]]-x[[6]])/(x[[7]]+x[[6]]))
}
data1.ndvi <- calc(data1.crop, calcNDVI_1)
data2.ndvi <- calc(data2.crop, calcNDVI_2)
plot(data1.ndvi)
plot(data2.ndvi)
# Calculate the difference between the two data sets
calcDelta <- function(x,y){
  return(y-x)
}
delta.ndvi <- overlay(x = data1.ndvi, y = data2.ndvi, fun = calcDelta)
plot(delta.ndvi)
