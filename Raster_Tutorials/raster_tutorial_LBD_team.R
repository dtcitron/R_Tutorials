# Working with Spatial Data
# Ani D - 1/9/2020

# 1. Points
# Any value that has been referenced on some geometric plane
pt <- data.table(x = c(2,2), y = c(4,3), size = c(3,4))
plot(pt$x, pt$y)

library(ggplot2)
ggplot() + geom_point(data = pt, aes(x = x, y = y, color = size))


# 2. Rasters
# data represented on a square grid, each square is the same size
# each square contains a value corresponding to the variable that
# the raster is displaying
# eg Population Raster
library(raster)

A <- matrix(c(1,2,3,4), nrow = 2)
# create a raster, where the matrix element position is a spatial position, 
# and the element values are the values to be plotted
A.raster <- raster(A)
plot(A.raster)

# Load some data from the cluster
dir <- c("/Volumes/snfs/WORK/11_geospatial/01_covariates/00_MBG_STANDARD/")
cov <- c("worldpop/total/1y/worldpop_total_1y_1980_00_00.tif")

pop <- raster(paste0(dir,cov))
plot(pop)

# Shape files
# vector data, defining an area with a boundary
testshp <- shapefile("/Volumes/snfs/WORK/11_geospatial/admin_shapefiles/current/lbd_standard_admin_0_stage_2_simplified.shp")
head(testshp@data)
# Subset out Yemen
yem <- testshp[testshp@data$ADM0_NAME == "Yemen",]
plot(yem) # hey it's Socotra!

# perform overlay analysis
library(sf)
# zonal analysis - population inside the yemen spdf
yem_sf <- st_as_sf(yem)
yem_sf
# note the "simple feature" which makes some things easier, such as subsetting or plotting
ggplot() + geom_sf(data = yem_sf, fill = NA, color = "red")


# To do:
# visualize point data and shapefile using one ggplot
dataset <- read.csv("/Volumes/snfs/temp/adesh/rclass/w_piped_small.csv", stringsAsFactors = FALSE)
head(dataset)
dataset[dataset$country == "YEM",]

ggplot() + 
  geom_point(data = dataset[dataset$country == "YEM",], aes(x = longitude, y = latitude, col = surv_year)) + 
  geom_sf(data = yem_sf, fill = NA, color = "red")
