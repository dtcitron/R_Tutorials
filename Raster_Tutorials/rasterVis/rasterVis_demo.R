library(raster)
library(rasterVis)

##Solar irradiation data from CMSAF, stored at ~/rasterVis/SISmm2008_CMSAF/
setwd('~/Documents/R Tutorials/rasterVis/SISmm2008_CMSAF')
# List of all data files
listFich <- dir(pattern='\\.nc')
stackSIS <- stack(listFich)
# Create a raster brick with all of the data files contained in the list of files
stackSIS <- stackSIS * 24 ##from irradiance (W/m2) to irradiation Wh/m2

idx <- seq(as.Date('2008-01-15'), as.Date('2008-12-15'), 'month')

SISmm <- setZ(stackSIS, idx)
names(SISmm) <- month.abb

# Create some plots
levelplot(SISmm, layers = 1)
