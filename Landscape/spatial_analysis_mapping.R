# https://us.sagepub.com/en-us/nam/an-introduction-to-r-for-spatial-analysis-and-mapping/book241031

#install.packages("GISTools", dep = TRUE)
library(sp)
library(rgeos)
library(maptools)
library(MASS)
library(RColorBrewer)
library(GISTools)

colours <- factor(c("red", "blue", "red", "white","silver", "red", "white", "silver",
                    "red", "red", "white", "silver", "silver"),
                  levels = c("red","blue", "white", "silver","black"))
car.type <- factor(c("saloon", "saloon", "hatchback", "saloon", "convertible",
                     "hatchback", "convertible", "saloon", "hatchback", "saloon",
                     "saloon", "saloon", "hatchback"))

table(colours, car.type)
table(car.type, colours)

crosstab <- table(car.type, colours)

# An ordered factor
engine <- ordered(c("1.1litre", "1.3litre", "1.1litre", "1.3litre", "1.6litre",
                    "1.3litre", "1.6litre", "1.1litre", "1.3litre", "1.1litre",
                    "1.1litre", "1.3litre", "1.3litre"))

# Colors of all cars with engines with greater than 1.1 litre capacity
colours[engine > "1.1litre"]
# Counts of all cars with capacity below 1.6 litres
table(car.type[engine < "1.6litre"])
# Counts of colors of all hatchbacks with capacity >= 1.3 litres
table(colours[engine >= "1.3litre" & car.type == "hatchback"])

# find the largest value of each row of crosstab
apply(crosstab, 1, max)
# find the index of the largest value of each row of crosstab
apply(crosstab, 1, which.max) # note which max only gives the first index, not all indices

which.max.name <- function(x) {
  return(names(x)[which.max(x)])
}
example <- c(1.4, 2.6, 1.1, 1.5, 1.2)
names(example) <- c("Leicester", "Nottingham", "Loughborough", "Birmingham", "Coventry")
which.max.name(example)


#find the row name with the largest value for the red column:
row.names(crosstab)[which.max(crosstab[,1])]
# write a different apply expression, that gives the row name with the largest value
# for each column:
apply(crosstab, 2, which.max.name)
# and the column name with the largest value for each row
apply(crosstab, 1, which.max.name)

# Create a list of both of these results
# colour: What is the best selling color for each car type?
# type: what is the best selling type of each color?
most.popular <- list(colour = apply(crosstab, 1, which.max.name), type = apply(crosstab, 2, which.max.name))
most.popular$type

# Create a class based on the list from above using a function that sets the class type:
new.sales.data <- function(colours, car.type) {
  xtab <- table(car.type, colours)
  result <- list(colour = apply(xtab, 1, which.max.name),
                 type = apply(xtab, 2, which.max.name),
                 total = sum(xtab))
  class(result) <- "sales.data"
  return(result)
}
# Create an instance of sales.data object:
this.week <- new.sales.data(colours, car.type)
this.week

# Plotting:
x1 <- rnorm(100)
y1 <- rnorm(100)
plot(x1, y1, pch = 16, col = 'red')
x2 <- seq(0, 2*pi, len = 100)
y2 <- sin(x2)
plot(x2, y2, type = 'l', lwd = 3, col = 'darkgreen', ylim = c(-1.2, 1.2))
y2r <- y2 + rnorm(100, 0, 0.1)     
points(x2, y2r, pch = 16, col = 'red')
y4 <- cos(x2)
lines(x2, y4, lwd = 3, lty = 2)
# Plotting polygons
x2 <- seq(0, 2*pi, len = 100)
y2 <- sin(x2)
y4 <- cos(x2)
# specify plot order
par(mfrow = c(1,2))
#par(mfrow = c(1,1))
plot(y2, y4)
polygon(y2, y4, col = 'lightgreen')
# start the second plot
# the "asp" argument sets the aspect ratio, in this case so that the axes have the same scale
plot(y2, y4, asp = 1, type = 'n')
polygon(y2, y4, col = 'orange')

# Plotting with some geographic data
library(GISTools)
# Load in 3 data sets: georgia, georgia2, georgia.polys
data(georgia)
# select the first element of the polygons data set, the outline of Appling county
appling <- georgia.polys[[1]]
# This is what the polygon looks like, without the borders shown!
# (polygon data is stored as a 2-column matrix of points)
plot(appling)
# Now the actual plot:
plot(appling, asp = 1, type = 'n', xlab = 'Easting', ylab = 'Northing')
polygon(appling, col = rgb(0,.5,.7,1))#, density = 6, angle = 135)
# Adding a label, setting the size using Character EXpansion "cex"
text(1287000, 1053000, "Appling County", cex = 1.5)
text(1287000, 1049000, "Georgia", col = 'darkred')
# use the locator() function to determine the coordinates of where the labels should go!



# Chapter 3 Handling SPatial Data in R
library(GISTools)
# Load in some data from New Haven.  This data set includes crime stats from the New Haven Crime Log.
data(newhaven)
ls()
# Plot road map of New Haven:
plot(roads)
# The class of the roads data is SpatialLinesDataFrame, which is defined in the sp package
head(data.frame(blocks))
plot(blocks)
plot(roads, add = TRUE, col = 'red')
# Plot locations of reported "breach of peace"
plot(blocks)
plot(breach, add = TRUE, col = 'red', pch = 1)
# and add a scale bar - defined by coordinates, length scale (key parameter!)
# name of units, number of gradations in scale, size of gradation
map.scale(534750, 152000, miles2ft(2), "Miles", 4, 0.5)
# add arrow for orientation!
north.arrow(534750, 154000, miles2ft(0.25), col = "lightblue")
# title
title('New Haven, CT')
# See: newhavenmap.R for practice with "sourcing"

# More on Mapping Spatial Objects
# Select a subset of specific counties and display them using an OpenStreetMap backdrop:
data(georgia)
plot(georgia, col = "red", bg = "wheat")
# Merge together the counties in Georgia to generate the full outline so we can highlight it
georgia.outline <- gUnaryUnion(georgia, id = NULL)
plot(georgia, col = "red", bg = "wheat", border = 'blue', lty = 2)
plot(georgia.outline, lwd = 3, add = TRUE)
title(main = "The State of Georgia", font.main = 2, cex.main = 1.5,
      sub = "and its counties", font.sub = 3, col.sub = "blue")

# Let's use the mfrow parameter to expand the plot window to include multiple plots
par(mfrow = c(1,2))
par(mar = c(2,0,3,0))
# 1st plot
plot(georgia, col = "red", bg = "wheat")
title("Georgia")
# 2nd plot
plot(georgia2, col = "orange", bg = "lightyellow3")
title("Georgia 2")
# reset:
par(mfrow = c(1,1))

# Let's draw labels on each of our counties
# Start by extracting latitude and longitude
Lat <- data.frame(georgia)[,1]
Lon <- data.frame(georgia)[,2]
# extract the names of all of the counties
Names <- data.frame(georgia)[,13]
plot(georgia, col = NA)
pl <- pointLabel(Lon, Lat, Names, offset = 0, cex = 0.5)

# Let's now examine a particular subset from the data
county.tmp <- c(81, 82, 83, 150, 62, 53, 21, 16, 124, 121, 17)
georgia.sub <- georgia[county.tmp,]
par(mar = c(0,0,3,0))
plot(georgia.sub, col = 'gold1')
plot(georgia.outline, add = True, lwd = 3)
title("A subset of Georgia's counties", cex.main = 2, font.main = 1)
pl <- pointLabel(Lon[county.tmp], Lat[county.tmp], Names[county.tmp], offset = 3, cex = 1.5)

# Mapping attributes and data frames
data(newhaven)
# "Attach" a data frame, to enter the columns as variables!
attach(data.frame(blocks))
# now that the columns are their own variables, we can easily access the P_VACANT
# data without explicitly pulling it out of the data frame:
hist(P_VACANT)
# good practice to detach from any objects that I'm not using
detach(data.frame(blocks))

# The breach data set is just a list of points where breaches have occurred
# We can create a raster from that data set using a "kernel destiny estimate"
breach.dens = kde.points(breach, lims  = tracts)
summary(breach.dens)
head(data.frame(breach.dens))

# Mapping polygons and attributes
# Choropleth: a thematic map in which areas are shaded in proportion to their attributes
# P_VACANT is the percentage of vacant buildings in each census block
choropleth(blocks, blocks$P_VACANT)
# create a legend, to help make sense of this
vacant.shades <- auto.shading(blocks$P_VACANT, n = 7, cols = brewer.pal(7, "Greens"), cutter = rangeCuts)
choro.legend(533000, 161000, vacant.shades)

# Mapping Points and Attributes
# where in New Haven have there been breaches of the peace?
plot(blocks)
plot(breach, add = TRUE, col = 'red', pch = '@')

# Point data in tabular format by creating a Spatial Points Data set
# Define coordinates
# Assign these to a SpatialPoints, SpatialLines, or SpatialPolygons object
# If we're interested in attributes, keep as a Data Frame: eg SpatialPointsDataFrame 
# Earthquake events near Fiji
data(quakes)
head(quakes)
# Extract the lon/lat coordinates
coords.tmp <- cbind(quakes$long, quakes$lat)
# From the lat/lon coordinates, let's create a Spatial Points Data set
quakes.spdf <- SpatialPointsDataFrame(coords.tmp, data = data.frame(quakes))
# set the plot parameters to show 2 maps
par(mar = c(0,0,0,0))
par(mfrow = c(1,2))
# First plot, with defaults
plot(quakes.spdf)
# Second plot, with a transparency term added
plot(quakes.spdf, pch = 1, col = '#FB6A4A80')

# Now we can create another similar plot, this time visualizing the magnitude in different ways:
par(mfrow = c(2,2))
par(mar = c(0,0,0,0)) # sets the margins
# 1. create a choropleth
choropleth(quakes.spdf, quakes$mag)
# 2. Plot using a different shading scheme and pch
shades <- auto.shading(quakes$mag, n = 6, cols = brewer.pal(6, 'Greens'))
choropleth(quakes.spdf, quakes$mag, shading = shades, pch = 20)
# 3. Plot with transparency
shades$cols <- add.alpha(shades$cols, 0.5)
choropleth(quakes.spdf, quakes$mag, shading = shades, pch = 20)
# 4. Plot character size is determined by the attribute magnitude
tmp <- quakes$mag
tmp <- tmp - min(tmp)
tmp <- tmp/max(tmp)
plot(quakes.spdf, cex = tmp*3, pch = 1, col = 'orange')

# Mapping lines and attributes
# Extract a subset of the roads in New Haven, and plot by converting to a SpatialPolygonsDataFrame
data(newhaven)
# clip our region
xmin <- bbox(roads)[1,1]
ymin <- bbox(roads)[2,1]
xmax <- xmin + diff(bbox(roads)[1,])/2
ymax <- ymin + diff(bbox(roads)[2,])/2
xx = as.vector(c(xmin, xmin, xmax, xmax, xmin))
yy = as.vector(c(ymin, ymax, ymax, ymin, ymin))
# Create a spatial polygon from this
crds <- cbind(xx, yy)
Pl <- Polygon(crds)
ID <- 'clip'
Pls <- Polygons(list(Pl), ID=ID)
SPls <- SpatialPolygons(list(Pls))
df <- data.frame(value = 1, row.names = ID)
clip.bb <- SpatialPolygonsDataFrame(SPls, df)
# Clip out roads and the data frame
roads.tmp <- gIntersection(clip.bb, roads, byid = T)
tmp <- as.numeric(gsub("clip", "", names(roads.tmp)))
tmp <- data.frame(roads)[tmp,]
# Create the SLDF object
roads.tmp <- SpatialLinesDataFrame(roads.tmp, data = tmp, match.ID = FALSE)

# Let's visualize this subset of our roadmap in three ways
# Simple (default), shaded according to an attribute, linewidth based on an attribute
par(mfrow = c(1,3))
par(mar = c(0,0,0,0))
# 1. simple map
plot(roads.tmp)
# 2. mapping an attribute variable, road classification
road.class <- unique(roads.tmp$AV_LEGEND)
# this is our coloring scheme
shades <- rev(brewer.pal(length(road.class), "Spectral"))
tmp <- roads.tmp$AV_LEGEND
index <- match(tmp, as.vector(road.class))
plot(roads.tmp, col = shades[index], lwd = 3)
# 3. Mapping using the attribute to specify line width
plot(roads.tmp, lwd = roads.tmp$LENGTH_MI * 10)
# (reset)
par(mfrow = c(1,1))


# Mapping Raster Attributes
data(meuse.grid)
plot(meuse.grid$x, meuse.grid$y, asp = 1)
# Convert to a SpatialPixelsDataFrame
meuse.grid <- SpatialPixelsDataFrame(points = meuse.grid[c("x", "y")], data = meuse.grid)
par(mfrow = c(1,2))
par(mar = c(0.25,0.25,0.25,0.25))
image(meuse.grid, "dist", col = rainbow(7))
image(meuse.grid, "dist", col = heat.colors(7))

# Exercises
# 1. Create a map of the counties in Georgia, shaded in a color scheme using 11 classes describing the
# distribution of median income in thousands of dollars ($MedInc).
data(georgia)
# Write out the map to a TIFF file with a resolution of 300 dpi and a size of 7x7 inches
tiff("Quest3_1.tiff", width = 7, height = 7, units = 'in', res = 300)
shades <- auto.shading(georgia$MedInc/1000, n = 11, cols = brewer.pal(11, 'Spectral'))
choropleth(georgia, georgia$MedInc/1000, shading = shades)
choro.legend(-81.9, 35.1, shades, title = "Median Income (1000s $)", cex = 0.7)
dev.off()

# 2. Misrepresentation of continuous variables: using different cut functions for choropleth mapping
# Create 3 maps using different cut functions to divide the HSE_UNITS in the new haven BLOCKS data set
data(newhaven)
# Create our shading schemes using different cut functions
sd.shades <- auto.shading(blocks$HSE_UNITS, n = 5, cutter = sdCuts, cols = brewer.pal(5, "Spectral"))
range.shades <- auto.shading(blocks$HSE_UNITS, n = 5, cutter = rangeCuts, cols = brewer.pal(5, "Spectral"))
q.shades <- auto.shading(blocks$HSE_UNITS, n = 5, cutter = quantileCuts, cols = brewer.pal(5, "Spectral"))
# Define the plotting window (so that the titles go in the right place)
if (.Platform$GUI == "AQUA") {
  quartz(w = 10, h = 6)} else {
    x11(w = 10, h = 6)
  }
tiff("Quest3_2.tiff", width = 7, height = 3, units = 'in', res = 300)
par(mfrow = c(1,3))
par(mar = c(.25,.25,2,.25))
par(lwd = 0.7)
choropleth(blocks, blocks$HSE_UNITS, shading = sd.shades)
title("Standard Deviation Cuts", cex.main = 1.3)
choro.legend(532000, 160000, sd.shades)
choropleth(blocks, blocks$HSE_UNITS, shading = range.shades)
title("Range Cuts", cex.main = 1.3)
choro.legend(532000, 160000, range.shades)
choropleth(blocks, blocks$HSE_UNITS, shading = q.shades)
title("Quantile Cuts", cex.main = 1.3)
choro.legend(532000, 160000, q.shades)
dev.off()

# Selecting data: creating variables and subsetting data
# Find all counties in GA have a rural population density greater than 20/square kilometer
data(georgia)
# First, find the fraction of people living in rural places:
rur.pop <- georgia$PctRural*georgia$TotPop90/100
# Second, find the areas of each of the places
GA.area <- gArea(georgia2, byid = TRUE)
GA.area <- GA.area/(1000*1000)
h <- rur.pop/GA.area > 20
plot(georgia[h,], col = 'goldenrod')
plot(georgia[!h,], col = 'darkgreen', add = TRUE)
legend(-82.33178, 35.20039, legend = "Rural", pch = 19, bty = 'n', col = 'goldenrod')
legend(-82.33178, 35.0039, legend = "Urban", pch = 19, bty = 'n', col = 'darkgreen')
title("GA counties with rural population density > 20 per square km")

# Chapter 4 Programming in R
# Control loops and structures; create, test, and use functions; automate short tasks

# Defining a function
# if this is a function that I will be using in a large number of programs, I can save it to a 
# separate file (functions.R) and then source it using source("functions.R")
cube.root <- function(x) {
  if (is.numeric(x)){
    if (x > 0){
      cr <- x^(1/3)
    } else {
      cr <- -1*(-x)^(1/3)
    }
    return(cr)
  } else {
    cat("WARNING: argument not numeric")
    return(NA)
  }
}
cube.root(27)
cube.root(-343)
cube.root("Leicester")

# Conditional loop example
# Euclid's algorithm for finding the greatest common divisor, using a repeat loop
gcd <- function(a,b){
  divisor <- min(a,b)
  dividend <-  max(a,b)
  repeat {
    remainder <- dividend %% divisor
    dividend <- divisor
    divisor <- remainder
    if (remainder == 0) break
  }
  return(dividend)
}
gcd(6,15)
gcd(25,75)
gcd(31, 33)

# Self-test question 3: 
# i) write a function to compute and print out gcd(x, 60) for all numbers 1 through 60
for (i in c(1:60)){
  print(gcd(i, 60))
}
# ii) write a function to compute and print out gcd(x,y) for ranges of x and y:
gcds <- function(n1, n2){
  res <- matrix(NA,nrow=n1,ncol=n2)
  for (i in c(1:n1)){
    for (j in c(1:n2)){
      res[i,j]=gcd(i,j)
    }
  }
  return(res)
}
# Self-test question 4: write a function that calculates cube roots from 0 to n in steps of 0.5
# make sure it works when negative values are passed to it!
cube.root.table <- function(n){
  if (n > 0){
    inputs <- seq(0,n, 0.5)
  } else {
    inputs <- seq(0,n, -0.5)
  }
  outputs <- array(NA, dim = length(inputs))
  for (i in 1:length(inputs)){
    outputs[i] <- cube.root(inputs[i])
  }
  return(outputs)
}

inputs <- seq(0,-2, by = -.5)
for (i in 1:length(inputs)){
  print(cube.root(i))
}

# Writing Functions for Spatial Data
library(GISTools)
data(georgia)
# Draw polygons in a list!
# First create a plot with the right boundaries
plot(c(939200, 1419420), c(905510, 1405900), asp=1, type='n',
     xlab='', ylab='', xaxt='n', yaxt='n')
# polygon() adds a polygon to the current plot
# each row of georgia.polys is a different county
# apply polygon to each item in the list of counties:
invisible(lapply(georgia.polys, polygon)) # invisible hides all of the NULLs returned here
# Automatically choose a bounding box by looping over every polygon and retrieving the boundaries
poly1 <- georgia.polys[[1]]
min(poly1[,1]) # most eastern point in polygon 1
max(poly1[,1]) # most western point in polygon 1
min(poly1[,2]) # most southern point in polygon 1
max(poly1[,2]) # most northern point in polygon 1

most.eastern.point <- function(polys){
  most.eastern.list <- lapply(georgia.polys, function(poly){return(min(poly[,1]))})
  return(min(unlist(most.eastern.list)))
}
most.eastern.point(georgia.polys)
# Now write similar functions for the other 3 extents of our map:
most.western.point <- function(polys){
  most.western.list <- lapply(georgia.polys, function(poly){return(max(poly[,1]))})
  return(max(unlist(most.western.list)))
}
most.southern.point <- function(polys){
  most.southern.list <- lapply(georgia.polys, function(poly){return(min(poly[,2]))})
  return(min(unlist(most.southern.list)))
}
most.northern.point <- function(polys){
  most.northern.list <- lapply(georgia.polys, function(poly){return(max(poly[,2]))})
  return(max(unlist(most.northern.list)))
}
most.western.point(georgia.polys)
most.southern.point(georgia.polys)
most.northern.point(georgia.polys)
# now, automatically size the window and re-plot
plot(c(most.eastern.point(georgia.polys), most.western.point(georgia.polys)), 
     c(most.southern.point(georgia.polys), most.northern.point(georgia.polys)),
     asp=1, type='n', xlab='', ylab='', xaxt='n', yaxt='n')
invisible(lapply(georgia.polys, polygon))


# Shaded maps
# Classifying counties as either rural or urban, depending on whether or not more than 50% of the pop is rural
classifier <- factor(ifelse(georgia$PctRural > 50, 'rural', 'urban'))
fill.cols <- vector(mode ="character", length = length(classifier))
fill.cols[classifier=="urban"] <- "goldenrod"
fill.cols[classifier=="rural"] <- "darkgreen"
# we draw the map using mapply, where the color is set according to fill.cols:
plot(c(most.eastern.point(georgia.polys), most.western.point(georgia.polys)), 
     c(most.southern.point(georgia.polys), most.northern.point(georgia.polys)),
     asp=1, type='n', xlab='', ylab='', xaxt='n', yaxt='n')
invisible(mapply(polygon, georgia.polys, col=fill.cols))
# or, we can distinguish by changing the hatching using a slightly different classifier
fill.hatches <- vector(mode = "numeric", length = length(classifier))
fill.hatches[classifier=="urban"] <- 40
fill.hatches[classifier=="rural"] <- 10
invisible(mapply(polygon, georgia.polys, col=fill.cols, density=fill.hatches))


# Chapter 5: Using R as a GIS
# GIS is all about comparing different landscapes, to investigate correlations, or co-occurrences
# We can clip one data set to match the extent of another
# We can merge features in a spatial dataset
# Point-in polygon, area calculations, distance calcuations
# Combining spatial data and attributes
# Answer questions: how does X interact with Y?  How many of X are there in different locations Y?
# How does process X vary with Y across space?

# Example: tornado data - 
# torn and torn2 represent locations of tornados; 
# us_states and us_states2 describe locations of states
data(tornados)
par(mar = c(0,0,0,0))
plot(us_states)
plot(torn, add = TRUE, pch = 1, col = 'red', cex = 0.2)
plot(us_states, add = TRUE)
# Now, aim focus at a part of the south middle US:
index <- us_states$STATE_NAME=="Texas" | us_states$STATE_NAME=="New Mexico" | us_states$STATE_NAME=="Arkansas" | us_states$STATE_NAME=="Oklahoma"
AoI <- us_states[index,]
# we don't want to plot the entire point set of tornados, so let's clip the tornados to just Area of Interest:
# gIntersection creates a new SpatialPoints data set of all tornados within AoI
AoI.torn <- gIntersection(AoI, torn)
par(mar = c(0,0,0,0))
plot(AoI)
plot(AoI.torn, add = T, pch = 1, col = 'red', cex = 0.2)
# If we want to preserve the original data for the points, us byid =TRUE
# This preserves the state for each point (indexed by a number), and the index of each point from torn
# And this can be used to attach different attributes to our data points
AoI.torn <- gIntersection(AoI, torn, byid = TRUE)

# Buffer - let's look at TX + a region immediately outside
AoI <- us_states2[us_states2$STATE_NAME=="Texas",]
AoI.buf <- gBuffer(AoI, width = 25000) # width in meters?
par(mar = c(0,0,0,0))
plot(AoI.buf)
plot(AoI, add = TRUE, border = "blue")
# This can also be split across lots of different counties, by ID
data(georgia)
buf.t <- gBuffer(georgia2, width = 5000, byid = TRUE, id = georgia2$Name)
plot(buf.t)

# Merge Spatial features
# We can use merges to agglomerate polygons together as a group, such as merging all US states into a single region:
AoI.merge <- gUnaryUnion(us_states)
# here are the individual states
plot(us_states, border = "darkgreen", lty = 3)
# here's the outline around those individual states
plot(AoI.merge, add = T, lwd = 2)

# Point-in-Polygon calculations
# count the number of points in a SpatialPoints data set that fall inside each zone in a polygon dataset
# use poly.counts() from GISTools
torn.count <- poly.counts(torn, us_states)
head(torn.count)
choropleth(us_states, torn.count)

# Area calcualtions
# calculate the area of a polygon (in squared map units - so need to pay attention to length scale)
# Check on the projection of a data set:
proj4string(us_states2) # <- notice how the "units" are meters
poly.areas(us_states2)/(1000*1000) # <- the dividing factor converts to square km
# Self Test Question 1: produce maps of densities of breaches of the peace in New Haven in "breaches per sq mi"
data(newhaven)
# obtain areas for each block in newhaven
proj4string(blocks)
# obtain counts of breaches of the peace
bb <- poly.counts(breach, blocks)
bb <- bb/(poly.areas(blocks)/(5280*5280))
par(mfrow = c(1,2))
choropleth(blocks, bb)
plot(blocks)
plot(breach, col = 'red', add = TRUE)
par(mfrow = c(1,1))
# we can use the cor() function to measure the correlation between breaches of the peace
# and owner-occupied housing in each block:
cor(blocks$P_OWNEROCC, bb)
plot(blocks$P_OWNEROCC, bb)
# WHat if we take a more appropriate model:
breaches ~ Poisson(AREA * exp(a + b * blocks$P_OWNEROCC))
attach(newhaven)
attach(data.frame(blocks))
n.breaches <- poly.counts(breach, blocks)
area <- ft2miles(ft2miles(poly.areas(blocks)))
# 'general linearized model', specifying a Poisson model
model1 <- glm(n.breaches~P_OWNEROCC, offset = log(area), family = poisson)
detach(data.frame(blocks))
summary(model1)
# now, plot the residuals as a choropleth:
s.resids = rstandard(model1)
resid.shades <- shading(c(-2,2), c("red","grey","blue"))
choropleth(blocks, s.resids, resid.shades)


# Distance attributes
# "distance measures are used to evaluate differences in accessibility for different social groups"
# places is a SpatialPoints data set that represents the locations of facilities that we want to make sure
# everyone has equal access to
data(newhaven)
proj4string(places) <- CRS(proj4string(blocks))
# Calculate center of mass "centroids" of each of the blocks in New Haven:
centroids. <- gCentroid(blocks, byid = TRUE, id = rownames(blocks))
# calculate distances from places to centroids (from places to centroid of each block)
distances <- gDistance(places, centroids., byid = T)
distances <- ft2miles(distances)
# find the minimum distance from each block to a facility (place)
min.dist <- as.vector(apply(distances, 1, min))
# is everyone within one mile of a supply facility?
access <- min.dist < 1
# Extract the percent of each ethnicity of each block:
ethnicity <- as.matrix(data.frame(blocks[,14:18])/100)
ethnicity <- apply(ethnicity, 2, function(x) (x * blocks$POP1990))
# Can check: rowSums(ethnicity) == block populations
colnames(ethnicity) <- c("White", "Black", "Native American", "Asian", "Other")
# Use crosstabulation to bring together the access data with each of the populations
mat.access.tab <- xtabs(ethnicity ~ access)
data.set <- as.data.frame(mat.access.tab)
colnames(data.set) <- c("Access", "Ethnicity", "Freq")
data.set
# Now, we are going to use a general linearized model to look for a correlation between access and ethnicity
modelethnic <- glm(Freq ~ Access*Ethnicity, data = data.set, family = poisson)
mod.coefs <- summary(modelethnic)$coef
# Subtract 1 from each of the coefficients and convert to percentages
# This gives likelihoods of access for different groups compared to ethnic group White
tab <- 100*(exp(mod.coefs[,1]) - 1)
tab <- tab[7:10]
names(tab) <- colnames(ethnicity)[2:5]
tab
mosaicplot(t(mat.access.tab), xlab='',ylab='Access to Supply', main = "Mosaic Plot of Access", shad=TRUE, las=3, cex=0.8)


# Combining Spatial Datasets and their Attributes
# a common situation in spatial analysis is the need to overlay different polygon features
# for example, we can determine the proportions of objects in X in each of the objects in Y

# Start with a zone dataset with aim of calculating the number of houses in each zone
data(newhaven)
bb <- bbox(tracts)
grd <- GridTopology(cellcentre.offset = c(bb[1,1]-200, bb[2,1] - 200),
                    cellsize = c(10000,10000), cells.dim = c(5,5))
int.layer <- SpatialPolygonsDataFrame(as.SpatialPolygons.GridTopology(grd),
                                      data = data.frame(c(1:25)), match.ID = FALSE)
names(int.layer) <- "ID"
# Two sp objects can only be intersected if they have the same proj4 string:
proj4string(int.layer)
proj4string(tracts)
# Intersect them:
int.res <- gIntersection(int.layer, tracts, byid = TRUE)
# Create a nice plot of the intersection:
par(mfrow = c(1,2))
par(mar = c(0,0,0,0))
plot(int.layer, lty = 2)
Lat <- as.vector(coordinates(int.layer)[,2])
Lon <- as.vector(coordinates(int.layer)[,1])
Names <- as.character(data.frame(int.layer)[,1])
# now plot the tracts
plot(tracts, add = TRUE, border = "red", lwd = 2)
pl <- pointLabel(Lon, Lat, Names, offset = 0, cex = 0.7)
# set plot extent
plot(int.layer, border = "white")
plot(int.res, col = blues9, add = TRUE)
# the names of the different regions
names(int.res) 
tmp <- strsplit(names(int.res), " ")
tracts.id <- (sapply(tmp, "[[", 2))
intlayer.id <- (sapply(tmp, "[[", 1))
# Proportions of original tract areas in each of the grid locations need to be extracted:
int.areas <- gArea(int.res, byid = T)
tract.areas <- gArea(tracts, byid = T)
# match these to the new layer
index <- match(tracts.id, row.names(tracts))
tract.areas <- tract.areas[index]
# proportions of each area in each tract area
tract.prop <- zapsmall(int.areas/tract.areas, 3) # this is a rounding function
# create a new data frame for this layer
df <- data.frame(intlayer.id, tract.prop)
houses <- zapsmall(tracts$HSE_UNITS[index] * tract.prop, 1)
df <- data.frame(df, houses, int.areas)
# cross tabulate the number of houses and the int.layer id
int.layer.houses <- xtabs(df$houses ~ df$intlayer.id)
index <- as.numeric(gsub("g", "", names(int.layer.houses))) # renaming the ids to be numerics
tmp <- vector("numeric", length = dim(data.frame(int.layer))[1])
tmp[index] <- int.layer.houses
i.houses <- tmp 
# now we create a new data frame using the int.layer and the counts of houses in each grid of the layer
int.layer <- SpatialPolygonsDataFrame(int.layer, data = data.frame(data.frame(int.layer), i.houses), match.ID = FALSE)
# plot
par(mfrow = c(1,1))
shades = auto.shading(int.layer$i.houses, n = 6, cols = brewer.pal(6, "Greens"))
choropleth(int.layer, int.layer$i.houses, shades)
plot(tracts, add = TRUE)
choro.legend(530000, 159115, bg = "white", shades, title = "No. of houses", under = "")

# Converting between Raster and Vector:
library(GISTools)
library(raster)
data(tornados)
# Converting from Points to a Raster object:
# Create a raster the size of the US
r = raster(nrow = 180, ncols = 360, ext = extent(us_states2))
t2 <- as(torn2, "SpatialPoints")
# Map the SP data set from its vector locations onto the raster, summing over total counts in each spot
r <- rasterize(t2, r, fun = sum)
plot(r, col = "white")
plot(us_states2, add = T, border = "gray")
plot(r, add = T)
# Converting from Lines to a Raster object:
us_outline <- as(us_states2, "SpatialLinesDataFrame")
r <- raster(nrow = 180, ncols = 360, ext = extent(us_states2))
r <- rasterize(us_outline, r, "STATE_FIPS")
plot(r)
# Converting Polygons or Areas to a Raster object:
r <- raster(nrow = 180, ncols = 360, ext = extent(us_states2))
r <- rasterize(us_states2, r, "POP1997")
plot(r)


# Raster analysis:
# Raster analysis requires that the different data sets have some characteristics in common:
# spatial extent, spatial resolution, projection/coord system
library(GISTools)
library(raster)
library(sp)
data("meuse.grid")
coordinates(meuse.grid) <- ~x+y
meuse.grid <- as(meuse.grid, "SpatialPixelsDataFrame")
# create 3 raster layers
r1 <- raster(meuse.grid, layer = 3) # dist
r2 <- raster(meuse.grid, layer = 4) # so
r3 <- raster(meuse.grid, layer = 5) # ffreq
image(r3, asp = 1)
# Reclassification
# Raster data are numerical, so we can perform operations like this:
Raster_result <- r2 + (r3*10)
spplot(Raster_result, col.regions = brewer.pal(9, "Spectral"), cuts = 8)
