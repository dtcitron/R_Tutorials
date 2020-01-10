# I am going to use this for my tutorial through ggplot2 from Hadley Wickham's book
library(tidyverse)

# First step: answer a question of whether cars with big engines use more fuel?

# miles-per-gallon data frame
# create a simple scatter plot:
# displ - car engine size in litres; hwy = car's fuel efficiency in mpg
# geom_point defines the shape of plotted points
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy))

# next make a scatter plot comparing hwy vs. cyl (number of cylinders)
ggplot(data = mpg) + geom_point(mapping = aes(x = cyl, y = hwy))

# next, let's convey some additional information in the first scatter plot
# by coloring each different point according to its class:
# remember, even though the members of "class" in mpg are character-type, R will
# coerce those data to fit to a color scheme - "scaling" - fits a unique color to each value of a variable
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, color = class))

# or change each point's size according to class:
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, size = class))

# aes stands for "aesthetic" - defines the look of the graph

# the "stroke" element changes the thickness of the lines used to draw the points in geom_point
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, color = class, stroke = 5), shape = 0)

# can also map color using a function like this:
# in this case, draws all engines below 20 mpg as one color and the rest another color!
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, color = hwy < 20))

# And here's a kludge-y way of mapping color onto multiple categories
cmap <- function(x) {
  if (x <= 20)
    return(1)
  else if (x > 20 & x <= 30)
    return(2)
  else if (x > 30 & x <= 40)
    return(3)
  else if (x > 40)
    return(4)
  else
    return(0)
}
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, color = sapply(hwy, cmap)*10))
#(can also accomplish something similar using a continuous mapping like this: round(mpg$hwy/10))))


# Facets: display more variables by creating subplot panels that display the data
# the facet_wrap() function - quick way of separating out subportions of data into subplots
# note that the first argument in facet_wrap() is a function, in this case separating out by class
ggplot(data = mpg) + geom_point(mapping=aes(x=displ,y=hwy)) + facet_wrap(~class, nrow = 2)
# we can also try separating the data out according to two variable names:
# drv (four-wheel vs 2-wheel) or cyl (number of cylinders)
ggplot(data = mpg) + geom_point(mapping=aes(x=displ,y=hwy)) + facet_grid(drv~cyl)

# Plotting Continuous Data
ggplot(data = mpg) + geom_smooth(mapping = aes(x = displ, y = hwy))

# Plotting Continuous Data, but this time with a different line type depending on the drv type:
ggplot(data = mpg) + geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv, color = drv))
ggplot(data = mpg) + geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv, color = drv))

# Plotting and re-plotting the same data, but without having to repeat lots of text by making the mapping universal
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + geom_smooth() + geom_point()

# ie, note the equivalency of the following two lines:
ggplot() + geom_point(data = mpg, mapping = aes(x = displ, y=hwy)) + geom_smooth(data = mpg, mapping = aes(x = displ, y=hwy))
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + geom_point() + geom_smooth()


# Can always override the global mapping locally by specifying a new one:
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + geom_point(mapping = aes(color = class)) +
  geom_smooth(data = filter(mpg, class == 'subcompact'), se = FALSE)

# We can "jitter" our data in order to adjust overplotting of redundant data points
# This adds a small amount of random noise to each data point
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy), position = "jitter")

# here's another example of a plot with too much overplotting, that can be fixed using jitter
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + geom_point()
# adding jitter
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + geom_point() + geom_jitter()
# controlling the size of the jitter noise
# the number is a fraction of the bin size in the horizontal/vertical direction
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + geom_point() + geom_jitter(height = 1., width = 0)

# superpose the jittered data over non-jittered data
ggplot(data = mpg) + geom_point(mapping = aes(x = cyl, y = hwy)) +
  geom_jitter(mapping = aes(x = cyl, y = hwy), color = 10, height = 0, width = 1)


# Plotting map data
nz <- map_data("nz")
ggplot(nz, aes(long, lat, group = group)) + geom_polygon(fill = "white", color = "black")
# and fixing the axes to have the correct aspect ratio:
ggplot(nz, aes(long, lat, group = group)) + geom_polygon(fill = "white", color = "black") + coord_quickmap()

# The Layered Grammar of Graphics
# 1. Start with a data set (eg, diamonds)
# 2. Compute quantitative aspect of the data set that will be represented (eg. stat_count)
# 3. Choose an aesthetic representation (eg, bar plot)
# 4. Map values of variables onto levels of an aesthetic
# 5. Place everything on coordinate axes
# 6. Split into subplots (faceting)
# 7. Position adjustment (position)

# Data transformation with dplyr
library(tidyverse)
library(nycflights13)
# using the flights data set
# tibbles are data frames, tweaked to work better in the tidyverse

# 1. Filter rows using filter() - subset observations based on their values
# syntax: first give the data set, then give a list of filters
filter(flights, month == 1, day == 1) # all flights on 1/1
# passing logical arguments to filter()
filter(flights, month == 1 | 3, day == 1) # all flights on 1/1 or 3/1
# alternatively
filter(flights, month %in% c(1,3), day == 1)
# filtering only returns rows that are TRUE, not rows that have no data (NA)
# Finding flights with an arrival delay of more than 2 hours
filter(flights,arr_delay >= 2)
# Finding flights that arrived in Houston (HOU, IAH)
filter(flights, dest=="HOU" | dest=="IAH")
# Finding flights that arrived more than 2 hours late, but left on time
filter(flights, dep_delay ==0 & arr_delay > 2)

# 2. Arrange rows with arrange()
# order rows by year, month, day (in that order)
arrange(flights, year, month, day)
# descending from most to least, order rows by arrival time delay
arrange(flights, desc(arr_delay))

# 3. Select columns with select()
select(flights, year, month, day)
# Select all columns between year and day
select(flights, year:day)
# Select all columns except those between year and day:
select(flights, -(year:day))
# Select all columns starting with "arr":
select(flights, starts_with('arr'))

# 4. Add and create new variables using mutate()
# adds new columns to the data set at the end
flights_sml <- select(flights, year:day, ends_with('delay'), distance, air_time)
# adds two new columns: the added delay time, and the speed
mutate(flights_sml, gain = arr_delay - dep_delay, speed = distance/air_time * 60)
# To keep only the new variables, use transmute()
transmute(flights, gain = arr_delay - dep_delay, hours = air_time/60, gain_per_hour = gain / hours)

# 5. Grouped summaries using summarize()
summarize(flights, delay = mean(dep_delay, na.rm = TRUE))
# More useful when we use group_by()
# in this case, we are grouping all flights together according to each day...
by_day <- group_by(flights, year, month, day)

# Combining multiple operations together using the Pipe
# Explore relationshp between distance and average delay for each location:
# grouping together flights by destination
by_dest <- group_by(flights, dest)
delay <- summarize(by_dest,
  count = n(),
  dist = mean(distance, na.rm = TRUE),
  delay = mean(arr_delay, na.rm = TRUE)
  )
# filter on cases where we have more than 20 flights in the data set, and the destination is not Honolulu
delay <- filter(delay, count > 20, dest != "HNL")
# Plot
ggplot(data = delay, mapping = aes(x = dist, y = delay)) + geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE)
# Where does the pipe come in?  We can do these three steps in one!
# This makes it much easier to parse: I don't have to be creating lots of new data sets or columns
# We are seamlessly passing data through the pipe
delays <- flights %>%
  group_by(dest) %>%
  summarize(
    count = n(),
    dist = mean(distance, na.rm = TRUE),
    delay = mean(arr_delay, na.rm = TRUE)
  ) %>%
  filter(count > 20, dest != "HNL")

# how does the counting function work?
not_cancelled <-  flights %>%
  filter(!is.na(dep_delay), !is.na(arr_delay))
delays <- not_cancelled %>%
  group_by(tailnum) %>%
  summarize(
    delay = mean(arr_delay)
  )
# create a histogram of delays associated with flights that are not canceled
ggplot(data=delays, mapping = aes(x = delay)) + geom_freqpoly(binwidth = 10)
# Look for nuance by creating a scatter plot instead
delays <- not_cancelled %>%
  group_by(tailnum) %>% # grouping according to the plane's tail number
  summarize(
    delay = mean(arr_delay, na.rm = TRUE),
    n = n()
  )
# this creates a scatter plot of the delay vs. group size
# we can see that for small group sizes (ie, few flights), the variation is much larger
ggplot(data = delays, mapping = aes(x = n, y = delay)) +
  geom_point(alpha = 1/10)
# now let's filter out the cases with the smallest numbers of flights
delays %>%
  filter(n > 25) %>%
  ggplot(mapping = aes(x = n, y = delay)) + geom_point(alpha = 1/10)



# Exploratory Data Analysis
library(tidyverse)
# Histogram count data for types of cut (categorical variable data)
diamonds %>% count(cut)
# and plotting:
ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut))
# Histogram count data for carat size (continuous variable data)
ggplot(data = diamonds) + geom_bar(mapping = aes(x = carat), binwidth = .5)
# histogram count data, binning with a "cut width" of .5:
diamonds %>% count(cut_width(carat, .5))
# and what happens when we filter on small diamonds, under 3 carats?
smaller <- diamonds %>% filter(carat < 3)
ggplot(data = smaller, mapping = aes(x = carat)) + geom_histogram(binwidth = 0.1)
# to overlay multiple histograms on top of the same plot, it's easiest with geom_freqpoly():
ggplot(data = smaller, mapping = aes(x=carat, color = cut)) + geom_freqpoly(binwidth = 0.1)

# Unusual values:
# plot a histogram of diamond widths (sizes, y)
ggplot(diamonds) + geom_histogram(mapping = aes(x = y), binwidth = 0.5)
# The above plot has a set of bins that are too wide: this is because of outliers with very small counts
# zooming in on a portion of the y axis by re-defining the coordinates shows this
ggplot(diamonds) + geom_histogram(mapping = aes(x = y), binwidth = 0.5) + coord_cartesian(ylim = c(0,50))
# let's take out the outliers in width (which may actually be errors in the data set)
unusual <-diamonds %>% filter(y < 3 | y > 20) %>% arrange(y)
unusual

# Covariation - describing behavior between variables
# Comparing aross cut quality, let's show the normalized histograms of price:
ggplot(data=diamonds, mapping = aes(x = price, y = ..density..)) +
  geom_freqpoly(mapping = aes(color = cut), binwidth = 500)
# Weird: fair diamonds appear to have the highest mean price
# We can check the visual intution by finding the mean price, summarizing by cut quality
diamonds %>%
  group_by(cut) %>%
  summarize(
    n = n(),
    mp = mean(price, na.rm=TRUE),
    medp = median(price, na.rm=TRUE)
  )
# Or even compare the full distribution of prices, grouped by cut, using a series of box plots:
ggplot(data = diamonds, mapping = aes(x = cut, y = price)) +
  geom_boxplot()
# From this, it is a little easier to see that better quality diamonds are cheaper on average
# Why would this be?
# The strongest predictor for price of a diamond is its weight, not its cut
# When we group by cut, we are ignoring the fact that the mean weight of Ideal diamonds is much smaller
# than the mean weight of Fair diamonds:
diamonds %>%
  group_by(cut) %>%
  summarize(
    n = n(),
    mp = mean(price, na.rm=TRUE),
    medp = median(price, na.rm=TRUE),
    mw = mean(carat, na.rm = TRUE),
    medw = median(carat, na.rm = TRUE)
  )
# We can also visualize this using a scatter plot:
ggplot(data = diamonds) + geom_point(mapping = aes(x = carat, y = price), alpha = .1)
# This data has extra structure in it, and is a little hard to parse
# We can also bin the data, showing a histogram in each bin:
ggplot(data = diamonds) + geom_bin2d(mapping = aes(x = carat, y = price))
# We also can bin the data and summarize it using a series of box plots for each bin:
ggplot(data = diamonds, mapping = aes(x = carat, y = price)) +
  geom_boxplot(mapping = aes(group = cut_width(carat, .1)))
# We can account for the variation in the number of points across each bin by instead binning by count number
ggplot(data = diamonds, mapping = aes(x = carat, y = price)) +
  geom_boxplot(mapping = aes(group = cut_number(carat, 20)))


# Covariation between two categorical variables
# Need to count the number of observations for each combination:
ggplot(data = diamonds) + geom_count(mapping = aes(x = cut, y = color))
# we can also do this in a less painful-looking way using color/heatmap:
diamonds %>%
  count(color, cut) %>%
  ggplot(mapping = aes(x = color, y = cut)) + geom_tile(mapping = aes(fill = n))



# ggplot2 tutorial from R in a Nutshell
d <-data.frame(a = c(0:9), b = c(1:10), c = c(rep(c("Odd","Even"), times = 5)))
d
# shown ow how variable b changes with variable a:
library(ggplot2)
qplot(x=a, y=b, data=d)
# alternatively
ggplot(data = d) + geom_point(mapping = aes(x = a, y = b))

# can store this plot by assigning it to a variable:
first.ggplot2.example <- qplot(x=a, y=b, data=d)
# summarize
summary(first.ggplot2.example)

# display facets of the data (cut on odd/even, column "c")
qplot(x = a, y = b, data = d, facets = c~.)
ggplot(data = d) + geom_point(mapping = aes(x = a, y = b)) + facet_wrap(~c, nrow = 2)
# or we can re-arrange the layout of the facets:
qplot(x = a, y = b, data = d, facets = .~c)
ggplot(data = d) + geom_point(mapping = aes(x = a, y = b)) + facet_grid(.~c)
# or we can color our data using c:
qplot(x = a, y = b, data = d, color =c)
ggplot(data = d) + geom_point(mapping = aes(x = a, y = b, color = c))

# plotting 1d data:
set.seed(12345)
e <- data.frame(f = rnorm(1000))
str(e)
#histogram:
qplot(x = f, data = e)
ggplot(data = e) + geom_histogram(mapping = aes(x = e))
# normalized histogram:
qplot(x = f, data = e, geom = "density")
ggplot(data = e) + geom_histogram(mapping = aes(x = e, y = ..density..))
# summarize the above histogram, to show its attributes:
thehistogram <- qplot(x = f, data = e, geom = "density")
summary(thehistogram)
# This goes back to the grammar of graphics:
# 1. Data - variable x, with 1000 values
# 2. Mappings - The 'x' value in the plot is assigned to the variable x in the data frame
# 3. Geometric objects (geom) - geom_density, a smooth density plot
# 4. Aesthetic properties (none written)
# 5. Statistical stransformations - stat_density, using density function to summarize the data

# A more complex example: Medicare Data Set
library(nutshell)
# average mortality and readmission rates for three common medical conditions
# (Heart Attack, Heart Failure, Pneumonia)
# one factor variable Measure
# two values: Mortality, Readmission
data("outcome.of.care.measures.national")
summary(outcome.of.care.measures.national)
outcome.of.care.measures.national
# how do the mortality/readmission rates differ for each condition?
bar.chart.example <- qplot(
  x = Condition,
  data = outcome.of.care.measures.national,
  geom = "bar", weight = Rate, facets = Measure~., fill = Measure)
print(bar.chart.example)
# Alternatively, can use dodge to juxtapose the mortality/readmission rates
ggplot(data = outcome.of.care.measures.national) +
  geom_bar(mapping = aes(x = Condition, weight = Rate, fill = Measure), position = "dodge")

# How does the number of patients treated by a hospital relate to the fees charged to Medicare?
# Would large hospitals charge less because patience experienced fewer complications, or
#  would large hospitals charge more because they were better at gaming the system?
# First: cut the data to only look at heart failure:
heart.failure <- c("Heart failure and shock w/o CC/MCC",
                   "Heart failure and shock w MCC",
                   "Heart failure and shock w CC")
data("medicare.payments")
data = subset(medicare.payments, Diagnosis.Related.Group %in% heart.failure)
# Set x = number of cases, y = average payment
payment.plot <- qplot(x = Number.Of.Cases, y = Medicare.Average.Payment,
                      # filter on a subset of data only in the relevant set of heart failure diagnoses
                      data = subset(medicare.payments, Diagnosis.Related.Group %in% heart.failure),
                      color = Diagnosis.Related.Group
                      )
payment.plot
# To improve the legibility of this plot,
# Transform x (number of cases) to a log scale, make data transparent using alpha,
# add some smoothing lines, hide outliers by adjusting limits:
heart.failure.cost.plot <-
  qplot(x = log(Number.Of.Cases), y = Medicare.Average.Payment,
        data = subset(medicare.payments, Diagnosis.Related.Group %in% heart.failure),
        color = Diagnosis.Related.Group, ylim = c(0, 20000),
        alpha = I(1/10),
        geom = c("point", "smooth")
  )
heart.failure.cost.plot
# More elegantly, we can recreate the plot and then add in adjustments to alpha and y limits:
payment.plot.alpha <- qplot(
  x = Number.Of.Cases,
  y = Medicare.Average.Payment,
  data = subset(medicare.payments, Diagnosis.Related.Group %in% heart.failure),
  color = Diagnosis.Related.Group,
  alpha = I(1/10), ylim = c(0,20000)
)
# Add smoothing lines and change scale:
payment.plot.scaled <- payment.plot.alpha + scale_x_log10() + geom_smooth()
payment.plot.scaled
summary(payment.plot.scaled)
# Why do costs tend to increase as the number of cases increases?  Could geography contribute to this?
data("medicare.payments.by.state")
medicare.payments.by.state.hf <- subset(medicare.payments.by.state, Diagnosis.Related.Group %in% heart.failure)
# mean cost for each state, ordering states by average payment
medicare.payments.by.state.hf$State <- with(medicare.payments.by.state.hf,
                                            reorder(State, Medicare.Average.Payment.Maximum, mean))
# Now create a dot plot for each state:
payment.dotplot <- qplot(
  x = Medicare.Average.Payment.Maximum,
  y = State,
  data = medicare.payments.by.state.hf,
  color = Diagnosis.Related.Group)
payment.dotplot

# We can also visualize the state-by-state results
library(maps)
states <- map_data("state")
library(datasets)
state.name.map <- data.frame(abb=state.abb, region = tolower(state.name), stringsAsFactors = FALSE)
states <- merge(states, state.name.map, by = "region")
# merge geography data with numerical data
toplot <- merge(states, medicare.payments.by.state, by.x ="abb", by.y = "State")
qplot(long, lat,
      data=subset(toplot, Diagnosis.Related.Group=="Heart failure and shock w/o CC/MCC"),
      group = group,
      fill=Medicare.Average.Payment.Maximum, geom="polygon")
