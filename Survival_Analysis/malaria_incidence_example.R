##############################
# Demo for how to calculate the relative risk based on incidence
# Use the output data from:
# Stored at: "/Users/dtcitron/Documents/R_Tutorials/Survival_Analysis/malaria_incidence_example_output"
##############################

library(data.table)
library(ggplot2)
library(survival)
library(prodlim)

source("/Users/dtcitron/Documents/MASH/MACRO-multipatch-test/Multipatch_data_transform.R")

directory = "/Users/dtcitron/Documents/R_Tutorials/Survival_Analysis/malaria_incidence_example_output"
human.pathogen.path <- paste0(directory, "/HumanPathogen_Run0.csv")
human.move.path <- paste0(directory, "/HumanMove_Run0.csv")
tMax = 850
patch_humans = c(499, 496, 0, 0, 0, 0, 0, 0, 0)

# This is a data set with humanID, time, event, vectorID
human.pathogen <- fread(human.pathogen.path)
# This is a data set with humanID, time, event, location
human.move <- fread(human.move.path)
# Rename "events" as "travel events"
setnames(human.move, "event", "travelEvent")

# Bind these two together
tmp1 = rbind(human.move, human.pathogen, fill=T)
# Sort by time
tmp1 = tmp1[order(humanID, time)]

# ESTABLISH INITIAL LOCATIONS
patch_id = rep(x = 1:length(patch_humans), patch_humans)
initLoc = data.table(humanID=unique(tmp1$humanID), home_id = patch_id)

# PULL OUT THE FIRST INFECTION TIME FOR EACH PERSON
setkey(tmp1, "humanID")
tmp2 <- tmp1[event == "I" & time > 90][, .(humanID, time)]
tmp2 <- tmp2[, .SD[1], by = c("humanID")]
tmp2$include <- 1

# COMBINE, AND HIGHLIGHT CENSORED VS UNCENSORED DATA
tmp3 <- merge(initLoc, tmp2, by = "humanID", all = T)
tmp3[is.na(include)]$include <- 0 # Fill in NAs in the include column
setnames(tmp3, c("time"), c("time_Infect"))
tmp3[is.na(time_Infect)]$time_Infect <- 851 # Fill in the NAs for the time of infection column

# NOW ADD WHETHER OR NOT THINGS WERE VACCINATED
tmp4 <- merge(tmp3, tmp1[event == "PEvaxx"][, .(humanID, event, time)], by = "humanID", all = T)
tmp4$PEvaxx <- FALSE
tmp4[event == "PEvaxx"]$PEvaxx <- TRUE
setnames(tmp4, c("time"), c("time_vaxx"))

# NOW ADD START TIMES
# In this case we have everyone starting at time = 120
tmp4$time_Start <- 120



# Now we do the CoxPH regression
# need to readjust the definition of the Censored column to be 0 if nothing happened and 1 if something did happen
survmod.cox_unadj <- coxph(Surv(time_Start, time_Infect, include) ~ PEvaxx, data=tmp4, method ="efron")
summary(survmod.cox_unadj)

# And plot the Kaplan-Meier curves
km0 <- prodlim(Hist(time_Infect, include) ~ PEvaxx, data = tmp4)
plot(km0)


# Compare this to a parametric model, exponential distribution
survreg.exp_unadj <- survreg(Surv(time_Infect - time_Start, as.numeric(!include)) ~ PEvaxx, data=tmp4, dist = "exponential")
summary(survreg.exp_unadj)
# Value for PEvaxx means that this is the hazard ratio comparing True to False
# In this case, we have that vaccinated people
# http://rstudio-pubs-static.s3.amazonaws.com/5564_bc9e2d9a458c4660aa82882df90b7a6b.html

# Compare this to a parametric model, weibull distribution
survreg.wei_unadj <- survreg(Surv(time_Infect - time_Start, include) ~ PEvaxx, data=tmp4, dist = "weibull")
summary(survreg.wei_unadj)

# How on earth do I interpret this stuff?
# The intercept value is the baseline hazard, assuming an exponential model
# The Value associated with PEvaxx being true is the extra hazard rate associated with being part of the vaccinated group
#

# Some fake data?
# For 500 people, let's generate an exponentially distributed set of data with decay rate 1
# For another 500 people, let's generate another set with decay rate 2
#

test <- data.table(humanId = c(1:1000), time = 0, PEvaxx = TRUE) 
test[humanId <= 500]$time <- rexp(500, 1) # Only vaccinate the first 500 people
test[humanId > 500]$time <- rexp(500, 2) # These people have a FASTER rate of event occurrence
test[humanId > 500]$PEvaxx <- FALSE
test$include = TRUE

test.coxph <- coxph(Surv(time, include) ~ PEvaxx, data=test, method ="efron")
summary(test.coxph)
# PEvaxxTRUE value = 0.48
# This is because we expect, if we vaccinate people, the rate of occurrence is .5 times the rate for the non-vax population

test.exp <- survreg(Surv(time, include) ~ PEvaxx, data = test, dist = "exponential")
summary(test.exp)
# These results are bonkers and I don't understand them.