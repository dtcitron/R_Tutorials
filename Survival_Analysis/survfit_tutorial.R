# This tutorial is taken from http://staff.pubhealth.ku.dk/~tag/Teaching/share/R-tutorials/SurvivalAnalysis.html
# How to obtain the Kaplan-Meier graph (estimate of survival curves, based on time series data)
# How to fit a univariate and multiple Cox PH regression model
#

# Load data
# German breast cancer data
library(prodlim) # Functions for estimating probabilities of right censored data
library(survival)
library(pec)
library(Publish) # This one is for creating publishable tables

data(GBSG2, package = "pec")
setDT(GBSG2)
View(GBSG2)

# Make sure tumor grade is a factor
GBSG2$tgrade <- as.factor(as.character(GBSG2$tgrade))
# Categorize age into 4 categories
GBSG2$Age <- cut(GBSG2$age,c(0,40,60,81),labels=c("21-40","41-60","61-80"))
GBSG2$Age <- cut(GBSG2$age,c(-Inf,40,50,65,Inf),labels=c("<40","40-50","51-65",">65"))

# Median follow-up time - how much time until half of the people have been observed a second time with a follow-up
# Calculate reverse Kaplan-Meier estimate to obtain median follow-up with 95% CI and IQR
quantile(prodlim(Hist(time,cens)~1,data=GBSG2,reverse=TRUE))


# Kaplan-Meier median estimation:
km0 <- prodlim(Hist(time, cens) ~ 1, data = GBSG2)
quantile(km0)
# Kaplan-Meier graph
plot(km0)
# and making it more beautiful:
par(mar=c(7,7,5,5), # margin of figure
    mgp=c(4,1,0))   # move axis label away from figure
plot(km0,
     xlab="Years",  # label for x-axis
     axis1.at=seq(0,2900,365.25), # time grid for x-axis
     axis1.labels=0:7, # time labels for x-axis
     axis2.las=2, # rotate labels of y-axis
     atrisk.dist=1, # adjust numbers below the figure
     atrisk.labels="Number of \npatients") # labels for numbers below figure


# Stratified Kaplan-Meier  - stratify based on the tumor grade
km1 <- prodlim(Hist(time, cens) ~ tgrade, data = GBSG2)
quantile(km1) # appears that the median follow-up time is much sooner for grade-III tumors than for grade-I tumors
# Stratified Kaplan-Meier graph
plot(km1,
     atrisk.labels=paste("Tumor grade: ",c("I","II","III"),": "),
     atrisk.title="",
     xlab="Years",  # label for x-axis
     axis1.at=seq(0,2900,365.25), # time grid for x-axis
     axis1.labels=0:7, # time labels for x-axis
     legend.x="bottomleft", # positition of legend
     legend.cex=0.8, # font size of legend
     legend.title="Tumor Grade\n", #
     logrank=TRUE) # show log-rank p-value


# Tabulated results
publish(km0,times=seq(0,2900,365.25),org=TRUE)


# Cox Regression
cox1 <- coxph(Surv(time,cens)~tgrade,data=GBSG2)
summary(cox1)
# Plot first the KM estimator (solid) and the Cox regression (dashed)
km.grade <- prodlim(Surv(time,cens)~tgrade,data=GBSG2)
cox.grade <- coxph(Surv(time,cens)~tgrade,data=GBSG2,x=TRUE,y=TRUE)
newdata <- data.frame(tgrade=c("I","II","III"))
## first show Kaplan-Meier without confidence limits
plot(km.grade, lty=1, lwd=3,
     col=c("darkgreen","darkorange","red"), confint=FALSE)
## now add survival estimates based on Cox regression
plotPredictSurvProb(cox.grade, lty=2,
                    col=c("darkgreen","darkorange","red"),
                    add=TRUE, sort(unique(GBSG2$time)),
                    newdata=newdata)
mtext("Comparison of univariate  Cox regression and stratified Kaplan-Meier")


# Another Cox PH analysis, this time including some additional data
cox2 <- coxph(Surv(time,cens)~tgrade+age+tsize+pnodes,data=GBSG2, x=TRUE)
summary(cox2)
# cut data to specify small group of individuals
newdata <- data.frame(tgrade=c("I","II","III"),age=50,tsize=30,pnodes=8)
## first show Kaplan-Meier without confidence limits
plot(km.grade, lty=1, lwd=3,
     col=c("darkgreen","darkorange","red"), confint=FALSE)
# now add survival estimates
plotPredictSurvProb(cox2, lty=2,
                    sort(unique(GBSG2$time)),
                    newdata=newdata,
                    col=c("darkgreen","darkorange","red"),
                    legend.title="Tumor grade",
                    legend.legend=c("I","II","III"))
mtext("Individualized survival curves from multiple Cox regression
age=50, tumor size= 30, no. positive lymph nodes=8",line=1.5,cex=1.3)
