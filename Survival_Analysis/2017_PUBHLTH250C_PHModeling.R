##################################################################################################
##Framingham Analysis
##created by Emon Elboudwarej
##January 21, 2016

##edited by Patrick Bradshaw
##January 21, 2017
##################################################################################################

#################################################
# STEP 1
# SET WORKING DIRECTORY (where you have the dataset)
#################################################
setwd("/Users/dtcitron/Documents/R Tutorials/Survival Analysis")
#################################################

#################################################
# STEP 2
# PACKAGES
#################################################
## If you don't already have these installed:
# install.packages("survival")
# install.packages("survMisc")
# install.packages("simPH")
# install.packages("multcomp")
# install.packages("doBy")
# install.packages("splitstackshape")

require(survival)
require(foreign)
require(survMisc)
require(simPH)
require(multcomp)
#################################################

#################################################
# STEP 3
# READ DATA
#################################################

#READ IN RECODED DATASET
load("frmgham_recoded.Rdata")

###########CONTINUOUS TIME SURVIVAL##############
#CREATE A SINGLE-RECORD DATASET (pull 1st observation from each individual)
frmgham_recoded <- frmgham_recoded[which(frmgham_recoded$period == 1),]

frmgham_recoded$bmi_cat <- relevel(as.factor(frmgham_recoded$bmi_cat),2)

# Unadjusted Cox Proportional Hazards model for BMI-mortality:
# syntax for Surv:
#    time_yrs - start time (for this data set, everyone starts at the same time)
#    timedth_yrs - end time (death)
#    death - status indicator of the event - whether or not the event occurred -
#            if death = 0, then we have right-censored data - no event occurred (so timedth_yrs = 24, max study duration)
# syntax for the coxph regression: Surv ~ bmi_cat, ie modeling survival time vs. BMI category as a covariate
# efron is a method for handling tied event occurrences
survmod.cox_unadj <- coxph(Surv(time_yrs,timedth_yrs, death)
                           ~ as.factor(bmi_cat), data=frmgham_recoded,
                           method="efron")
summary(survmod.cox_unadj)
# When interpreting the output, we find exp(coef) as the hazard ratio for each bmi_cat vs. bmi_cat = 2, or normal weight
# In this case, for example, those with bmi_cat = 4 expect death to occur at rate 1.685


# Fully-adjusted Cox Proportional Hazards model for BMI-mortality:
# Adjusting for sex and age
survmod.cox_adj <- coxph(Surv(time_yrs,timedth_yrs, death)
                           ~ as.factor(bmi_cat) + age + male,
                         data=frmgham_recoded, method="efron")
summary(survmod.cox_adj)


# Easiest to use survfit in R:
# Limiting to normal weight and obese so easy to read:
sf.cox_unadj <- survfit(Surv(time_yrs,timedth_yrs, death) ~ as.factor(bmi_cat),
                        data=frmgham_recoded, subset= bmi_cat==2 | bmi_cat==4)

# Graphical assessment of PH assumption: log-log Survival plot:
# ie, can we assume the baseline hazards to be constant?
# Plot estimate of log(-log(S(t))) vs log(t)
# Proportional Hazards assumption should imply that these lines, for different groups, should
# be close to parallel (only applicable for categorical covariates)
pdf("log-logSurvivalCurveBMI.pdf") # Saves to PDF file (don't run this to draw on screen)
plot(sf.cox_unadj, main="log(-log(S(t))) by Baseline BMI",
     xlab="Time", ylab="log(-log(S(t)))",
     fun=cloglog<-function(x) log(-log(x)), log="x",
     bty="l", col=c("darkblue","darkorange"))
legend("bottomright",legend=c("BMI 18.5-24.9","BMI >=30.0"),
       col=c("darkblue","darkorange"),
       cex=0.8, lty=1, lwd=2, bty="n")
dev.off()
# Interpretation:
#  Not parallel, but hard to tell how strong the deviation is
#  Red flags: curves that start out, then converge, then diverge
#  Possible next step: covariate adjustment?


# Time-covariate interaction:
# Analytical Assessment of the PH assumption
survmod.cox_ixn <- coxph(Surv(time_yrs,timedth_yrs, death) ~
                              underwt + overwt + obese + age + male +
                              tt(underwt) + tt(overwt)+ tt(obese)+ tt(age) +tt(male),
                         data=frmgham_recoded, method="efron",
                         tt=function(x,t,...) x*t)
summary(survmod.cox_ixn)

# Stratified Cox PH model with time interactions:
# Pretend you had PH violation of the sex variable:
survmod.cox_ixn_male <- coxph(Surv(time_yrs,timedth_yrs, death) ~
                underwt + overwt + obese + age + strata(male) +
                tt(underwt) + tt(overwt)+ tt(obese),
                data=frmgham_recoded, method="efron",
                tt=function(x,t,...) x*t)
summary(survmod.cox_ixn_male)

# Time-specific effets
# Create vector to indicate linear contrast:
# Obese at t=5 (main effect + 5*obese(tt))

k0 <- matrix(c(0,0,1,0,0,0,0),1)
k2 <- matrix(c(0,0,1,0,0,0,2),1)
k5 <- matrix(c(0,0,1,0,0,0,5),1)
k10 <- matrix(c(0,0,1,0,0,0,10),1)

# Association of obese (vs. normal weight) at t=0
exp(confint(glht(survmod.cox_ixn_male,linfct=k0))$confint)[,1:3]

# Association of obese (vs. normal weight) at t=2
exp(confint(glht(survmod.cox_ixn_male,linfct=k2))$confint)[,1:3]

# Association of obese (vs. normal weight) at t=5
exp(confint(glht(survmod.cox_ixn_male,linfct=k5))$confint)[,1:3]

# Association of obese (vs. normal weight) at t=10
exp(confint(glht(survmod.cox_ixn_male,linfct=k10))$confint)[,1:3]


## Effect within regions of time: >/< 5 years:
survmod.cox_ixn_t5_male <- coxph(Surv(time_yrs,timedth_yrs, death) ~
          underwt + overwt + obese + age + strata(male)
          + tt(underwt) + tt(overwt)+ tt(obese),
     data=frmgham_recoded, method="efron",
     tt=function(x,t,...) x*as.integer(t>5))
summary(survmod.cox_ixn_t5_male)

# Obese before 5 years (only main effect)
k.before5 <- matrix(c(0,0,1,0,0,0,0),1)
# Obese 5 years and after (main effect + 1*obese(tt))
k.after5 <- matrix(c(0,0,1,0,0,0,1),1)

# Effect of obese before 5 years
exp(confint(glht(survmod.cox_ixn_t5_male,linfct=k.before5))$confint)[,1:3]

# Effect of obese after 5 years
exp(confint(glht(survmod.cox_ixn_t5_male,linfct=k.after5))$confint)[,1:3]

###### Time Varying Covariate (BMI)
require(doBy)

# Reload dataset
load("frmgham_recoded.Rdata")

#CREATE AN INTERVAL PERIOD VARIABLE AND TOTAL NUMBER OF PERIODS VARIABLE
frmgham_recoded$n <- frmgham_recoded$period

tot.obs.periods <-  setNames(as.data.frame(frmgham_recoded[!duplicated(frmgham_recoded$randid, fromLast=T),c("randid","period")]),c("randid","N"))
frmgham_recoded <- merge(frmgham_recoded,tot.obs.periods, by='randid')

#EACH PERIOD ENTRY IS THE TIME OF EXAM
frmgham_recoded$enter <- frmgham_recoded$time

#EXIT IS EXAM TIME FROM NEXT PERIOD #FINAL TIME IS LAST FOLLOW-UP
#CREATE A NEW DEATH INDICATOR (DEATH2) AND MAKE ALL DEATH INDICATORS BEFORE LAST ONE 0
for(i in 1:nrow(frmgham_recoded)){
     if(frmgham_recoded[i,"n"] < frmgham_recoded[i,"N"]){
          frmgham_recoded[i,"exit"] <- frmgham_recoded[i+1,"time"]
          frmgham_recoded[i,"death2"] <- 0
     }
     else if(frmgham_recoded[i,"n"] == frmgham_recoded[i,"N"]){
          frmgham_recoded[i,"exit"] <- frmgham_recoded[i,"timedth"]
          frmgham_recoded[i,"death2"] <- frmgham_recoded[i,"death"]
     }
}

#CREATING A BASELINE BMI VARIABLE
frmgham_recoded <- merge(frmgham_recoded,
                         setNames(frmgham_recoded[which(frmgham_recoded$period == 1),c("randid","bmi_cat")],c("randid","bmi0_cat")),
                         by='randid')

# Check out a few observations
frmgham_recoded[frmgham_recoded$randid %in% c(6238, 10552, 11252),
                c("randid","enter","exit","death2","bmi","age","sex")]

# BASELINE BMI MODEL WITH MULTIPLE RECORD (ENTER/EXIT) FORMAT
frmgham_recoded$bmi0_cat <- relevel(as.factor(frmgham_recoded$bmi0_cat),2)
survmod.cox_unadj2 <- coxph(Surv(enter,exit, death2) ~ as.factor(bmi0_cat),
                            data=frmgham_recoded, method="efron")
summary(survmod.cox_unadj2)

# TIME-UPDATED BMI MODEL WITH MULTIPLE RECORD FORMAT
frmgham_recoded$bmi_cat <- relevel(as.factor(frmgham_recoded$bmi_cat),2)
survmod.cox_unadj2 <- coxph(Surv(enter,exit, death2) ~ as.factor(bmi_cat),
                            data=frmgham_recoded, method="efron")
summary(survmod.cox_unadj2)

######### Interval Censored Time to Event

require("splitstackshape")
data.wcgs <- read.dta("wcgs.dta")

# Create some new variables:
data.wcgs$id2 <- 1:dim(data.wcgs)[1]          # create a pseudo-id variable
data.wcgs$year <- data.wcgs$time169/365.25        # years of follow-up
data.wcgs$count <- ceiling((data.wcgs$year)/2)    # Number of intervals (of 2 years)
data.wcgs$inter <- data.wcgs$count*2              # Time of final visit
data.wcgs$bmi <- 703*data.wcgs$weight/(data.wcgs$height^2)

# Reshape dataset into multiple-record format
data.wcgs.long <- expandRows(data.wcgs, "count", drop=FALSE) # need "splitstackshape"

data.wcgs.long <- data.wcgs.long[order(data.wcgs.long$id),] # sort
data.wcgs.long$intnew <- round(10*(as.numeric(rownames(data.wcgs.long))-data.wcgs.long$id2))+1

data.wcgs.long$chd <- rep(0,length(data.wcgs.long$id2)) # Event indicator at each period
data.wcgs.long$chd[data.wcgs.long$intnew == data.wcgs.long$count] <-
     as.integer(data.wcgs.long$chd69[data.wcgs.long$intnew == data.wcgs.long$count]=="yes")

table(data.wcgs.long$chd)

# Categorize BMI variable
data.wcgs.long$bmi.cat <- cut(data.wcgs.long$bmi, c(0,18.5,25,30,1000), right=FALSE)

data.wcgs.long$bmi.cat <- relevel(as.factor(data.wcgs.long$bmi.cat),"[18.5,25)")

data.wcgs.long.nouw <- data.wcgs.long[data.wcgs.long$bmi.cat != "[0,18.5)",]

wcgs.cloglog.fit <- glm(chd ~ factor(intnew) + factor(bmi.cat) + age -1,
                        data=data.wcgs.long.nouw,  family=binomial(cloglog))
summary(wcgs.cloglog.fit)
hr.wcgs<-exp(cbind(coef(wcgs.cloglog.fit), confint(wcgs.cloglog.fit)))
colnames(hr.wcgs)<-c("HR","95% LL","95% UL")
hr.wcgs.expci <- round(hr.wcgs,digits=4) # Estimated HRs from interval-censored data
hr.wcgs.expci[6:7,]

