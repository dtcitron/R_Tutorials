library(data.table)
library(prodlim)
library(survival)
library(stats)
# Cf: https://staff.pubhealth.ku.dk/~tag/Teaching/share/R-tutorials/SurvivalAnalysis.html

# Start by generating some fake data, 
# using the Gillespie algorithm to generate survival times

# No vaccines, SI process

n = 400
x.curr = n
x = c(x.curr)
t.curr = 0
t = c(t.curr)
tmax = 100
bEIR = .02

while (t.curr < tmax & x.curr > 0){
  r1 <- runif(1)
  r2 <- runif(2)
  rate.SI <- bEIR*x.curr
  rate.tot <- rate.SI
  # udpate time step
  dt <- -log(r1)/rate.tot
  t.curr <- t.curr + dt
  t <- c(t, t.curr)
  # update number of susceptibles
  x.curr <- x.curr - 1
  x <- c(x, x.curr)
}

# We need to say which time each of the people were infected
# Luckily, we can say that 1 got infected at t = .472, 2 got infected at .84, etc...
# And then everyone else is "censored"
surv.data <-rbind(data.table(t = t[1:(length(x)-1)], x = 1:(length(x)-1), cens = 1, group = 1), 
                  data.table(t = tmax, x = (length(x):n), cens= 0, group = 1))

# Create an object that defines survival curves
km0 <- prodlim(Hist(t, cens) ~ group, data = surv.data)
plot(km0)

# And calculate an estimate of the hazard rate:
# Apparently if there are no covariates than I can't use cox regression
lm(log(x) ~ t)

# Vaccines in half of the population
n1 = 2000
n2 = 2000
n = n1 + n2
x1.curr = n1
x2.curr = n2
t.curr = 0
t = c(t.curr)
x1 = c(x1.curr)
x2 = c(x2.curr)
tmax = 500
bEIR1 = 0.001
bEIR2 = 0.003

while (t.curr < tmax & x1.curr + x2.curr > 0){
  r1 <- runif(1)
  r2 <- runif(1)
  rate.SI1 <- bEIR1*x1.curr
  rate.SI2 <- bEIR2*x2.curr
  rate.tot <- rate.SI1 + rate.SI2
  # udpate time step
  dt <- -log(r1)/rate.tot
  t.curr <- t.curr + dt
  t <- c(t, t.curr)
  # update number of susceptibles
  if (r2 < (rate.SI1/rate.tot)){
    x1.curr <- x1.curr - 1
    x1 <- c(x1, x1.curr)
    x2 <- c(x2, x2.curr)
  } else {
    x2.curr <- x2.curr - 1
    x1 <- c(x1, x1.curr)
    x2 <- c(x2, x2.curr)
  }
}

# Now we have to convert again - the time at which each person
# Be careful: there are now two groups of people and each person still needs their own id

t1 = which(x1[2:(length(x1))] - x1[1:(length(x1)-1)] != 0) + 1 
t2 = which(x2[2:(length(x2))] - x2[1:(length(x2)-1)] != 0) + 2
surv.data.2 <- rbind(data.table(t = t[t1], id = c(1:length(t1)), cens = 1, group = 1),
      data.table(t = tmax, id = c((length(t1)+1):n1), cens = 0, group = 1),
      data.table(t = t[t2], id = c((n1 + 1):(n1 + length(t2))), cens = 1, group = 2),
      data.table(t = tmax, id = c((n1 + length(t2) + 1):n), cens = 0, group = 2)
)

# Create an object that defines survival curves
km0 <- prodlim(Hist(t, cens) ~ 1, data = surv.data.2)
plot(km0)

# And calculate an estimate of the hazard rate:
x <- x1 + x2 # if we want to fit an exponential curve to all people
l <- lm(log(x) ~ t)
summary(l)

# Plot, showing best fit
plot(t, log(x))
abline(l)
# Really unclear to me why it is that the fit total hazard rate is so wrong here...

# Survival curves, partitioned by group:
km0 <- prodlim(Hist(t, cens) ~ group, data = surv.data.2)
plot(km0)

# Use Cox PH modeling to see how belonging to one group affects the hazard rate:
cox <- coxph(Surv(t,cens)~group,data=surv.data.2, x=TRUE)
summary(cox)
# We find, in this case, that being in group 2 yields a hazard rate that is close to 3x as big


# Completely effective vaccines, half the population
# Vaccines in half of the population
n1 = 2000
n2 = 2000
n = n1 + n2
x1.curr = n1
x2.curr = n2
t.curr = 0
t = c(t.curr)
x1 = c(x1.curr)
x2 = c(x2.curr)
tmax = 500
bEIR1 = 0
bEIR2 = 0.003

while (t.curr < tmax & x1.curr + x2.curr > 0){
  r1 <- runif(1)
  r2 <- runif(1)
  rate.SI1 <- bEIR1*x1.curr
  rate.SI2 <- bEIR2*x2.curr
  rate.tot <- rate.SI1 + rate.SI2
  # udpate time step
  dt <- -log(r1)/rate.tot
  t.curr <- t.curr + dt
  t <- c(t, t.curr)
  # update number of susceptibles
  if (r2 < (rate.SI1/rate.tot)){
    x1.curr <- x1.curr - 1
    x1 <- c(x1, x1.curr)
    x2 <- c(x2, x2.curr)
  } else {
    x2.curr <- x2.curr - 1
    x1 <- c(x1, x1.curr)
    x2 <- c(x2, x2.curr)
  }
}

# Now we have to convert again - the time at which each person
# Be careful: there are now two groups of people and each person still needs their own id

t1 = which(x1[2:(length(x1))] - x1[1:(length(x1)-1)] != 0) + 1 
t2 = which(x2[2:(length(x2))] - x2[1:(length(x2)-1)] != 0) + 2
surv.data.3 <- rbind(data.table(t = tmax, id = c((length(t1)+1):n1), cens = 0, group = 1),
                     data.table(t = t[t2], id = c((n1 + 1):(n1 + length(t2))), cens = 1, group = 2),
                     data.table(t = tmax, id = c((n1 + length(t2) + 1):n), cens = 0, group = 2)
)

# Create an object that defines survival curves
km0 <- prodlim(Hist(t, cens) ~ 1, data = surv.data.3)
plot(km0)

# And calculate an estimate of the hazard rate:
x <- x1 + x2 # if we want to fit an exponential curve to all people
l <- lm(log(x) ~ t)
summary(l)
# Plot, showing best fit
plot(t, log(x))
abline(l)

# Subtracting the offset
l <- lm(log(x-n/2) ~ t)
summary(l)
# Plot, showing best fit
plot(t, log(x-n/2))
abline(l)
# Really unclear to me why it is that the fit total hazard rate is so wrong here...

# Survival curves, partitioned by group:
km0 <- prodlim(Hist(t, cens) ~ group, data = surv.data.3)
plot(km0)

# Use Cox PH modeling to see how belonging to one group affects the hazard rate:
cox <- coxph(Surv(t,cens)~group,data=surv.data.3, x=TRUE)
summary(cox)
# We find in this case that because there are no removals in group 1 there is no 


