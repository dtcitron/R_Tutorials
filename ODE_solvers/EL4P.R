# EL4P model of mosquito population dynamics
library(deSolve)

# Larva-Mature mosquito model
# Taken from "Mosquito Population Regulation and Larval Source management..."
# Single larval stage
# Single site
parameters.simple <- c(g = 1/10., # adult deathrate
                       fnup = 40, # per adult generation of eggs
                       alpha = 5/10., # maturation rate
                       gamma = 1/10., # larva death rate
                       psi = 500 # per larva density-dependent death rate
                       )
#
state.simple <- c(M = .5,
                  L = 0
                  )
#
el4p.simple <- function(t, state, parameters){
  with(as.list(c(state,parameters)), {
    dM <- alpha*L - g*M
    dL <- fnup*M - (alpha + gamma + psi*L)*L
    # return
    return(list(c(dM,dL)))
  })
}
# time steps
times <- seq(0,100,by=.01)

# return M*, L*
with(as.list(parameters.simple), {
  Lstar <- (alpha*fnup/g-alpha-gamma)/psi
  Mstar <- Lstar*alpha/g
  print(c(Mstar, Lstar))
}
)
# Return eigenvalues
with(as.list(parameters.simple), {
  x <- g - alpha - gamma + 2*alpha/gamma*fnup
  y <- g*(alpha + gamma) - alpha*fnup
  e1 <- -.5*x*(1 + sqrt(1 + 4 * y/x/x))
  e2 <- -.5*x*(1 - sqrt(1 + 4 * y/x/x))
  return(c(e1, e2))
}
)
# Lside > Rside for stability above 0, and oscillations if third term < 0
with(as.list(parameters.simple), {
  lside <- fnup/g
  rside <- 1 + gamma/alpha
  osc <- 1 + 4*(g*(alpha + gamma) - alpha*fnup)/(g + 2*alpha/g*fnup-alpha - gamma)/(g + 2*alpha/g*fnup-alpha - gamma)
  print(c(lside, rside, osc))
}
)

# integrate!
out <- ode(y = state.simple, times = times, func = el4p.simple, parms = parameters.simple)
head(out)
tail(out)

plot(out[,"time"],out[,"M"], col = 'blue', ylim = c(0,1))
points(out[,"time"],out[,"L"], col = 'red')

# Implement multi-stage model for a single site, ignoring migration across sites for now
# Define parameters
# Refer to "Adult vector control..." article by Smith et al. for definitions of most of these parameters
parameters.single <- c(g = .1, # adult death rate
                # nu*f represents the per adult mosquito rate at which eggs are produced
                f = .5, # blood feeding rate
                nu = 20, # egg batch size
                mu = .5, # maturation rate, 1/mu is 8 to 12 days
                alpha = .1, # larval death rate, should be close to mu
                psi = .1 # population-density-dependent mortality rate
)
# Define state variables and initial conditions
state.single <- c(M = 1, # adult mosquitoes
           L1 = .1, # 1st stage larval mosquitoes
           L2 = 0, # ... nth stage larval mosquitoes
           L3 = 0,
           L4 = 0,
           P = .1 # pupa populations
)
# Define the equations
el4p.single <- function(t, state, parameters){
  with(as.list(c(state,parameters)), {
    # rates of change
    dM <-  mu*P - g*M
    dL1 <- f*nu*M - (mu + alpha + psi*(L1 + L2 + L3 + L4))*L1
    dL2 <- mu*L1 - (mu + alpha + psi*(L1 + L2 + L3 + L4))*L2
    dL3 <- mu*L2 - (mu + alpha + psi*(L1 + L2 + L3 + L4))*L3
    dL4 <- mu*L3 - (mu + alpha + psi*(L1 + L2 + L3 + L4))*L4
    dP <-  mu*L4 - mu*P
    # return vector of rates:
    list(c(dM, dL1, dL2, dL3, dL4, dP))
  })
}
# time steps
times <- seq(0,200,by=1)

# integrate!
out <- ode(y = state.single, times = times, func = el4p.single, parms = parameters.single)
head(out)
tail(out)

# plot!
dev.off()
plot(out[,"time"], out[,"M"], xlab = "time", ylab = "-", ylim = c(0,1), col = 'red') 
points(out[,"time"], out[,"P"], col = 'blue')

# Vectorized version, incorporating in different sites
# Define the equations
# Outputs a list-conversion of the 6*N state variables, stacked in a single vector
el4p <- function(t, state, parameters){
  with(
    as.list(
      c(
        # extract parameters (from list)
        N <- parameters[[1]],
        delta <- parameters[[2]],
        g <- parameters[[3]],
        f <- parameters[[4]],
        nu <- parameters[[5]],
        mu <- parameters[[6]],
        alpha <- parameters[[7]],
        psi <- parameters[[8]],
        Psi <- array(parameters[[9]], dim <- c(N,N)),
        # extract states (from numeric)
        M <- state[1:N],
        L1 <- state[(N+1):(2*N)],
        L2 <- state[(2*N+1):(3*N)],
        L3 <- state[(3*N+1):(4*N)],
        L4 <- state[(4*N+1):(5*N)],
        P <- state[(5*N+1):(6*N)]
      )
    ), 
    { 
      dM <- -1.*delta*M + delta*as.numeric(Psi %*% M) - g*M + mu*P
      dL1 <- nu*f*M - (mu + alpha + psi*(L1+L2+L3+L4))*L1
      dL2 <- mu*L1 - (mu + alpha + psi*(L1+L2+L3+L4))*L2
      dL3 <- mu*L2 - (mu + alpha + psi*(L1+L2+L3+L4))*L3
      dL4 <- mu*L3 - (mu + alpha + psi*(L1+L2+L3+L4))*L4
      dP <- mu*L4 - mu*P
      # return vector of derivatives:
      list(c(dM, dL1, dL2, dL3, dL4, dP))
    }
  )
}
# Define parameters
# A list, such that each element of the list represents a different parameter, even parameters that are arrays or vectors
# Refer to "Adult vector control..." article by Smith et al. for definitions of most of these parameters
parameters <- list(2, # N: number of sites
                   1., # delta: migration rate
                   .1, # g: adult death rate
                   50, # f: blood feeding rate
                   1, # nu: egg batch size; nu*f represents the per adult mosquito rate at which eggs are produced
                   c(1/8.,1/12.), # mu: maturation rate, is vectorized across sites
                   c(.1,.1), # alpha: larval death rate, is vectorized across sites
                   c(.001,.0005), # psi: population-density-dependent mortality rate, vectorized across sites
                   c(.9,.1,.1,.9) # Psi: immigration matrix, 
                   #Psi = matrix(c(0,1,1,0), nrow = 2, ncol = 2)
                   # we need the rows to sum to 1, so that the migration does not become a death term
                   # remember: when reshaping, this collection is read in by column, not by row
)

# Define state variables and initial conditions
# A numeric consisting of the 6*N state variables, stacked in a single vector
state <- c(c(100,100), # M: adult mosquitoes, vectorized across sites
           c(10,10), # L1: 1st stage larval mosquitoes, vectorized across sites
           c(10,10), # L2: 2nd stage larval mosquitoes, vectorized across sites
           c(10,10), # L3: 2nd stage larval mosquitoes, vectorized across sites
           c(10,10), # L4: 2nd stage larval mosquitoes, vectorized across sites
           c(10,10) # P: pupa populations, vectorized across sites
)

# time steps
times <- seq(0,100,by=.1)

# Solve by implementing ODE
# Takes as input the initial conditions, the time steps, the function, and all parameters
out <- ode(y = state, times = times, func = el4p, parms = parameters)

colnames(out) <- c("t", "M1","M2","L1,1","L1,2","L2,1","L2,2","L3,1","L3,2","L4,1","L4,2","P1","P2")

tail(out,1)

# Plot!
plot(out[,"t"], out[,"M1"], xlab = "time", ylab = "-", ylim=c(0,200), xlim = c(0,100), col = 'red') 
points(out[,"t"], out[,"M2"], xlab = "time", ylab = "-", col = 'blue') 
points(out[,"t"], out[,"M1"]+out[,"M2"], xlab = "time", ylab = "-", col = 'black')


dev.off()

plot(out[,"t"], out[,"L1,1"], xlab = "time", ylab = "-", ylim=c(0,250), xlim = c(0,10), col = 'red') 
points(out[,"t"], out[,"L2,1"], xlab = "time", ylab = "-", col = 'blue')
points(out[,"t"], out[,"L3,1"], xlab = "time", ylab = "-", col = 'orange')
points(out[,"t"], out[,"L4,1"], xlab = "time", ylab = "-", col = 'green') 
points(out[,"t"], out[,"L1,1"]+out[,"L2,1"]+out[,"L3,1"]+out[,"L4,1"], 
       xlab = "time", ylab = "-", col = 'black')


plot(out[,"t"], out[,"L1,2"], xlab = "time", ylab = "-", ylim=c(0,350), xlim = c(0,10), col = 'red') 
points(out[,"t"], out[,"L2,2"], xlab = "time", ylab = "-", col = 'blue')
points(out[,"t"], out[,"L3,2"], xlab = "time", ylab = "-", col = 'orange')
points(out[,"t"], out[,"L4,2"], xlab = "time", ylab = "-", col = 'green') 
points(out[,"t"], out[,"L1,2"]+out[,"L2,2"]+out[,"L3,2"]+out[,"L4,2"], 
       xlab = "time", ylab = "-", col = 'black')
