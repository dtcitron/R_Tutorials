library(deSolve)

# Demo
# https://cran.r-project.org/web/packages/deSolve/vignettes/deSolve.pdf
# Implementing deSolve for integrating the Lorenz equations
parameters <- c(a = -8/3, b = -10, c = 28)
# initial conditions
state <- c(X = 1 , Y = 1, Z = 1)
# Model equations are specified in a function that 
# calculates the rates of change of the state variables
# The function must be formatted to take variables in this order, 
# and the output variables must be ordered the same as in state
Lorenz <- function(t, state, parameters) {
  with(as.list(c(state,parameters)), {
    # rates of change
    dX <- a*X + Y*Z
    dY <- b * (Y - Z)
    dZ <- -X*Y + c*Y - Z
    # return vector of rates:
    list(c(dX, dY, dZ))
  })
}
# specify time steps
times <- seq(0,100,by=.01)

# Solve by implementing ODE
# Takes as input the initial conditions, the time steps, the function, and all parameters
out <- ode(y = state, times = times, func = Lorenz, parms = parameters)
head(out)

# and plot!
par(oma = c(0,0,3,0))
plot(out, xlab = "time", ylab = "-")
plot(out[, "X"], out[,"Z"], pch = ".")
mtext(outer = TRUE, side = 3, "Lorenz Model", cex = 1.5)

# Solving Initial Value Problems for ODEs: lots of different options
# Runge-Kutta 4:
out <- rk4(state, times, Lorenz, parameters)
print(system.time(out <- rk4(state, times, Lorenz, parameters)))
# LSODA
out <- lsoda(state, times, Lorenz, parameters)
print(system.time(out <- lsoda(state, times, Lorenz, parameters)))

# Diagnostics:
diagnostics(out)
