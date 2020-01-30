nf = 20
nl = 20 

# create 32 feeding sites
# these are vectors with 32 elements, drawn from 3 different normal distributions
# "r" in front of a distribution name refers to random sampling from that distribution
# that is to say, rnorm(20,2,1) is drawing 20 times from N(2,1)
xf = c(rnorm(20, 2, 1), rnorm(10, 8, 1), rnorm(2, 7,.5)) 
yf = c(rnorm(20, 3, 1), rnorm(10, 10, 1), rnorm(2,7,.5)) 
# 30 elements, drawn from a gamma distribution
# Gamma distribution, characterized by shape and scale(=1/rate)
# maxEnt dist in the case where the mean is fixed >0, and the mean of the log is fixed
# in this case, wf sets the distribution of diameters
wf = rgamma(length(xf), 5, 5)
#wf[10]=10
#wf[25]=10

# setting graphical parameters, creating a 2x2 grid
par(mfrow = c(2,2), mar = c(0,0,0,0))
# plot feeding sites as red
plot(xf, yf, pch = 19, col = "red", ylim = c(-1,12), xlim = c(-1,12), xlab = "", xaxt = "n", ylab = "", yaxt = "n", cex = wf)

# creating 29 egg leaying sites
xl = c(rnorm(5, 2, .5), rnorm(14, 9, .5), rnorm(10, 8, .5)) 
yl = c(rnorm(5, 4, .5), rnorm(14, 4, .5), rnorm(10, 10, .5))
#xl = c(rnorm(1, 2, .5), rnorm(27, 9, .5), rnorm(1,  8, .5)) 
#xl = c( rnorm(29, 9, .5)) 
#yl = c(rnorm(29, 4, .5))

# K and wl are two different weighting parameters that define the diameters of the egg laying
# sites in the two upper plots - 
K = rgamma(length(xl), 1, 1)
wl = rgamma(length(xl), 5, 5)

# adds points to the previous plot
points(xl, yl, pch = 8, col = "blue", cex= wl)

# creates a second plot
plot(xl, yl, pch = 8, col = "blue", ylim = c(-1,12), xlim = c(-1,12), xlab = "", xaxt = "n", ylab = "", yaxt = "n", cex = K)

# kernel of connectivity
kerW = function(x, y, X, Y, w, p=1){
  # measures distance between two points (x,y) and (X,Y)
  d = sqrt( (x - X)^2 + (y - Y)^2) 
  # exponential decaying strength of the decay
  exp(-d*p)*w
}

# kernel of connectivity - 
# for a single point (x,y), return all connectivities between that point and the list of points (XX,YY)
kerW.i = function(i, x, y, XX, YY, w, p=1){
  kerW(x[i], y[i], XX, YY, w, p)
}

# these are the two non-symmetric block-diagonal parts of that connectivity matrix Dave talks about
# calculate the strength of interactions between l-sites and f-sites
LF = sapply(X=c(1:length(xl)), FUN=kerW.i, x=xl, y=yl, XX=xf, YY=yf, w=wf, simplify = "array")
FL = sapply(X=c(1:length(xf)), FUN=kerW.i, x=xf, y=yf, XX=xl, YY=yl, w=wl, simplify = "array")

LF = t(LF)
FL = t(FL)

# taking the dot product between the two block-diagonal elements
# yields interactions between l-sites and l-sites
Q = LF%*%FL

# row-normalizes Q
# for each l-site, what is the probability of hopping to another l-site
Q = Q / rowSums(Q)
# non-symmetric piece
N = matrix(pmax(0, Q - t(Q)), length(xl), length(xl)) 
# symmetric piece
C = Q-N

# Q.Q
Q2 = (Q %*% Q)
Q2 = Q2 / rowSums(Q2)

N2= matrix(pmax(0, Q2 - t(Q2)), length(xl), length(xl)) 
C2 = Q2-N2

# type n means "no plotting" - this plot will only have line segments showing weight between different sites
plot(xf, yf, type = "n", pch = 16, col = "blue", ylim = c(-1,12), xlim = c(-1,12), xlab = "", xaxt = "n", ylab = "", yaxt = "n")

# plot line segments
# line segment thickness/weighting is given by symmetric weights in Q.Q
for(i in 1:length(xl))
  for(j in i:length(xl))
    segments(xl[i], yl[i], xl[j], yl[j], lwd = C2[i,j]*4)

plot(xl, yl, type = "n", pch = 16, col = "blue", ylim = c(-1,12), xlim = c(-1,12), xlab = "", xaxt = "n", ylab = "", yaxt = "n")

# plot arrows
# arrow thickness is given by asymmetric weights in Q.Q
for(i in 1:length(xl))
  for(j in 1:length(xl))
    arrows(xl[i], yl[i], xl[j], yl[j], lwd = N2[i,j], length = .05)


# Visualizing C and N:
# heatmap(C, Rowv = NA, Colv = NA, scale = "none")
# heatmap(N, Rowv = NA, Colv = NA, scale = "none")