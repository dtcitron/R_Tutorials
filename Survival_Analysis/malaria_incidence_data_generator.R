########################################
# The simulation output is for performing a demo of incidence-related Cox PH modeling
# Analyze here: /Users/dtcitron/Documents/R Tutorials/Survival Analysis/malaria_incidence_example.R
########################################

rm(list=ls());gc()
library(data.table)
library(ggplot2)
library(MASHmacro)
source("/Users/dtcitron/Documents/MASH/MACRO-multipatch-test/Multipatch_data_transform.R")
set.seed(0)


########################################
# Data from calibration
########################################
source("/Users/dtcitron/Documents/MASH/Bioko_Macro/Bioko_Island_Simulation_Setup/Bioko_Island_Simulation_Setup.R")


########################################
# List of Clusters used
########################################
cluster.ids <- sort(c(10, 17))
n.clusters <- length(cluster.ids)

########################################
# Define PfSI model parameters
########################################
PfSI.Setup(
  DurationPf = 200, mnChemoprophylaxisPf = 30, # setting duration of infectious period and Prophylaxis period
  FeverPf = fever.pf, TreatPf = treat.pf, # setting fever/treatment parameters
  peBlockPf = 0.9, mnPEPf = 365, vrPEPf = 1 # setting PE vaccination parameters
)
SimBitePfSI.Setup()


########################################
# Define Human parameters
########################################
# Vector of human populations in each patch
patch.human.populations <- cluster.human.pops[c(cluster.ids, c(42:48))]
MACRO.Human.Setup(pathogen = "PfSI",tripFrequency = travel.freq, tripDuration = 3) # travel.freq is travel frequency


########################################
# Define Patch parameters
########################################
# Number of patches
n = n.clusters + 7 # 10 areas, with 7 regions for travel

# Aquatic ecology parameters
patch.lambda <- c(area.lambda[cluster.ids], rep(0,7))
aquaPar = AquaPop_Emerge.Parameters(nPatch = n,lambda = patch.lambda, seasonality = FALSE)

# Create the movement matrix
# Needs to have zero diagonals, and all rows normalized
moveMat <- P.ij[c(cluster.ids, c(42:48)), c(cluster.ids, c(42:48))]
diag(moveMat)[1:n] <- 0
moveMat[1:n,] <- moveMat[1:n,]/travel.freq
# Patch Parameters
patchPar = lapply(X = 1:n,FUN = function(i){
  list(
    bWeightZoo = 0,
    bWeightZootox = 0,
    travelWeight = moveMat[i,],
    reservoir = FALSE,
    resEIR = NULL
  )
})
# Designate the last 7 patches as reservoirs
eir <- area.EIR[c(cluster.ids, c(42:48))]
for(i in (n.clusters+1):n){
  patchPar[[i]]$reservoir <- TRUE
  patchPar[[i]]$resEIR <- eir[i]
}

# PfPR
pfpr = x.pfpr.input[c(cluster.ids, c(42:48))]


########################################
# Define Mosquito parameters
########################################
# numbers of infectious mosquitoes
Z = eir/a*patch.human.populations
psi = diag(n)
mosquitoPar = list(model="RM", M=patch.lambda*p/(1-p),EIP = rep(11,365),
                   Y=Z/peip, Z=Z,
                   p=0.9, f=0.3, Q=0.9, v=20, psi = psi)


########################################
# Define Human Parameters
########################################
n_humans = sum(patch.human.populations)
patch_id = rep(x = 1:n,patch.human.populations)
home_id = rep(x = 1:n,patch.human.populations)
human_ages = unlist(lapply(X = patch.human.populations[1:n.clusters],FUN = siteAges_HumanPop))
# set biting weights
human_bWeight = rep(1, n_humans)
# Human Parameters
humanPar = lapply(X = 1:n_humans,function(i){
  list(
    houseID = home_id[i],
    patchID = patch_id[i],
    homeHouseID = home_id[i],
    homePatchID = patch_id[i],
    age = human_ages[i],
    bWeight = human_bWeight[i]
  )
})


########################################
# Set up schedule of vaccinations
########################################
humanIDs = as.character(1:n_humans)
vaxPar = vector(mode = "list",length = n_humans)
names(vaxPar) = humanIDs
# Give drug treatment to all people:
for (i in 1:n_humans){
  vaxPar[[i]] = list(tVax = c(1200), tTreat = c(90))
}
# Vaccinate all of the people in 4 of the clusters after 2 months, and also treat them
for (i in c(1:499) # patch 10
){
  vaxPar[[i]] = list(tVax = c(90), tTreat = c(90))
}





########################################
# Create a tile!
########################################
# Where the outputs go
directory = "/Users/dtcitron/Documents/R_Tutorials/Survival_Analysis/malaria_incidence_example_output"
tile = MacroTile$new(nPatch = n,AquaPar = aquaPar,PatchPar = patchPar,MosquitoPar = mosquitoPar,HumanPar = humanPar,directory = directory)


########################################
# Run a single simulation
########################################
set.seed(0)
t.max = 850
tile$simMacro(tMax = t.max, PfPAR = pfpr, PEVAXPAR = vaxPar)


########################################
# Create a plot of the single simulation
########################################
human.pathogen.path <- paste0(directory, "/HumanPathogen_Run0.csv")
human.move.path <- paste0(directory, "/HumanMove_Run0.csv")
h <- SIP.Conversion.Curves.PEvaxx(human.pathogen.path, human.move.path, patch.human.populations, t.max)
#t <- SIP.Conversion.Curves(human.pathogen.path, human.move.path, patch.human.populations, t.max)
#h <- SIP.FULL(t, n, t.max, status.list = c("S", "I", "P"))
# Add location names, to properly label the output
# Add pfpr values, to check on our calibration
location.names.table <- data.table(location = c(1:n),
                                   loc.name = c(as.character(cluster.ids), "Off", "Baney", "Luba", "Malabo", "Moka", "Riaba", "Ureka"),
                                   loc.pfpr = pfpr*patch.human.populations # expected number of infected humans
)
location.names.table$dummy <- as.factor("PfPR")
location.names.table$dummy2 <- as.factor("Vaccinated")
h <- merge(h, location.names.table, by = "location")

# Make a plot
plot <- ggplot(data = h[location %in% c(1:n.clusters)]) + # , n.clusters + 1, n.clusters + 4
  geom_hline(data=location.names.table[location %in% c(1:n.clusters)],
             aes(yintercept = loc.pfpr)) +
  geom_line(mapping = aes(x = time, y = N, color = status)) +
  geom_line(mapping = aes(x = time, y = N.PEvaxx, linetype = dummy2), color = "purple") +
  facet_wrap(~loc.name, ncol = 2) +
  scale_color_manual(name = "Status",
                     values = c("#ff0000", "#000cff", "#00ff1d"),
                     breaks = c("I", "S", "P"),
                     labels = c("Infected (PR)", "Susceptible", "Protected")) +
  scale_linetype_manual(name = " ", values = c(1)) +
  xlim(0,t.max) + ylim(0,500) +
  xlab("Time") + ylab("N") +
  ggtitle("90% effective vaccine, 100% vaccine coverage")
pdf("/Users/dtcitron/Documents/R_Tutorials/Survival_Analysis/malaria_incidence_example_output/Vaccinations_time_series.pdf", width = 6, height = 8)
plot
dev.off()
