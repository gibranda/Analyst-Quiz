# 
# Stochastic Location Simulation Model
# -----------------------------------------------------
# Copyright: Paul Viefers <paulviefers@gmail.com>
# Date: 24 July 2015
# License: Apache License, Version 2.0
# 
# 
################################################################################
############# DESCRIPTION ######################################################
################################################################################
# 
# The basic idea of this program is to simulate the density of the possible location
# of an agent on a map. 
# 
# The information available about the location is given in form of three independent
# distribution of the map (or surface). The parameters of the distributions are given
# in the text to this quiz. 
# 
# The idea behind the solution is to simulate the _joint_ denisty of the location, given
# all three distributions. Since all given distributions are statistically independent,
# their joint distribution is given by their product. 
# Since the resulting distribution is analytically intractable, but easy to evaluate at a 
# given piont I use the Metropolis-Hastings algorithm to simulate it. 
# 
# The exact implementation is given below. Notably, I make use of two important packages.
# First, package 'geosphere' which allows me to make exact caluculations on the the manifold
# that is our globe. This avoids error prone transformations from polar to cartesian coordinates etc.
# Second, the package 'MHadaptive' provides an adaptive MH algorithm that tunes the sampler over
# iterations to make it more efficient.  

################################################################################
################ CODE ##########################################################
################################################################################

# Check for required packages and install
list.of.packages <- c("ggmap",
                      "coda",
                      "MASS",
                      "emdbook",
                      "ggplot2",
                      "MHadaptive",
                      "geosphere"
)

# Auto-install if not available
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(ggmap)
library(coda)
library(MASS)
library(emdbook)
library(MHadaptive)
library(geosphere)
theme_set(theme_bw(16))
set.seed(190684)

################################################################################
###############  FUNCTIONS AND DATA  ###########################################
################################################################################

##### FUNCTIONS 

# Functions computes the _joint_ likelihood of a coordinate based on the three
# independent criteria: (i) distance to Brandenburg Gate, (ii) distance to Spree,
# and distance to satellite path
# 
all_prob <- function(x){
    # Load packages
    require(geosphere) 
    
    # Make algorithm stay in box
    Lower <- c(13, 52.1) ; Upper <- c(13.8, 52.7)

    d.spree = dist2Line(x, line, distfun = distVincentySphere)
    log.prob.spree <- dnorm(d.spree[,1], mean = 0, sd = sdev.from.spree, log = TRUE)
    
    dist.bg <- distVincentySphere(bg, x)
    bg.log.prob <- dlnorm(dist.bg, meanlog = mu, sdlog= sqrt(sigma.2), log = TRUE)
    
    d.sat = dist2Line(x, sat.gc, distfun = distVincentySphere)
    log.prob.sat <- dnorm(d.sat[,1], mean = 0, sd = sdev.from.sat.line, log = TRUE)
    
    total.log.prob <- log.prob.spree + bg.log.prob + log.prob.sat

    return(total.log.prob)
}

# DATA
bg <- data.frame(lon = 13.377689, lat = 52.516288)
sat <- data.frame(lon = c(13.39915, 13.553989), lat = c(52.590117, 52.437385))
spree <- data.frame(lon = c(13.274099, 13.29234, 13.298541, 13.317349, 13.322434, 13.329, 13.332075, 13.340743, 13.356665, 13.372158, 13.379453, 13.392328, 13.399703, 13.406054, 13.416354, 13.435923, 13.461587, 13.483216, 13.491456, 13.503386),
                    lat = c(52.529198,52.531835,52.522116,52.520569,52.524877,52.522788,52.517056,52.522514,52.517239,52.523063,52.519198,52.522462,52.520921,52.515333,52.514863,52.506034,52.496473,52.487641,52.488739,52.464011)
)

# Parameters for deviation from Spree
spree_splines <- data.frame(startLon = spree$lon[1:(nrow(spree)-1)],
                            startLat = spree$lat[1:(nrow(spree)-1)],
                            endLon = spree$lon[2:(nrow(spree))], 
                            endLat = spree$lat[2:(nrow(spree))],
                            leg = seq(1,19)
)
sdev.from.spree <- 2730/1.96

# Parameters for deviation from satellite path
# Draw the grand circle line connecting the two satellite positions given
sat_spline <- data.frame(gcIntermediate(sat[1,], sat[2,]))
sat.gc <- greatCircle(sat[1,], sat[2,])
sdev.from.sat.line <- 2400/1.96

# Parameters of the radial profile distribution
sigma.2 <- (log(4700/3877))/1.5
mu <- log(3877) + sigma.2

# Radius of the earth in meters
# Please note: I do NOT use this here. See below for an explanation.
earth_radius <- 6371*1000

# I use the R package geosphere here to make use of off-the-shelve functions
# Note: for geosphere all coordinates have to be in degrees and in the order (lon, lat).
# 
# The package uses r = 6378137m as the earth radius. I do NOT change this to the given
# r' = 6371000m, because results are more exact. E.g. when I compute the coordinates
# of the point that is reached from the first satellite coordinate using the package
# default, I end up exactly at the point given in the instructions. 
# See here:
#       destPoint(sat[1,], b= bearing(sat[1,], sat[2,]), d=distVincentySphere(sat[1,], sat[2,]))

# Distance in meters between the two satellite coordinates
dist.sat <- distVincentySphere(sat[1,], sat[2,])

# The line segment giving the Spree
line <- spree

################################################################################
################### SIMULATE JOINT DENSITY OF COORDINATES ######################
################################################################################################################################################################

# Use Metropolis-Hastings algorithm to sample from unknown target distribution
mhall <- Metro_Hastings(all_prob, 
                        pars = as.matrix(spree[14, ]), 
                        prop_sigma = diag(apply(spree, 2, var)),
                        par_names = c("lon", "lat"),
                        iterations = 10000,
                        burn_in = 5000
)

# Collect data for plot
scatter_all <- data.frame(lon = mhall$trace[seq(1,nrow(mhall$trace),10), 1], 
                          lat = mhall$trace[seq(1,nrow(mhall$trace),10), 2],
                          iteration = seq_along(mhall$trace[mhall$trace[seq(1,nrow(mhall$trace),10), 1]]))


# Take the 'posterior mean' as the best guess on the map
guess <- apply(mhall$trace[seq(1,nrow(mhall$trace),10), ], 2, mean)
guess <- data.frame(lon = guess[1], lat = guess[2])

# You may also try some hill-climbing using optim to see where this leads
# but the solution vrtually coincides with the mean location from simulation.
# 
# If you wish to use numerical maximization, please also make sure you restrict
# the parameter values for function 'all_prob' so they do not, e.g. exceed 180°.
# 
# mode <- optim(spree[14, ],
#               fn=all_prob, method="BFGS", 
#               hessian = TRUE,
#               control=list(fnscale=-1))
# sp <- mode$par
# 
# # Consider a second attempt using Nelder-Mead for robustness
# mode <- optim(sp, 
#               fn=all_prob, method="Nelder-Mead", 
#               hessian = TRUE,
#               control=list(fnscale=-1))
# peak <- data.frame(lon = mode$par[1], lat = mode$par[2])

# Use ggmap to plot results on map of Berlin
#Berlin <- get_stamenmap(bbox = c(left = 13.36, bottom = 52.45, right = 13.52, top = 52.55), maptype = "toner", zoom = 13)
Berlin <-  get_googlemap(center = c(lon = 13.45705, lat=52.51122), 
                         zoom = 13, maptype = 'roadmap', markers = bg)
BerlinMap <- ggmap(Berlin)

# Get highest posterior regions to indicate on the map
ContourLines <- as.data.frame(HPDregionplot(mcmc(data.matrix(mhall$trace), thin = 10), prob=0.3))

BerlinMap + 
    geom_point(aes(x = bg$lon, y = bg$lat), size = 1, color = "black") + 
    geom_point(aes(x = lon, y = lat), data = sat_spline, size = 3, color = "black") +
    geom_leg(aes(x = startLon, 
                 y = startLat, 
                 xend = endLon, 
                 yend = endLat), 
             data = spree_splines[12:nrow(spree_splines), ], size = 1.5, color = "blue") +
    geom_polygon(data = ContourLines, aes(x = x, y = y), color = "red", size = 2, fill = "red", alpha = 0.2) +
    stat_density2d(
        aes(x = lon, y = lat, fill = ..level.., alpha = ..level..),
        size = 1, bins = 9, data = scatter_all,
        geom = "density2d", color = "black"
    ) +
    geom_point(aes(x = guess$lon, y = guess$lat), size = 4, color = "red")
