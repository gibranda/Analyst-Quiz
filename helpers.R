load(file = "data/data_large.RData")

hist.dat <- data.frame(coordinate = c(mhall$trace[, 1], lat = mhall$trace[, 2]),
                       achse = rep(c("lon", "lat"), each=nrow(mhall$trace))
)

zlabels <- data.frame(lon = 13.4710043, lat = 52.5066796)