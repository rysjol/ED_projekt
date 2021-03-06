library(dplyr)

circuits <- read.csv("circuits.csv")
constructorResults <- read.csv("constructorResults.csv")
constructors <- read.csv("constructors.csv")
constructorStandings <- read.csv("constructorStandings.csv")
drivers <- read.csv("drivers.csv")
driverStandings <- read.csv("driverStandings.csv")
lapTimes <- read.csv("lapTimes.csv")
pitStops <- read.csv("pitStops.csv")
qualifying <- read.csv("qualifying.csv")
races <- read.csv("races.csv")
results <- read.csv("results.csv")
seasons <- read.csv("seasons.csv")
status <- read.csv("status.csv")

races_2014 <- races %>% subset(year > 2013)
circuits_2014 <- circuits %>% subset(circuitId %in% races_2014$circuitId)
constructorResults_2014 <- constructorResults %>% subset(raceId %in% races_2014$raceId)
constructors_2014 <- constructors %>% subset(constructorId %in% constructorResults_2014$constructorId)
constructorStandings_2014 <- constructorStandings %>% subset(raceId %in% races_2014$raceId)
driverStandings_2014 <- driverStandings %>% subset(raceId %in% races_2014$raceId)
drivers_2014 <- drivers %>% subset(driverId %in% driverStandings_2014$driverId)
lapTimes_2014 <- lapTimes %>% subset(raceId %in% races_2014$raceId)
pitStops_2014 <- pitStops %>% subset(raceId %in% races_2014$raceId)
qualifying_2014 <- qualifying %>% subset(raceId %in% races_2014$raceId)
results_2014 <- results %>% subset(raceId %in% races_2014$raceId)
seasons_2014 <- seasons %>% subset(year > 2013)
status_2014 <- status %>% subset(statusId %in% results_2014$statusId)

write.csv(races_2014, "2014/races_2014.csv", row.names = FALSE)
write.csv(circuits_2014, "2014/circuits_2014.csv", row.names = FALSE)
write.csv(constructorResults_2014, "2014/constructorResults_2014.csv", row.names = FALSE)
write.csv(constructors_2014, "2014/constructors_2014.csv", row.names = FALSE)
write.csv(constructorStandings_2014, "2014/constructorStandings_2014.csv", row.names = FALSE)
write.csv(driverStandings_2014, "2014/driverStandings_2014.csv", row.names = FALSE)
write.csv(drivers_2014, "2014/drivers_2014.csv", row.names = FALSE)
write.csv(lapTimes_2014, "2014/lapTimes_2014.csv", row.names = FALSE)
write.csv(pitStops_2014, "2014/pitStops_2014.csv", row.names = FALSE)
write.csv(qualifying_2014, "2014/qualifying_2014.csv", row.names = FALSE)
write.csv(results_2014, "2014/results_2014.csv", row.names = FALSE)
write.csv(seasons_2014, "2014/seasons_2014.csv", row.names = FALSE)
write.csv(status_2014, "2014/status_2014.csv", row.names = FALSE)

