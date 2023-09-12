library(tidyverse)

estimates <- read_csv("Data/Created/pCO2.csv")
sites <- read_csv("Data/Created/Site-Lookup.csv")
obsv <- read_csv("Data/Created/Raw-Obsv.csv")


obsv <- obsv %>%
  select(obs_id, ActivityStartDate, ActivityStartTime.Time, `pH.std units`, `Temperature, water.deg C`, `Alkalinity.ueq/kgw`, State)

sites <- sites %>%
  select(-MLI_Long, -MLI_Lat)

CDFLOW <- merge(estimates, obsv, by = "obs_id")
CDFLOW <- merge(CDFLOW, sites, by = "MonitoringLocationIdentifier")

CDFLOW <- CDFLOW %>%
  select(-obs_id ,-MonitoringLocationIdentifier)

write.csv(CDFLOW, "CDFLOW.csv", row.names = FALSE)
