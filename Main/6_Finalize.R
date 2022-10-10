# Intro ----
# This script take our created Data files and finalizes the end product -- CDFLOW

# Load Libraries ----
library(tidyverse)

# Load Data ----
# bring in site lookup values
SiteLookup <- readRDS("Data/CreatedData/SiteLookup.RDS")
# bring in pCO2 estimates
Estimated <- readRDS("Data/CreatedData/Estimates.RDS")
# merge to define sites
CDFLOW <- merge(Estimated, SiteLookup, by = "MonitoringLocationIdentifier")
# get rid of unnecessary columns
CDFLOW <- CDFLOW %>%
  select(-MonitoringLocationIdentifier, -obs_id, -lat, -long)
# rename column
CDFLOW <- CDFLOW %>%
  rename(`CO2.mg/l` = pCO2.mg, Time = ActivityStartTime.Time, Temp.C = `Temperature, water.deg C`, pH.std_units = `pH.std units`, Date = ActivityStartDate) %>%
  select(COMID, Date, Time, HUC_12, State, Temp.C, pH.std_units, `Alkalinity.ueq/kgw`, pCO2.uatm, `CO2.mg/l`)

CDFLOW <- CDFLOW %>%
  filter(pH.std_units > 5.4, pCO2.uatm > 0)

CDFLOW <- CDFLOW %>%
  filter(pCO2.uatm < (mean(pCO2.uatm) + 2*(sd(CDFLOW$pCO2.uatm))))

# Save final product ----
write.csv(CDFLOW, "Final/CDFLOW.csv", row.names = F)
