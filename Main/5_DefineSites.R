# Intro ----
# This scripts queries the raw data needed to estimate pCO2 from the Water Quantity Portal.

# Load libraries ---- 
library(nhdplusTools)
library(sf)
library(progress)
library(tidyverse)

# Downlad NHD ----
# First you need you download the NHD
download_nhdplusv2(
  "Data/RawData/",
  url = paste0("https://edap-ow-data-commons.s3.amazonaws.com/NHDPlusV21/",
               "Data/NationalData/NHDPlusV21_NationalData_Seamless", "_Geodatabase_Lower48_07.7z"),
  progress = TRUE
)

# Set up ----
#Read in the data frame of observations with CO2 estimates
Estimates <- readRDS("Data/CreatedData/Estimates.RDS")
#copy DF
df <- Estimates
#Create SF objects from lat/long
df <- df %>%
  st_as_sf(coords = c("long", "lat"), crs = 4269)
#Only need unique monitoring identifiers
df <- df[!duplicated(df$MonitoringLocationIdentifier),] %>%
  select(MonitoringLocationIdentifier)
#Read in NHD flowlines
Flowlines <- read_sf("Data/RawData/NHDPlusNationalData/NHDPlusV21_National_Seamless_Flattened_Lower48.gdb", layer="NHDFlowline_Network")
#Read in NHD HUC12's
HUC12 <- read_sf("Data/RawData/NHDPlusNationalData/NHDPlusV21_National_Seamless_Flattened_Lower48.gdb", layer="HUC12")

# Site Join COMID ----
#Find nearest comid to monitoring location identifiers for pCO2 estimates
nearest = st_nearest_feature(df, st_zm(Flowlines), check_crs = TRUE)
#Calc distance
dist = st_distance(df, st_zm(Flowlines[nearest,]), by_element=TRUE)
#Join by nearest feature
join = cbind(df, Flowlines[nearest,])
#Add distance column
join$dist <- dist
#limit to joins within 100 meters
join <- join %>%
  filter(as.numeric(dist)<=100)


# Save progress ----
saveRDS(join, "Data/CreatedData/join.RDS")

# Site Join HUC12 ----
#Allows for geometry type
sf_use_s2(FALSE)
#J oin monitoring locations with HUC12's
join_huc12 <- st_join(df, HUC12) 
# select values needed
SiteLookup_huc <- st_drop_geometry(join_huc12) %>%
  select(MonitoringLocationIdentifier, HUC_12)
# select values needed
SiteLookup_com <- st_drop_geometry(join) %>%
  select(MonitoringLocationIdentifier, COMID)
# merge to create lookup data frame
SiteLookup <- merge(st_drop_geometry(df), SiteLookup_huc, by = "MonitoringLocationIdentifier")
SiteLookup <- merge(SiteLookup, SiteLookup_com, by = "MonitoringLocationIdentifier")
# drop na's
SiteLookup <- SiteLookup %>%
  drop_na(COMID, HUC_12)

# Save Data ----
saveRDS(SiteLookup, "Data/CreatedData/SiteLookup.RDS")
