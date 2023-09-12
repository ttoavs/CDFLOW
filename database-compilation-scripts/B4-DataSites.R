library(tidyverse)
library(progress)
source("database-compilation-scripts/Functions/F-DatumFix.R")

# Create vector of site data files
data_vec <- list.files(path = "Data/Downloaded/Site", pattern = "*.csv")

# Progress bar object
pb <- progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
                       total = length(data_vec),
                       complete = "=",   # Completion bar character
                       incomplete = "-", # Incomplete bar character
                       current = ">",    # Current bar character
                       clear = FALSE,    # If TRUE, clears the bar when finish
                       width = 100)      # Width of the progress bar

# empty list to store data
site_df <- list()

# Load loop
for(i in 1:length(data_vec)){
  temp <- read_csv(paste0("Data/Downloaded/Site/", data_vec[[i]]), progress = FALSE, show_col_types = FALSE) 
  
  if(nrow(temp)>0){
    temp <- temp %>%
      select(MonitoringLocationIdentifier, LatitudeMeasure, LongitudeMeasure, HorizontalCoordinateReferenceSystemDatumName) %>%
      mutate(LatitudeMeasure = as.numeric(LatitudeMeasure),
             LongitudeMeasure = as.numeric(LongitudeMeasure))
    
    site_df[[i]] <- temp 
  }
  
  pb$tick()
}

# bind rows
site_df <- bind_rows(site_df)

# Only need uniques ids
site_df <- site_df[!duplicated(site_df$MonitoringLocationIdentifier),]

# custom function to put all lat/longs in same datum format
site_df <- DatumFix(site_df)

# drop column
site_df <- site_df %>%
  select(-(HorizontalCoordinateReferenceSystemDatumName))

# add cols to keep lat long
site_df <- site_df %>%
  mutate(MLI_Long = long, MLI_Lat = lat)

# read in estimated data
est_df <- read_csv("Data/Created/pCO2.csv")
# only need uniques locations
est_sites_df <- est_df[!duplicated(est_df$MonitoringLocationIdentifier),]
# Only relevant col for now
est_sites_df <- est_sites_df %>%
  select(MonitoringLocationIdentifier)
# Merge lat long data over
est_sites_df <- merge(est_sites_df, site_df, by = "MonitoringLocationIdentifier")

# Only need to run below if NHD is not on your local drive!

# download_nhdplusv2(
#  "Data/Downloaded/",
#  url = paste0("https://edap-ow-data-commons.s3.amazonaws.com/NHDPlusV21/",
#               "Data/NationalData/NHDPlusV21_NationalData_Seamless", "_Geodatabase_Lower48_07.7z"),
#  progress = TRUE
# )

# Create geometry object
est_sites_df <- est_sites_df %>%
  st_as_sf(coords = c("long", "lat"), crs = 4269)

# read in flowlines
Flowlines <- st_read("Data/Downloaded/NHDPlusNationalData/NHDPlusV21_National_Seamless_Flattened_Lower48.gdb", layer="NHDFlowline_Network")

# Look for nearest flowline
nearest = st_nearest_feature(est_sites_df, st_zm(Flowlines), check_crs = TRUE)
# calc distance
dist = st_distance(est_sites_df, st_zm(Flowlines[nearest,]), by_element=TRUE)
# join 
join = cbind(est_sites_df, Flowlines[nearest,])
# add dist
join$dist <- dist
# filter out those over 100m from nearest
join <- join %>%
  filter(as.numeric(dist)<=100)

remove(Flowlines)

# read in HUC12s
HUC12 <- read_sf("Data/Downloaded/NHDPlusNationalData/NHDPlusV21_National_Seamless_Flattened_Lower48.gdb", layer="HUC12")

# turn z axis off
sf_use_s2(FALSE)
# join based on which huc12 plane the data pt is in
join_huc12 <- st_join(est_sites_df, HUC12) 

remove(HUC12)

# Create site loop up for hucs
SiteLookup_huc <- st_drop_geometry(join_huc12) %>%
  select(MonitoringLocationIdentifier, HUC_12)
# Create site loop up for comids
SiteLookup_com <- st_drop_geometry(join) %>%
  select(MonitoringLocationIdentifier, COMID)
# Single site loop up
SiteLookup <- merge(st_drop_geometry(est_sites_df), SiteLookup_huc, by = "MonitoringLocationIdentifier")
SiteLookup <- merge(SiteLookup, SiteLookup_com, by = "MonitoringLocationIdentifier")
# drop NAs
SiteLookup <- SiteLookup %>%
  drop_na(COMID, HUC_12)

write.csv(SiteLookup, "Data/Created/Site-Lookup.csv" , row.names = FALSE)