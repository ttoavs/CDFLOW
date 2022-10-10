# Intro ----
#This script takes are raw data and processes it so we can estimate pCO2

# Load libraries ----
library(tidyverse)
library(data.table)
library(janitor)
library(progress)

# Load functions ----
source("Main/Functions/UnitFix.R")
source("Main/Functions/CreateObservation.R")
source("Main/Functions/DuplicateValues.R")
source("Main/Functions/NumericAndSurface.R")
source("Main/Functions/DatumFix.R")

# Set up ----
# Set seed for random operations
set.seed(1) 
# vector created from queried data files
chem_vec <- list.files(path = "Data/RawData/Chemical", pattern = "*.RDS")
site_vec <-  list.files(path = "Data/RawData/Site", pattern = "*.RDS")
partialSite_vec <- list.files(path = "Data/RawData/Partial Site", pattern = "*.RDS")
# Create empty data frame that will be filled with data
RawObvs <- tibble()
SiteData <- tibble() 

# Loop for site data ----
# Loop that goes through each state data file that was queried and pulls out the lat/long then adds to one of the empty dataframes
for(i in site_vec) {
  df_site <- readRDS(paste0("Data/RawData/Site/", i))
  
  df_site <- df_site %>%
    select(MonitoringLocationIdentifier, LatitudeMeasure, LongitudeMeasure, HorizontalCoordinateReferenceSystemDatumName)
  
  SiteData <- bind_rows(SiteData, df_site)
}
# Loop for partial data files
# This loop goes through data that need to be queried using the script "DataQueryIssue.R".
for(i in partialSite_vec) {
  df_site <- readRDS(paste0("Data/RawData/Partial Site/", i))
  
  df_site <- df_site %>%
    select(MonitoringLocationIdentifier, LatitudeMeasure, LongitudeMeasure, HorizontalCoordinateReferenceSystemDatumName)
  
  SiteData <- bind_rows(SiteData, df_site)
}
# Fix datums
SiteData <- DatumFix(SiteData)

# Loop for chemical data ---- 
# Progress bar
pb <- progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
                       total = length(chem_vec),
                       complete = "=",   # Completion bar character
                       incomplete = "-", # Incomplete bar character
                       current = ">",    # Current bar character
                       clear = FALSE,    # If TRUE, clears the bar when finish
                       width = 100)      # Width of the progress bar
#Loop that goes through each of the chem data files queried and processes it and creates obersevations that will be used to estimate pCO2 later.
for(i in chem_vec) {
  
  df_chem <- readRDS(paste0("Data/RawData/Chemical/", i)) 
  
  #Only numeric result values, drop non-surface level measurements and Na's
  df_chem <- NumericAndSurface(df_chem)
  
  #Unit fix using function in UnitFix.R
  df_chem <- unit_fix(df_chem)
  
  #Duplicate values
  df_chem <- ridDuplicats(df_chem)
  
  #Create observations
  df_chem <- createOBSV(df_chem)
  
  df_chem$State<-substr(i, start = 1, stop = 2)
  
  RawObvs <- bind_rows(RawObvs, df_chem)
  
  pb$tick() #tick over for progress bar, rinse and repeat!!
}

# Merge Data ----
# Merge site data and chemical data
DataObsv <- merge(RawObvs, SiteData, by = "MonitoringLocationIdentifier")
#Get rid on unneeded column
DataObsv <- DataObsv %>%
  select(-HorizontalCoordinateReferenceSystemDatumName)

#Save progress so we don't have to preform this step again
saveRDS(DataObsv, "Data/CreatedData/DataObsv.RDS")
