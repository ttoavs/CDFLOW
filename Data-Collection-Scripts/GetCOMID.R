library(tidyverse)
library(nhdplusTools)
library(progress)


# Load in data ---- 
sites <- readRDS("Data/Created/SiteLookup.RDS")

sites <- sites %>%
  mutate(HUC_10 = substr(HUC_12, start = 1, stop = 10)) %>%
  mutate(HUC_8 = substr(HUC_12, start = 1, stop = 8)) %>%
  mutate(HUC_6 = substr(HUC_12, start = 1, stop = 6)) %>%
  mutate(HUC_4 = substr(HUC_12, start = 1, stop = 4)) %>%
  mutate(HUC_2 = substr(HUC_12, start = 1, stop = 2))

HUC_vec <- unique(sites$HUC_2)

loc.df <- sites[!duplicated(sites$COMID),] #dataframe of unique comid locations 

pb <- progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",  #Add progress bar
                       total = length(HUC_vec),
                       complete = "=",   # Completion bar character
                       incomplete = "-", # Incomplete bar character
                       current = ">",    # Current bar character
                       clear = FALSE,    # If TRUE, clears the bar when finish
                       width = 100)      # Width of the progress bar

#This is a loop to pull NHD data based on each of the comid's in the loc.df 
NHD.df <- tibble()
for(i in HUC_vec){
  dat <- loc.df %>%
    filter(HUC_2==i)
  vec <- dat$COMID
  temp <- get_nhdplus(comid = vec)
  NHD.df <- rbind(NHD.df, temp)
  pb$tick() 
}

sites$COMID

NHD.df <- NHD.df %>% 
  rename(comid = COMID) #rename column

sites <- merge(sites, NHD.df, by.x = "COMID", by.y = "comid") #merge with CDFLOW

saveRDS(sites, "Data/Created/SiteLookup_v2.RDS")