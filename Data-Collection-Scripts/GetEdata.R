library(tidyverse)
library(nhdplusTools)
library(progress)


# Load in data ---- 

estimates <- read_csv("Data/Created/estimated-pCO2.csv")
sites <- read_csv("Data/Created/Site-Lookup.csv")

df <- merge(estimates, sites, by = "MonitoringLocationIdentifier")

df <- df %>%
  mutate(HUC_10 = substr(HUC_12, start = 1, stop = 10)) %>%
  mutate(HUC_8 = substr(HUC_12, start = 1, stop = 8)) %>%
  mutate(HUC_6 = substr(HUC_12, start = 1, stop = 6)) %>%
  mutate(HUC_4 = substr(HUC_12, start = 1, stop = 4)) %>%
  mutate(HUC_2 = substr(HUC_12, start = 1, stop = 2))

# NHD Data ----

#This section of code adds NHD data to CDFLOW

HUC_vec <- unique(df$HUC_2) #Vector of HUC2's to loop over

loc.df <- df[!duplicated(df$COMID),] #dataframe of unique comid locations 

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


NHD.df <- NHD.df %>% 
  rename(COMID = comid) #rename column

dat_final <- merge(df, NHD.df, by = "COMID") #merge with CDFLOW


# NLDI Data ----

dat <- loc.df %>%
  dplyr::select(COMID)

nldi.df <- list()

pb <- progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
                       total = nrow(dat),
                       complete = "=",   # Completion bar character
                       incomplete = "-", # Incomplete bar character
                       current = ">",    # Current bar character
                       clear = FALSE,    # If TRUE, clears the bar when finish
                       width = 100)      # Width of the progress bar

for(i in 1:nrow(dat)){
  
  extracted <- FALSE
  attempts <- 0

  while(extracted==FALSE & attempts < 50) {
    tryCatch(
      values <- get_nldi_characteristics(list(featureSource = "comid", featureID = dat$COMID[i]), type = "total"),
      extracted <<- TRUE,
      error = function(e) { 
        extracted <<- FALSE
      }
    )
    if(!extracted) {
      attempts <- attempts + 1
    }
  }
  
  if(extracted == TRUE){
    temp.df <- data.frame(rbind(
      values$total$characteristic_id,
      values$total$characteristic_value, 
      values$total$percent_nodata
    ))
    
    colnames(temp.df) <- temp.df[1,]
    temp.df <- temp.df[-1, ]
    
    if(nrow(temp.df>0)){
      temp.df$COMID <- dat$COMID[i]
      nldi.df[[i]] <- temp.df
    }
  }
  pb$tick() 
}

test <- bind_rows(nldi.df)

saveRDS(nldi.df, "Data/Created/nldi_tot_df.RDS")

temp <- test[!duplicated(test$COMID),]

dat_final2 <- merge(dat_final, temp, by = "COMID")

saveRDS(dat_final2, "Data/Created/CDFLOW_EnvirData.RDS")

saveRDS(temp, "Data/Created/NLDI.RDS")
saveRDS(NHD.df, "Data/Created/NHD.RDS")