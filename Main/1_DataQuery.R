# Intro ----
# This scripts queries the raw data needed to estimate pCO2 from the Water Quantity Portal.

# Load libraries ---- 
library(dataRetrieval)
library(tidyverse)
library(progress)

# Set up ----

# Vector with state abbreviations
state_vec <- state.abb[!state.abb %in% c("HI","AK")]
# Add DC
state_vec[49] <- "DC"
# Create arguments to query the WQP, see script "ExtraParameters.R" in supplementary if you wish to add parameters
args <- list(statecode = "", 
             siteType="Stream",
             sampleMedia="Water",
             startDateLo="1990-01-01", 
             startDateHi="2020-12-31",
             characteristicName = "pH",
             characteristicName = "Temperature%2C%20water",
             characteristicName = "Alkalinity",
             characteristicName = "Alkalinity%2C%20total"
)


# Query Chemical Data ----

# Create progress bar
pb <- progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
                       total = length(state_vec),
                       complete = "=",   
                       incomplete = "-", 
                       current = ">",    
                       clear = FALSE,   
                       width = 100)

# Main loop to query chem data. Notice trycatch(), sometimes you will run into server errors and will need to try again, and the trycatch() makes it try as many times as you like. 
# Each iteration of the loop will create an r object in the raw data folder that can be loaded later. The iterations are seperated by state 
# NOTE if a particular state is not working you may need to break it up into smaller year chunks to get the data, see script "DataQueryIssue.R" if you need help with this.
for (i in state_vec){
  
  args$statecode <- i
  attempts <- 1
  extracted <- FALSE

  while(extracted==FALSE & attempts <= 20) {
    
    tryCatch(
      saveRDS(readWQPdata(args), paste0("RawData/Chemical/",i,".RDS")),
      extracted <<- TRUE,
      error = function(e) { 
        extracted <<- FALSE
      }
    )
    
    if(!extracted) {attempts <- attempts + 1}
    if(!extracted & attempts > 20) { next }
  }
  
  assign(paste0(i,"_attempts"), attempts)
  pb$tick() 
}


# Query Sites Data ---- 

# Create Progress bar
pb <- progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
                       total = length(state_vec),
                       complete = "=",   # Completion bar character
                       incomplete = "-", # Incomplete bar character
                       current = ">",    # Current bar character
                       clear = FALSE,    # If TRUE, clears the bar when finish
                       width = 100)      # Width of the progress bar

# Same idea as the chemical loop but this is querying site data
for (i in state_vec){
  
  args$statecode <- i
  attempts <- 1
  extracted <- FALSE
  
  while(extracted==FALSE & attempts <= 100) {
    
    tryCatch(
      saveRDS(whatWQPsites(args), paste0("RawData/Site/",i,".RDS")),
      extracted <<- TRUE,
      error = function(e) { 
        extracted <<- FALSE
      }
    )
    
    if(!extracted) {attempts <- attempts + 1}
    if(!extracted & attempts > 100) { next }
  }
  
  assign(paste0(i,"_attempts"), attempts)
  pb$tick() 
}
