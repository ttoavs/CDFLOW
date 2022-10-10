# Function to get rid of duplicate values, it will choose one at random to keep.

library(janitor)
library(tidyverse)

ridDuplicats <- function(dat) {
  df <- dat
  df$dupe_id <- paste0(df$MonitoringLocationIdentifier, "_", df$CharacteristicName, "_",  df$ActivityStartDate, "_", df$`ActivityStartTime.Time`) 
  
  df <- df %>%
    group_by(dupe_id) %>%
    slice_sample() %>%
    ungroup()
  
  return(df)
}